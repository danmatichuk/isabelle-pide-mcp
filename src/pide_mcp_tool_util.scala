/*  Title:      PIDE_MCP/pide_mcp_tool_util.scala
    Author:     Kevin Kappelmann

Utilities for PIDE MCP Tools.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Tool_Util {
  val retry_soon_message: String = "Please retry soon."
  val default_start_line: Int = 1
  val all_tools_option: String = "*"

  def make_tool_table(tool_names: String = all_tools_option): Map[String, PIDE_MCP_Tool] = {
    val tools =
      Isabelle_System.make_services(classOf[PIDE_MCP_Tools]).flatMap(_.entries).
        foldLeft(Map.empty[String, PIDE_MCP_Tool]) {
          case (tools, tool) =>
            if (tools.isDefinedAt(tool.name))
              error(s"Duplicate PIDE_MCP tool: ${Library.quote(tool.name)}")
            else tools + (tool.name -> tool)
        }
    if (tool_names == all_tools_option) tools
    else {
      val names = space_explode(',', tool_names)
      val unknown = names.filterNot(tools.isDefinedAt)
      if (unknown.nonEmpty)
        error(s"Unknown PIDE_MCP tool(s): ${commas_quote(unknown)}. " +
          s"Available tool(s): ${commas_quote(tools.keys.toList.sorted)}")
      tools.filter { case (name, _) => names.contains(name) }
    }
  }

  def running_session_param(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T
  ): PIDE_MCP_Session = {
    def available = s"Available session(s): ${JSON.Format(sessions.state_json())}"
    val session_ids = PIDE_MCP_Tool_Schema.running_session_arg.get(args).map(List(_))
    Result.release(sessions.running_sessions(session_ids)) match {
      case List(session) => session
      case Nil => error(s"No running session. $available")
      case _ => error(s"Multiple running sessions: specify ${quote("session")}. $available")
    }
  }

  def running_sessions_param(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T
  ): List[PIDE_MCP_Session] =
    Result.release(
      sessions.running_sessions(PIDE_MCP_Tool_Schema.running_sessions_arg.get(args)))

  def origin_param(session: PIDE_MCP_Session, args: JSON.Object.T): Document.Node.Name =
    session.node_name(PIDE_MCP_Tool_Schema.origin_arg.get(args))

  // with wait_seconds > 0, wait that long for a queued or unprocessed origin before failing
  def require_loaded_origin_snapshot(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    progress: Progress,
    wait_seconds: Int = 0
  ): Document.Snapshot = {
    val deadline = Time.now() + Time.seconds(wait_seconds)
    var queued = false
    def attempt(): Either[String, Document.Snapshot] =
      Exn.result { session.node_snapshot(node_name) } match {
        case Exn.Res(snapshot) => Right(snapshot)
        case Exn.Exn(_) =>
          if (PIDE_MCP_Util.is_loaded_dynamic(session.tip_version(progress).nodes(node_name)))
            Left(s"The origin ${quote(session.origin(node_name))} is loaded but has not been " +
              "processed yet. ")
          else if (queued)
            Left(s"The origin ${quote(session.origin(node_name))} is queued for loading. ")
          else {
            session.read_update_resolve(node_name, List((1, Some(0))), // pass empty range to load empty nodes
              await_stable_before_resolve = false, hide_others = false, range_context = 0,
              progress = progress)
            queued = true
            Left(s"The origin ${quote(session.origin(node_name))} was not loaded and has now " +
              "been queued for loading. ")
          }
      }
    var result = attempt()
    val ticker = new PIDE_MCP_Status.Ticker(progress, "loading")
    while (result.isLeft && Time.now() < deadline) {
      progress.expose_interrupt()
      ticker.tick(java.io.File(session.origin(node_name)).getName + " is being loaded")
      Time.seconds(0.5).sleep()
      result = attempt()
    }
    val snapshot = result match {
      case Right(snapshot) => snapshot
      case Left(msg) =>
        error(msg + (if (wait_seconds > 0) s"Still not ready after waiting $wait_seconds seconds. " else "") +
          retry_soon_message)
    }
    if (!node_name.is_theory && snapshot.commands_loading.isEmpty)
      error(s"File ${quote(session.origin(node_name))} is not loaded by any theory. " +
        "Load the containing theory first.")
    snapshot
  }

  /* Synchronise loaded files that were edited on disk outside of the server (e.g. by scripts).
     Waits until the edit is part of the current document version (not until it is processed),
     so that the state never shows results of the replaced text. Returns the synced changes. */
  private val sync_lines_limit = 10

  // changed lines of the new text, e.g. "11", "151-152", "30 (removed)"
  private def sync_lines(change: PIDE_MCP_Session#Sync_Change): List[String] = {
    val ranges = change.hunks.map { h =>
      if (h.new_lines == 0) s"${h.new_line} (removed)"
      else if (h.new_lines == 1) h.new_line.toString
      else s"${h.new_line}-${h.new_line + h.new_lines - 1}"
    }
    if (ranges.length > sync_lines_limit)
      ranges.take(sync_lines_limit) :+ s"... (+${ranges.length - sync_lines_limit} more spans)"
    else ranges
  }

  private val sync_diff_keep = 100

  // write the diffs of a synchronisation to a file; returns its path
  private def write_sync_diff(session: PIDE_MCP_Session, result: PIDE_MCP_Session#Sync_Result): Option[String] =
    Option.when(session.sync_diff_dir.nonEmpty) {
      val dir = Path.explode(session.sync_diff_dir).absolute  // relative: project-local
      Isabelle_System.make_directory(dir)
      val ignore = dir + Path.basic(".gitignore")  // keep the diffs out of the project's git status
      if (!ignore.is_file) File.write(ignore, "*\n")
      val stamp = Date.Format("uuuuMMdd-HHmmss-SSS")(Date.now())
      val file = dir + Path.basic(s"sync_${session.id}_$stamp.diff")
      File.write(file, Symbol.encode(result.changes.map(_.diff).mkString("\n")))
      val old = Option(dir.file.listFiles).toList.flatten.filter(_.getName.endsWith(".diff"))
        .sortBy(_.getName).dropRight(sync_diff_keep)
      old.foreach(_.delete())
      File.platform_path(file)
    }

  def sync_disk_edits(session: PIDE_MCP_Session, progress: Progress): Option[JSON.Object.T] = {
    val result = session.sync_disk(progress)
    if (result.nonEmpty) {
      session.await_stable_snapshot(progress)
      session.resolve_dependencies(result.node_names, progress)
      Some(JSON_Object(
        "files" -> result.changes.map(change => JSON_Object(
          "origin" -> session.origin(change.node_name),
          "changed_lines" -> change.changed_lines,
          "spans" -> change.spans,
          "lines" -> sync_lines(change),
          "total_lines" -> change.total_lines) ++
          // what this edit makes Isabelle process again (counts; the old processing time is gone after an external edit)
          (reload_estimate(session, change.node_name) match {
            case Right(estimate) => JSON_Object("reprocessed" -> (estimate - "previous_processing_ms"))
            case Left(_) => JSON_Object()
          }) ++
          // a changed ML file: say at once whether it has a syntax error (its reload would only report it after minutes)
          (if (!change.node_name.is_theory && change.node_name.path.implode.endsWith(".ML"))
            Exn.result {
              val text = File.read(change.node_name.path)
              PIDE_MCP_ML_Check.check(text).toOption.filter(_.errors.nonEmpty).map(result =>
                PIDE_MCP_ML_Check.describe(change.node_name.path.base.implode, text, result))
            } match {
              case Exn.Res(Some(description)) => JSON_Object("ml_syntax_error" -> description)
              case _ => JSON_Object()
            }
           else JSON_Object()))) ++
        JSON.optional("diff_file" -> (Exn.result(write_sync_diff(session, result)) match {
          case Exn.Res(file) => file
          case Exn.Exn(_) => None // the diff is a convenience only
        })))
    }
    else None
  }

  // loaded theories imported (transitively) by node_name that are still being processed
  def upstream_pending(
    session: PIDE_MCP_Session,
    snapshot: Document.Snapshot,
    node_name: Document.Node.Name
  ): List[PIDE_MCP_Theory.Status] = {
    val nodes = snapshot.version.nodes
    @scala.annotation.tailrec
    def closure(todo: List[Document.Node.Name], seen: Set[Document.Node.Name]): Set[Document.Node.Name] =
      todo match {
        case Nil => seen
        case name :: rest =>
          if (seen(name)) closure(rest, seen) else closure(nodes(name).header.imports ::: rest, seen + name)
      }
    val upstream = closure(nodes(node_name).header.imports, Set.empty).toList.filter(name =>
      name.is_theory && !session.is_base_session_theory(name) &&
      PIDE_MCP_Util.is_loaded_dynamic(nodes(name)))
    PIDE_MCP_Theory.Status.statuses(session, snapshot, upstream)
      .filter(_.overall_status == Document_Status.Overall_Status.pending)
  }

  /* A theory is waiting if none of its commands runs yet because upstream theories are still being
     processed (e.g. after an edit of an imported file). With wait_seconds > 0, wait for that to end.
     Returns the latest snapshot and the upstream theories that the node is still waiting for. */
  def await_upstream(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    snapshot: Document.Snapshot,
    wait_seconds: Int,
    progress: Progress
  ): (Document.Snapshot, List[PIDE_MCP_Theory.Status]) =
    if (!node_name.is_theory) (snapshot, Nil)
    else {
      def is_waiting(snapshot: Document.Snapshot): Boolean = {
        val status = PIDE_MCP_Theory.Status.statuses(session, snapshot, List(node_name)).head.node_status
        status.unprocessed > 0 && status.running == 0
      }
      val deadline = Time.now() + Time.seconds(wait_seconds)
      var current = snapshot
      var upstream = upstream_pending(session, current, node_name)
      val ticker = new PIDE_MCP_Status.Ticker(progress, "waiting")
      var stuck_since: Option[Time] = None
      def stuck_for_long: Boolean = {
        if (upstream.nonEmpty && stuck_import(session, upstream).nonEmpty) { if (stuck_since.isEmpty) stuck_since = Some(Time.now()) }
        else stuck_since = None
        stuck_since.exists(Time.now() - _ >= Time.seconds(3))
      }
      while (upstream.nonEmpty && is_waiting(current) && !stuck_for_long && Time.now() < deadline) {
        progress.expose_interrupt()
        ticker.tick(wait_status(session, current, node_name, upstream))
        Time.seconds(0.5).sleep()
        current = session.node_snapshot(node_name)
        upstream = upstream_pending(session, current, node_name)
      }
      (current, if (is_waiting(current)) upstream else Nil)
    }

  /* What an edit of node_name makes PIDE process again: the theory (for a non-theory file, the theories
     loading it) and all loaded theories importing it, transitively. The time is what processing them
     took before. None if the node is not loaded. */
  def reload_estimate(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name
  ): Either[String, JSON.Object.T] =
    Exn.result {
      val snapshot = session.node_snapshot(node_name)
      val nodes = snapshot.version.nodes
      val dynamic_theories = nodes.iterator.collect { case (name, node)
        if name.is_theory && !session.is_base_session_theory(name) && PIDE_MCP_Util.is_loaded_dynamic(node) =>
          (name, node) }.toList
      val roots =
        if (node_name.is_theory) List(node_name)
        else {  // the theories whose commands load this file (also while the file is being reprocessed)
          val loading = dynamic_theories.collect {
            case (name, node) if node.commands.iterator.exists(_.blobs_names.contains(node_name)) => name }
          (loading ::: snapshot.commands_loading.map(_.node_name)).distinct
        }
      if (roots.isEmpty) Left("no loaded theory loads this file")
      else {
        val candidates = dynamic_theories.map(_._1)
        var affected = roots.toSet
        var more = List.empty[Document.Node.Name]
        while ({
          more = candidates.filter(name => !affected(name) && nodes(name).header.imports.exists(affected))
          affected ++= more
          more.nonEmpty
        }) ()
        val names = affected.toList
        val statuses = PIDE_MCP_Theory.Status.statuses(session, snapshot, names)
        Right(JSON_Object(
          "theories" -> names.length,
          "commands" -> names.map(name => nodes(name).commands.iterator.count(!_.is_ignored)).sum,
          "previous_processing_ms" -> statuses.map(_.node_status.cumulated_time.ms).sum))
      }
    } match {
      case Exn.Res(result) => result
      case Exn.Exn(_) => Left("the file was not loaded before this edit")
    }

  // failed or sorry'd commands before the requested range: later commands may rest on them although they show finished
  def earlier_problems_note(
    snapshot: Document.Snapshot,
    doc: Line.Document,
    start_line: Int
  ): Option[String] =
    if (start_line <= 1) None
    else {
      val before = PIDE_MCP_Util.text_range(doc, 1, start_line - 1).get
      val state = PIDE_MCP_Command.State.states_json(
        PIDE_MCP_Command.State.states(snapshot, Some(before)), doc, PIDE_MCP_Command.State.Options(),
        PIDE_MCP_Command.State.Detail.locations, Some(0), 100000)
      // distinct commands that failed or are sorry'd (a plain warning is not one; a failed `by simp` is `failed` and `bad` at once)
      val failed_or_bad = state.get("problems").collect { case JSON.Object(o) => o.get("problems") }.flatten
        .collect { case xs: List[?] => xs }.getOrElse(Nil).collect { case JSON.Object(o) => o }
        .filter(o => o.get("kinds").collect { case ks: List[?] => ks }.getOrElse(Nil)
          .exists(k => k == "failed" || k == "bad"))
      val problems = failed_or_bad.length
      val first = failed_or_bad.headOption.flatMap(_.get("line"))
      Option.when(problems > 0)(
        s"$problems failed or sorry'd commands before line $start_line" +
          first.fold("")(line => s" (first at line $line)") +
          ": later commands may rest on them although they show finished")
    }

  // a pending import that does not run and has failed commands (e.g. a malformed theory, a failed ML load): nothing in it will ever
  // progress, so everything that waits for it waits forever. Returns (origin of that import, line, first error message in full).
  def stuck_import(
    session: PIDE_MCP_Session,
    upstream: List[PIDE_MCP_Theory.Status]
  ): Option[(String, Int, String)] =
    upstream.filter(u => u.node_status.running == 0 && u.node_status.failed > 0).sortBy(_.origin).iterator.flatMap { u =>
      Exn.result {
        val name = session.node_name(u.origin)
        val snapshot = session.node_snapshot(name)
        first_failure(snapshot, Line.Document(snapshot.node.source)).map { case (line, message) => (u.origin, line, message) }
      } match {
        case Exn.Res(found) => found
        case Exn.Exn(_) => None
      }
    }.nextOption()

  def import_blocked_text(file: String, stuck: (String, Int, String)): String = {
    val (origin, line, message) = stuck
    val imp = java.io.File(origin).getName
    s"BLOCKED, NOT RUNNING: $file waits for its import $imp, which has failed at line $line and is not running: $message. " +
      s"Waiting or repeating this call changes nothing: fix $imp first"
  }

  /* PROVISIONAL (something is running or loading: wait) or BLOCKED (nothing runs, commands stay unprocessed because an earlier command,
     usually the header, failed: waiting changes nothing). Returns (provisional note, blocked note). */
  def settle_notes(
    session: PIDE_MCP_Session,
    snapshot: Document.Snapshot,
    node_name: Document.Node.Name,
    doc: Line.Document,
    state: JSON.Object.T,
    waiting: List[PIDE_MCP_Theory.Status]
  ): (Option[String], Option[String]) = {
    def count(key: String): Int =
      state.get(key).collect { case n: Int => n; case n: Long => n.toInt }.getOrElse(0)
    val unprocessed = count("commands_unprocessed")
    val running = count("commands_still_running_possibly_nonterminating")
    lazy val stuck = if (running == 0) stuck_import(session, waiting) else None
    if (unprocessed + running == 0 && waiting.isEmpty) (None, None)
    else if (stuck.nonEmpty)
      (None, Some(import_blocked_text(java.io.File(session.origin(node_name)).getName, stuck.get)))
    else {
      val file_running = node_name.is_theory && (Exn.result(
        PIDE_MCP_Theory.Status.statuses(session, snapshot, List(node_name)).head.node_status.running > 0) match {
          case Exn.Res(running) => running
          case Exn.Exn(_) => false
        })
      val busy = running > 0 || waiting.nonEmpty || file_running
      lazy val failure = first_failure(snapshot, doc)
      if (busy || failure.isEmpty) (provisional_note(unprocessed + running, waiting), None)
      else {
        val (line, message) = failure.get
        (None, Some(s"BLOCKED, NOT RUNNING: nothing in this file is running, and $unprocessed command(s) stay unprocessed because of the failure " +
          s"at line $line. Waiting or repeating this call changes nothing until that is fixed: $message"))
      }
    }
  }

  // the first failing command of the whole file with its first error message in full (up to 600 characters)
  def first_failure(snapshot: Document.Snapshot, doc: Line.Document): Option[(Int, String)] = {
    val range = PIDE_MCP_Util.text_range(doc, 1, doc.lines.length).get
    val state = PIDE_MCP_Command.State.states_json(
      PIDE_MCP_Command.State.states(snapshot, Some(range)), doc,
      PIDE_MCP_Command.State.Options(source_max_lines = 1),
      PIDE_MCP_Command.State.Detail.full, Some(0), 1)
    // the line is in first_problem; the full message in the first entry of the problems (which has no line key)
    val line = state.get("first_problem").collect { case JSON.Object(o) => o.get("line") }.flatten
      .collect { case n: Int => n; case n: Long => n.toInt }
    val message = state.get("problems").collect { case JSON.Object(o) => o.get("problems") }.flatten
      .collect { case (JSON.Object(entry)) :: _ => entry.get("error") }.flatten.flatMap {
        case s: String => Some(s)
        case (s: String) :: _ => Some(s)
        case _ => None
      }
    for (l <- line; m <- message) yield (l, m.trim.take(600))
  }

  def pending_commands(state: JSON.Object.T): Int =
    List("commands_unprocessed", "commands_still_running_possibly_nonterminating").map(key =>
      state.get(key).collect { case n: Int => n; case n: Long => n.toInt }.getOrElse(0)).sum

  /* Why a result may not be final: commands of the (range of the) file are still unprocessed or running,
     or the file waits for imported theories that are still being processed. */
  def provisional_note(pending_commands: Int, waiting: List[PIDE_MCP_Theory.Status]): Option[String] = {
    val reasons = List(
      Option.when(pending_commands > 0)(s"$pending_commands commands are still unprocessed or running"),
      Option.when(waiting.nonEmpty)(
        "waiting for imported theories still being processed: " + waiting.map(_.origin).mkString(", "))).flatten
    Option.when(reasons.nonEmpty)(
      "PROVISIONAL RESULT, errors and counts below may be transient (" + reasons.mkString("; ") +
        "): repeat with wait_seconds and wait_settled before acting on them")
  }

  /* A theory whose header failed on an import (`Bad theory import`) stays failed even when the imported theory appears later
     (PIDE does not look again as long as the text is unchanged). Unload it so that it is loaded, and its imports resolved,
     again. Returns true if it did. */
  def reload_failed_header(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    progress: Progress
  ): Boolean =
    Exn.result {
      val snapshot = session.node_snapshot(node_name)
      // the failed import is an error message of one of the first commands (the header), not an entry of header.errors
      node_name.is_theory && PIDE_MCP_Command.State.states(snapshot, None).take(3).exists(state =>
        state.results.exists(msg => Protocol.is_error(msg) &&
          PIDE_MCP_Util.elem_body_plain_text(msg).contains("Bad theory import")))
    } match {
      case Exn.Res(true) =>
        session.unload(List(node_name), progress)
        session.await_stable_snapshot(progress)
        true
      case _ => false
    }

  // one short line about what a waiting call is waiting for: commands processed of total, the running line, imports, or BLOCKED
  def wait_status(
    session: PIDE_MCP_Session,
    snapshot: Document.Snapshot,
    node_name: Document.Node.Name,
    upstream: List[PIDE_MCP_Theory.Status]
  ): String = {
    val doc = Line.Document(snapshot.node.source)
    val state = PIDE_MCP_Command.State.states_json(
      PIDE_MCP_Command.State.states(snapshot, None), doc, PIDE_MCP_Command.State.Options(),
      PIDE_MCP_Command.State.Detail.summary, Some(0), 1)
    def count(key: String): Int =
      state.get(key).collect { case n: Int => n; case n: Long => n.toInt }.getOrElse(0)
    val done = count("commands_finished") + count("commands_warned") + count("commands_failed")
    val running = count("commands_still_running_possibly_nonterminating")
    val unprocessed = count("commands_unprocessed")
    val file = java.io.File(session.origin(node_name)).getName
    lazy val stuck = stuck_import(session, upstream)
    if (upstream.nonEmpty && running == 0 && stuck.nonEmpty) {
      val (origin, line, message) = stuck.get
      s"$file BLOCKED: waits for its import ${java.io.File(origin).getName}, which failed at line $line and is not running: ${message.linesIterator.nextOption().getOrElse("").take(100)}"
    }
    else if (upstream.nonEmpty && running == 0)
      s"$file waits for imports " + upstream.map(u => s"${java.io.File(u.origin).getName} (${u.node_status.percentage}%)").mkString(", ")
    else if (running == 0 && unprocessed > 0) first_failure(snapshot, doc) match {
      case Some((line, message)) => s"$file BLOCKED, nothing runs: failure at line $line: ${message.linesIterator.nextOption().getOrElse("").take(100)}"
      case None => s"$file: $done of ${done + unprocessed} commands, not started yet"
    }
    else {
      val line = Exn.result(PIDE_MCP_Theory.commands_running_json(session, snapshot, List(node_name)).headOption
        .flatMap(_.get("source")).collect { case s: String => s }
        .flatMap(s => """^(\d+):""".r.findFirstMatchIn(s).map(_.group(1))))
      s"$file: $done of ${done + unprocessed + running} commands" +
        (line match { case Exn.Res(Some(l)) => s", running line $l"; case _ => "" })
    }
  }

  /* With wait_settled: wait until the node and all its dynamically loaded imports are completely
     processed, or the time is up (with stop_at_error: or one more command has failed).
     Returns the latest snapshot and the upstream theories that are still pending. */
  def await_settled(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    snapshot: Document.Snapshot,
    wait_seconds: Int,
    progress: Progress,
    stop_at_error: Boolean = false
  ): (Document.Snapshot, List[PIDE_MCP_Theory.Status]) =
    if (!node_name.is_theory) (snapshot, Nil)
    else {
      def observe(snapshot: Document.Snapshot) =
        (PIDE_MCP_Theory.Status.statuses(session, snapshot, List(node_name)).head.node_status,
          upstream_pending(session, snapshot, node_name))
      val deadline = Time.now() + Time.seconds(wait_seconds)
      val ticker = new PIDE_MCP_Status.Ticker(progress, "waiting")
      var blocked_since: Option[Time] = None
      var current = snapshot
      var (node, upstream) = observe(current)
      def settled: Boolean = node.unprocessed == 0 && node.running == 0 && upstream.isEmpty
      // nothing runs, imports are not pending, commands stay unprocessed after a failure: waiting longer changes nothing
      // (a file waiting on a stuck import may have no unprocessed commands of its own: its commands all failed)
      def blocked: Boolean =
        node.running == 0 &&
          (if (upstream.isEmpty) node.unprocessed > 0 && node.failed > 0 else stuck_import(session, upstream).nonEmpty)
      // a failure that is already there must not end the wait: only an additional one does, and only on request
      val failed_before = node.failed
      def blocked_for_long: Boolean = {
        if (blocked) { if (blocked_since.isEmpty) blocked_since = Some(Time.now()) } else blocked_since = None
        blocked_since.exists(Time.now() - _ >= Time.seconds(3))
      }
      while (!settled && !blocked_for_long && !(stop_at_error && node.failed > failed_before) && Time.now() < deadline) {
        progress.expose_interrupt()
        ticker.tick(wait_status(session, current, node_name, upstream))
        Time.seconds(0.5).sleep()
        current = session.node_snapshot(node_name)
        val observed = observe(current)
        node = observed._1
        upstream = observed._2
      }
      (current, upstream)
    }

  def with_waiting(
    json: JSON.Object.T,
    waiting: List[PIDE_MCP_Theory.Status]
  ): JSON.Object.T =
    json ++ JSON.optional("waiting_for" -> proper_list(waiting.map(status => JSON_Object(
      "origin" -> status.origin,
      "percentage_commands_processed" -> status.node_status.percentage))))

  def with_synced(
    json: JSON.Object.T,
    synced: Option[JSON.Object.T]
  ): JSON.Object.T =
    json ++ JSON.optional("external_edits_synced" -> synced)

  // offset in the file for a query: the end of a line (or the given prefix of it), by default the
  // first command (the theory header), where the theory context with all imports is available
  def query_offset(
    snapshot: Document.Snapshot,
    start_line: Option[Int],
    prefix: Option[String]
  ): Text.Offset = {
    val doc = Line.Document(snapshot.node.source)
    start_line match {
      case None => // the last command before `end`: the context with everything the file defines
        val proper = snapshot.node.commands.iterator.filter(!_.is_ignored).toList
        proper.reverseIterator.find(_.span.name != "end").orElse(proper.headOption)
          .flatMap(snapshot.node.command_start).getOrElse(error("The file has no commands"))
      case Some(line) =>
        require_lines_in_bounds(Some(line), None, doc.lines.length)
        val range = PIDE_MCP_Util.text_range(doc, line, line).get
        val text = range.substring(snapshot.node.source).stripLineEnd
        val indent = text.length - text.stripLeading.length
        val caret = prefix match {
          case None => text.length
          case Some(p) =>  // leading whitespace does not matter
            if (!text.stripLeading.startsWith(p.stripLeading))
              error(s"Line $line does not start with ${quote(p)}. Actual line: ${quote(text)}")
            indent + p.stripLeading.length
        }
        range.start + caret
    }
  }

  def require_ordered_lines(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int]
  ): Unit = {
    for (start_line <- opt_start_line; end_line <- opt_end_line if end_line < start_line)
      error(s"end_line $end_line < start_line $start_line")
  }

  def require_lines_in_bounds(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    total_lines: Int
  ): Unit = {
    for (start_line <- opt_start_line 
        if start_line < 1 || start_line > (total_lines max default_start_line))
      error(s"start_line $start_line out of bounds (file has $total_lines lines)")
    for (end_line <- opt_end_line if end_line < 1 || end_line > total_lines)
      error(s"end_line $end_line out of bounds (file has $total_lines lines)")
  }

  def require_valid_lines(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    total_lines: Int
  ): Unit = {
    require_ordered_lines(opt_start_line, opt_end_line)
    require_lines_in_bounds(opt_start_line, opt_end_line, total_lines)
  }

  def resolve_lines(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    total_lines: Int
  ): (Int, Int) = {
    require_valid_lines(opt_start_line, opt_end_line, total_lines)
    (opt_start_line.getOrElse(default_start_line), opt_end_line.getOrElse(total_lines))
  }
}
