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
    while (result.isLeft && Time.now() < deadline) {
      progress.expose_interrupt()
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
          "total_lines" -> change.total_lines))) ++
        JSON.optional("diff_file" -> (Exn.result(write_sync_diff(session, result)) match {
          case Exn.Res(file) => file
          case Exn.Exn(_) => None // the diff is a convenience only
        })))
    }
    else None
  }

  def with_synced(
    json: JSON.Object.T,
    synced: Option[JSON.Object.T]
  ): JSON.Object.T =
    json ++ JSON.optional("external_edits_synced" -> synced)

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
