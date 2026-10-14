/*  Title:      PIDE_MCP/pide_mcp_command.scala
    Author:     Kevin Kappelmann

Command utilities.
*/

package isabelle.pide.mcp

import isabelle._

import scala.collection.mutable

object PIDE_MCP_Command {
  object Status {
    val unprocessed = "unprocessed"
    // experimental verbose name: remind agents of possible non-termination
    val running = "still_running_possibly_nonterminating"
    val warned = "warned"
    val failed = "failed"
    val finished = "finished"
    val canceled = "canceled"
    val all: List[String] = List(unprocessed, running, warned, failed, finished, canceled)

    def key(status: String): String = "commands_" + status

    def prefix_keys(obj: JSON.Object.T): JSON.Object.T = {
      val keys = all.toSet + "bad"
      obj.map { case (k, v) => if (keys.contains(k)) (key(k), v) else (k, v) }
    }
  }

  def status(snapshot: Document.Snapshot, command: Command): Document_Status.Command_Status =
    snapshot.state.command_status(snapshot.version, command)

  def eval_exec(snapshot: Document.Snapshot, command: Command): Option[Document_ID.Exec] =
    snapshot.state.assignments.get(snapshot.version.id).flatMap(_.command_execs.get(command.id)).flatMap(_.headOption)

  /* Since when an evaluation of a command runs: PIDE times the evaluation itself, but not a forked proof
     (`by` of a lemma), which then shows 0 ms however long it runs. Fed by each session's commands_changed. */
  object Running_Clock {
    private val since = Synchronized(Map.empty[(Document_ID.Command, Document_ID.Exec), Time])
    // evaluations that stopped running recently: when, and after how long (for `cancel` that comes too late)
    private val stopped = Synchronized(Map.empty[(Document_ID.Command, Document_ID.Exec), (Time, Time)])
    private val stopped_keep = Time.seconds(60)

    def observe(snapshot: Document.Snapshot, commands: Set[Command]): Unit = {
      val now = Time.now()
      val updates = commands.toList.map(cmd =>
        (cmd.id, eval_exec(snapshot, cmd), snapshot.node_name.is_theory && status(snapshot, cmd).is_running))
      val ended = since.change_result { map =>
        val map1 = updates.foldLeft(map) {
          case (m, (id, Some(exec), true)) => if (m.contains((id, exec))) m else m + ((id, exec) -> now)
          case (m, (id, _, _)) => m.filterNot(_._1._1 == id)
        }
        (map.filterNot(p => map1.contains(p._1)), map1)
      }
      if (ended.nonEmpty || stopped.value.nonEmpty)
        stopped.change(m => m.filter(p => now - p._2._1 < stopped_keep) ++ ended.map((k, t) => k -> (now, now - t)))
    }

    // stopped running this long ago, after running for so long
    def stopped_ago(snapshot: Document.Snapshot, command: Command): Option[(Time, Time)] =
      eval_exec(snapshot, command).flatMap(exec => stopped.value.get((command.id, exec)))
        .map((at, ran) => (Time.now() - at, ran))

    // evaluations interrupted by `cancel` or a time limit, with their running time then: they are shown as canceled
    private val canceled = Synchronized(Map.empty[(Document_ID.Command, Document_ID.Exec), Long])

    def cancel(snapshot: Document.Snapshot, command: Command, running_ms: Long): Unit =
      eval_exec(snapshot, command).foreach(exec => canceled.change(_ + ((command.id, exec) -> running_ms)))

    def canceled_ms(snapshot: Document.Snapshot, command: Command): Option[Long] =
      eval_exec(snapshot, command).flatMap(exec => canceled.value.get((command.id, exec)))

    def elapsed_ms(snapshot: Document.Snapshot, command: Command): Long =
      eval_exec(snapshot, command).flatMap(exec => since.value.get((command.id, exec)))
        .fold(0L)(t => (Time.now() - t).ms)
  }

  private def status_list(status: Document_Status.Command_Status): List[String] =
    List(
      Option.when(status.is_unprocessed)(Status.unprocessed),
      Option.when(status.is_running)(Status.running),
      Option.when(status.is_warned)(Status.warned),
      Option.when(status.is_failed)(Status.failed),
      Option.when(status.is_finished)(Status.finished),
      Option.when(status.is_canceled)(Status.canceled)).flatten

  def iterator(snapshot: Document.Snapshot, range: Option[Text.Range]): Iterator[(Command, Text.Offset)] =
    range.fold(snapshot.node.command_iterator())(snapshot.node.command_iterator(_))

  // like select, but returns all covering markups (full markup stack) at each sub-range
  private def select_covering(
    snapshot: Document.Snapshot,
    range: Text.Range,
    elements: Markup.Elements
  ): List[Text.Info[List[Text.Info[XML.Elem]]]] =
    snapshot.cumulate(range, List.empty[Text.Info[XML.Elem]], elements,
      _ => { case (acc, info) => Some(info :: acc) })

  val Markup_ML: String = "ML"

  def bad_json(snapshot: Document.Snapshot, range: Text.Range): List[JSON.Object.T] =
    snapshot.select(range, Markup.Elements(Markup.BAD), _ => {
      case Text.Info(r, elem) =>
        Some(JSON_Object(
          "message" -> PIDE_MCP_Util.elem_body_plain_text(elem),
          "source" -> r.substring(snapshot.node.source)))
    }).map(_.info)

  def facts_json(snapshot: Document.Snapshot, range: Text.Range): List[String] = {
    val fact_kinds = Set(Markup.FACT, Markup.DYNAMIC_FACT, Markup.LITERAL_FACT)
    snapshot.select(range, Markup.Elements(Markup.ENTITY), _ => {
      case Text.Info(_, XML.Elem(Name_Space.Entity(entry), _)) if fact_kinds.contains(entry.kind) =>
        Some(entry.name)
      case _ => None
    }).map(_.info)
  }

  // this is hairy, but I think there is no good library function to obtain type information
  def types_json(snapshot: Document.Snapshot, range: Text.Range): List[JSON.Object.T] = {
    val term_kinds = Set(Markup.FREE, Markup.BOUND, Markup.VAR, Markup.SKOLEM, Markup.CONST)
    val elements =
      Markup.Elements(term_kinds.toSeq :+ Markup.TYPING :+ Markup.ENTITY :+ Markup.ML_TYPING: _*)

    def kind_and_type(at_range: List[Text.Info[XML.Elem]]): Option[(String, String)] =
      at_range.collectFirst { case Text.Info(_, e) if e.name == Markup.ML_TYPING =>
        (Markup_ML, XML.content(e.body))
      }.orElse(for {
        kind_elem <- at_range.collectFirst {
          case Text.Info(_, e) if term_kinds.contains(e.name) => e }
        typing_elem <- at_range.collectFirst {
          case Text.Info(_, e) if e.name == Markup.TYPING => e }
      } yield (kind_elem.name, XML.content(typing_elem.body)))

    select_covering(snapshot, range, elements).flatMap {
      case Text.Info(r, infos @ (info :: _)) if info.range == r => // only keep leaf nodes
        val at_range = infos.filter(_.range == r)
        val name_at = PIDE_MCP_Util.display_name(
          at_range.collectFirst { case Text.Info(_, XML.Elem(Name_Space.Entity(entry), _)) => entry },
          r, snapshot.node.source)
        kind_and_type(at_range).map { case (kind, t) =>
          JSON_Object("name" -> name_at, "kind" -> kind, "type" -> t)
        }
      case _ => None
    }.distinct
  }

  def markup_json(
    snapshot: Document.Snapshot,
    range: Text.Range,
    elements: Markup.Elements
  ): List[JSON.Object.T] =
    select_covering(snapshot, range, elements).flatMap {
      case Text.Info(r, infos) =>
        infos.filter(_.range == r).map(i => PIDE_MCP_JSON.from_xml(i.info))
    }

  private def self_id(
    snapshot: Document.Snapshot,
    cmd: Command
  )(id: Document_ID.Generic): Boolean =
    id == cmd.id || snapshot.state.lookup_id(id).exists(_.command.id == cmd.id)

  def results(
    snapshot: Document.Snapshot,
    cmd: Command,
    command_start: Text.Offset,
    range: Text.Range
  ): Iterator[XML.Elem] = {
    val chunk_name =
      snapshot.commands_loading.headOption match {
        case None => Symbol.Text_Chunk.Default
        case Some(_) => Symbol.Text_Chunk.File(snapshot.node_name.node)
      }
    def in_range(chunk: Symbol.Text_Chunk)(elem: XML.Elem): Boolean = {
      val positions = cmd.message_positions(self_id(snapshot, cmd), chunk_name, chunk, elem)
      positions.isEmpty || positions.exists(pos => (pos + command_start).overlaps(range))
    }
    val elems = snapshot.command_results(cmd).iterator.collect { case (_, elem: XML.Elem) => elem }
    cmd.chunks.get(chunk_name).fold(elems)(chunk => elems.filter(in_range(chunk)))
  }

  object Message_Kind {
    val important: List[String] = List("goal", "error", "warning")
    val info: List[String] = List("writeln", "information", "tracing")
    val all: List[String] = important ::: info
  }

  def message_entries(
    elements: Iterator[XML.Elem],
    kinds: List[String],
    max_lines: Option[Int] = None,
    goal_max_lines: Option[Int] = None
  ): List[Option[JSON.Object.Entry]] = {
    val texts = classify_results(elements)
    kinds.map(kind => texts.get(kind).map { messages =>
      // datatype and similar commands print one `consts` block per generated constant (typerep_, random_, ...)
      val shown = if (kind == "writeln" && max_lines.nonEmpty) collapse_consts(messages) else messages
      kind -> (
        if (kind == "goal" && goal_max_lines.nonEmpty) shown.map(cap_goal(_, goal_max_lines.get))
        else max_lines.fold(shown)(n => shown.map(truncate_lines(_, n))))
    })
  }

  /* Goals that a method blew up (thousands of lines, or one line of 100k characters) flood the context: say how large a
     big goal is, and cut it after max_lines lines (and max_lines * 200 characters). */
  val goal_size_note_lines: Int = 30
  val goal_warning_lines: Int = 200

  def cap_goal(text: String, max_lines: Int): String = {
    val lines = text.linesIterator.toList
    val n = lines.length
    val chars = text.length
    val max_chars = max_lines.toLong * 200
    val large = n >= goal_warning_lines || chars >= goal_warning_lines * 200
    val header =
      Option.when(n >= goal_size_note_lines || large)(
        s"(goal: $n lines, $chars characters" +
          (if (large) "; VERY LARGE: the method probably blew the term up, try a more targeted method" else "") + ")")
    val cut =
      if (max_lines <= 0 || n <= max_lines && chars <= max_chars) text
      else {
        val kept = lines.take(max_lines).mkString("\n")
        val kept1 = if (kept.length > max_chars) kept.take(max_chars.toInt) else kept
        val rest = n - kept1.linesIterator.length
        kept1 + s"\n... (+$rest more lines, ${chars - kept1.length} characters; raise goal_max_lines)"
      }
    header.fold(cut)(_ + "\n" + cut)
  }

  private val collapse_consts_threshold = 3

  def collapse_consts(messages: List[String]): List[String] = {
    val aux = messages.count(_.startsWith("consts"))
    if (aux < collapse_consts_threshold) messages
    else {
      var first = true
      messages.flatMap { m =>
        if (!m.startsWith("consts")) List(m)
        else if (first) { first = false; List(s"($aux `consts` messages of generated constants omitted)") }
        else Nil
      }
    }
  }

  private def classify_results(elements: Iterator[XML.Elem]): Map[String, List[String]] =
    elements.flatMap { elem =>
      val text = PIDE_MCP_Util.elem_body_plain_text(elem)
      if (Protocol.is_state(elem)) Some("goal" -> text)
      else if (Protocol.is_error(elem)) Some("error" -> text)
      else if (Protocol.is_warning_or_legacy(elem)) Some("warning" -> text)
      else if (Protocol.is_writeln(elem)) Some("writeln" -> text)
      else if (Protocol.is_information(elem)) Some("information" -> text)
      else if (Protocol.is_tracing(elem)) Some("tracing" -> text)
      else None
    }.toList.groupMap(_._1)(_._2)

  // the first informative line of a message: headers like "ML error:" continue with the next line
  def message_line(text: String): String =
    text.linesIterator.map(_.trim).filter(_.nonEmpty).toList match {
      case header :: next :: _ if header.endsWith(":") => header + " " + next
      case line :: _ => line
      case Nil => ""
    }

  // first informative line of the first message, marking how much of all messages it leaves out
  def message_summary(messages: List[String]): Option[String] =
    messages.headOption.map { first =>
      def lines(text: String): List[String] = text.linesIterator.map(_.trim).filter(_.nonEmpty).toList
      val used = lines(first) match {
        case header :: _ :: _ if header.endsWith(":") => 2
        case _ => 1
      }
      val more = messages.map(lines(_).length).sum - used
      message_line(first) + (if (more > 0) s" (+$more more lines, see detail: full)" else "")
    }

  // the echo of top-level ML declarations, e.g. "val x = 1: int" (a heuristic: it is plain writeln output)
  private val ml_echo = """(?s)\s*(val|structure|signature|functor|exception|type|eqtype|datatype)\s.*""".r
  def is_ml_echo(elem: XML.Elem): Boolean =
    Protocol.is_writeln(elem) &&
      ml_echo.matches(PIDE_MCP_Util.elem_body_plain_text(elem).linesIterator.nextOption().getOrElse(""))

  def truncate_lines(text: String, max_lines: Int): String = {
    val lines = text.linesIterator.toList
    if (lines.length <= max_lines) text
    else (lines.take(max_lines) :+ s"... (+${lines.length - max_lines} lines)").mkString("\n")
  }

  object State {
    def commands(snapshot: Document.Snapshot, range: Option[Text.Range]): Iterator[State] =
      iterator(snapshot, range).filterNot(_._1.is_ignored).map { case (cmd, command_start) =>
        val full = cmd.range + command_start
        val restricted = PIDE_MCP_Util.intersect_range(full, range)
        State(snapshot, cmd, restricted, results(snapshot, cmd, command_start, full).toList, full)
      }

    def command_spans(
      snapshot: Document.Snapshot,
      theory_cmd: Command,
      range: Option[Text.Range]
    ): Iterator[State] =
      snapshot.command_spans(PIDE_MCP_Util.restrict_text_range(snapshot.node.source, range))
        .iterator.filterNot(span =>
          PIDE_MCP_Util.is_ignored_source(span.range.substring(snapshot.node.source))).map { span =>
          val restricted = PIDE_MCP_Util.intersect_range(span.range, range)
          State(snapshot, theory_cmd, restricted,
            Rendering.text_messages(snapshot, span.range).map(_.info), span.range)
        }

    def blob(snapshot: Document.Snapshot, range: Option[Text.Range]): Option[State] =
      snapshot.commands_loading.headOption.map { cmd =>
        val restricted = PIDE_MCP_Util.restrict_text_range(snapshot.node.source, range)
        val whole = PIDE_MCP_Util.restrict_text_range(snapshot.node.source, None)
        State(snapshot, cmd, restricted, results(snapshot, cmd, 0, restricted).toList, restricted,
          if (restricted == whole) Nil
          else results(snapshot, cmd, 0, whole).filter(Protocol.is_error).toList)
      }

    def states(snapshot: Document.Snapshot, range: Option[Text.Range]): Iterator[State] =
      if (!snapshot.node_name.is_theory) blob(snapshot, range).iterator
      else {
        snapshot.node.get_theory match {
          case Some(theory_cmd) => command_spans(snapshot, theory_cmd, range)
          case None => commands(snapshot, range)
        }
      }

    enum Detail { case summary, locations, full }

    object Detail {
      val format = PIDE_MCP_Tool_Arg.Format(
        JSON_Object("type" -> "string", "enum" -> values.toList.map(_.toString)),
        { case mode: String => values.find(_.toString == mode); case _ => None },
        _.toString)
    }

    private val proof_closers = Set("by", "qed", "done", "oops", "sorry", "end", ".", "..", "next", "}")

    val default_source_max_lines: Int = 5
    val default_info_max_lines: Int = 30
    val default_goal_max_lines: Int = 100
    val default_problems_limit: Int = 10

    def states_json(
      states: Iterator[State],
      doc: Line.Document,
      opts: Options,
      detail: Detail,
      limit: Option[Int],
      problems_limit: Int
    ): JSON.Object.T = {
      val counts = mutable.Map[String, Int]().withDefaultValue(0)
      val returned_commands = new mutable.ListBuffer[JSON.Object.T]
      val returned_problems = new mutable.ListBuffer[JSON.Object.T]
      var first_problem: Option[JSON.Object.T] = None
      var previous: Option[(Int, String)] = None  // line and name of the command before
      var open_cause: Option[Option[(Int, String)]] = None  // the command that left a proof open, if the "Bad context" failures point to one
      var open_checked = false
      var open_count = 0
      var total_timing_ms = 0L
      var count = 0
      var problems = 0
      for (state <- states) {
        if (detail == Detail.full && limit.forall(count < _)) returned_commands += state.json(doc, opts)
        count += 1
        total_timing_ms += state.timing_ms
        for (flag <- state.status) counts(flag) += 1
        counts("bad") += state.bad.length
        counts("errors") += state.errors
        counts("warnings") += state.warnings
        if (state.bad_context) {
          if (!open_checked) {  // after a command that closes a proof the failure has another reason: say nothing then
            open_checked = true
            if (previous.forall(p => !proof_closers.contains(p._2))) open_cause = Some(previous)
          }
          if (open_cause.nonEmpty) open_count += 1
        }
        previous = Some((doc.position(state.command_range.start).line1, state.cmd.span.name))
        val problem_kinds = state.problem_kinds
        if (problem_kinds.nonEmpty) {
          problems += 1
          if (first_problem.isEmpty && problem_kinds.exists(Set(Status.failed, "errors", "errors_outside_range")))
            first_problem = Some(state.location_json(doc, problem_kinds))
          if (detail != Detail.summary && problems <= problems_limit) {
            returned_problems += (
              if (detail == Detail.full) state.json(doc, opts) + ("kinds" -> problem_kinds)
              else state.location_json(doc, problem_kinds))
          }
        }
      }
      val kinds = List(Status.running, Status.warned, Status.failed, Status.canceled, "bad",
        "errors", "warnings", Status.unprocessed, Status.finished)
      Status.prefix_keys(JSON_Object("total_timing_ms" -> total_timing_ms) ++
        JSON_Object(kinds.map(kind => kind -> counts(kind)))) ++
        JSON.optional("first_problem" -> first_problem) ++
        JSON.optional("open_proof" -> open_cause.map { cause =>
          val after = cause.fold("the proof before these commands")(c => s"the command at line ${c._1} (`${c._2}`)")
          s"$open_count command(s) fail only because $after left a proof or goal open (a failed or incomplete command leaves its goal open, " +
            "and the next command then fails with `Bad context ... using reset state`): finish or abandon that proof first (`done`, `sorry`, `oops`); " +
            "run experiments in a scratch theory (create_scratch) instead of the file you are building" }) +
        ("problems" -> (JSON_Object("count" -> problems) ++
          JSON_Object.if_proper(detail != Detail.summary,
            JSON_Object("count_returned" -> returned_problems.length,
              "problems" -> returned_problems.toList)))) +
        ("commands" -> (JSON_Object("count" -> count) ++
          JSON_Object.if_proper(detail == Detail.full,
            JSON_Object("count_returned" -> returned_commands.length,
              "commands" -> returned_commands.toList))))
    }

    sealed case class Options(
      include_types: Boolean = false,
      include_facts: Boolean = false,
      include_infos: Boolean = false,
      include_full_markup: Boolean = false,
      source_max_lines: Int = default_source_max_lines,
      goal_max_lines: Int = default_goal_max_lines,
      info_max_lines: Int = default_info_max_lines,
      include_ml_echo: Boolean = true
    )
  }

  sealed case class State private(
    snapshot: Document.Snapshot,
    cmd: Command,
    range: Text.Range, // requested part of the command, for types, facts, and markup
    results: List[XML.Elem],
    command_range: Text.Range, // the whole command, for status, messages, and source
    outside_errors: List[XML.Elem] = Nil // errors outside the requested range of a file-wide command
  ) {
    lazy val cmd_status: Document_Status.Command_Status =
      PIDE_MCP_Command.status(snapshot, cmd)

    private def has_markup(elements: Markup.Elements): Boolean =
      snapshot.select(command_range, elements, _ => { case _ => Some(()) }).nonEmpty

    // interrupted by `cancel` or a time limit, after this many ms (not once it runs again)
    lazy val canceled_ms: Option[Long] =
      Running_Clock.canceled_ms(snapshot, cmd).filter(_ => !cmd_status.is_running)

    lazy val status: List[String] = {
      val list = status_list(cmd_status).filter {
        case Status.warned => has_markup(Markup.Elements(Markup.WARNING, Markup.LEGACY))
        case Status.failed => has_markup(Markup.Elements(Markup.FAILED, Markup.ERROR))
        case _ => true
      }
      if (canceled_ms.nonEmpty && !list.contains(Status.canceled)) list :+ Status.canceled else list
    }
    lazy val timing_ms: Long = cmd_status.timings.sum(Date.now()).ms
    // how long the command has been running, also for a forked proof
    lazy val running_ms: Long =
      if (status.contains(Status.running)) timing_ms max Running_Clock.elapsed_ms(snapshot, cmd) else 0L
    lazy val errors: Int = results.count(Protocol.is_error)
    lazy val warnings: Int = results.count(Protocol.is_warning_or_legacy)
    lazy val bad: List[JSON.Object.T] = bad_json(snapshot, command_range)

    // the problem kinds of this command: relevant status flags, bad, errors, warnings
    lazy val problem_kinds: List[String] =
      status.filter(flag => flag != Status.unprocessed && flag != Status.finished) :::
        List(
          Option.when(bad.nonEmpty)("bad"),
          Option.when(errors > 0)("errors"),
          Option.when(outside_hint.nonEmpty)("errors_outside_range"),
          Option.when(warnings > 0)("warnings")).flatten

    // the command failed only because the proof before it was left open ("Bad context for command ... using reset state")
    lazy val bad_context: Boolean =
      results.exists(r => Protocol.is_error(r) && PIDE_MCP_Util.elem_body_plain_text(r).contains("Bad context for command"))

    // errors of a file (one command) that lie outside the requested lines
    lazy val outside_hint: Option[String] =
      Option.when(errors == 0 && outside_errors.nonEmpty)(
        s"${outside_errors.length} error(s) elsewhere in the file, outside the requested lines, first: " +
          message_line(PIDE_MCP_Util.elem_body_plain_text(outside_errors.head)))

    private def source_text: String = command_range.substring(snapshot.node.source).stripLineEnd

    private def first_message(max_length: Int = 300): Option[String] = {
      def texts(pred: XML.Elem => Boolean) =
        results.iterator.filter(pred).map(PIDE_MCP_Util.elem_body_plain_text).toList
      message_summary(texts(Protocol.is_error))
        .orElse(message_summary(texts(Protocol.is_warning_or_legacy)))
        .orElse(bad.headOption.flatMap(_.get("message")).collect { case s: String => message_line(s) })
        .orElse(outside_hint)
        .filter(_.nonEmpty)
        .map(line => canceled_ms.fold(line)(ms => s"cancelled after ${ms / 1000} s; $line"))
        .orElse(canceled_ms.map(ms => s"cancelled after ${ms / 1000} s"))
        .map(line =>
          if (line.length <= max_length) line
          else {  // cut at a word boundary
            val cut = line.take(max_length)
            val at = cut.lastIndexWhere(_.isWhitespace)
            (if (at > max_length / 2) cut.take(at) else cut) + "..."
          })
    }

    // compact entry without source text
    def location_json(doc: Line.Document, kinds: List[String]): JSON.Object.T = {
      val line = doc.position(command_range.start).line1
      val end_line = line + Line.logical_lines(source_text).length - 1
      JSON_Object.flatten(List(
        Some("line" -> line),
        Option.when(end_line > line)("end_line" -> end_line),
        Some("kinds" -> kinds),
        Option.when(status.contains(Status.running))("running_seconds" -> running_ms / 1000),
        first_message().filter(_.nonEmpty).map("first_message" -> _)))
    }

    def json(doc: Line.Document, opts: State.Options): JSON.Object.T = {
      val source_line = doc.position(command_range.start).line1
      val source_lines = Line.logical_lines(source_text)
      val source =
        if (source_lines.length > opts.source_max_lines)
          PIDE_MCP_Util.numbered_lines(source_lines.take(opts.source_max_lines), source_line) +
            s"\n... (+${source_lines.length - opts.source_max_lines} lines)"
        else PIDE_MCP_Util.numbered_lines(source_lines, source_line)
      val entries: List[Option[JSON.Object.Entry]] =
        List(
          Some("status" -> status),
          Some("timing_ms" -> (timing_ms max running_ms)),
          Some("source" -> source),
          outside_hint.map("hint" -> _),
          proper_list(bad).map("bad" -> _)) :::
        message_entries(results.iterator, Message_Kind.important, goal_max_lines = Some(opts.goal_max_lines)) :::
        List(
          Option.when(opts.include_types)(
            proper_list(types_json(snapshot, range)).map("types" -> _)).flatten,
          Option.when(opts.include_facts)(
            proper_list(facts_json(snapshot, range)).map("facts" -> _)).flatten) :::
        (if (opts.include_infos)
          message_entries(
            results.iterator.filter(elem => opts.include_ml_echo || !is_ml_echo(elem)),
            Message_Kind.info, Some(opts.info_max_lines)) else Nil) :::
        List(
          Option.when(opts.include_full_markup)(
            "markup" -> markup_json(snapshot, range, Markup.Elements.full)))
      JSON_Object.flatten(entries)
    }
  }

}
