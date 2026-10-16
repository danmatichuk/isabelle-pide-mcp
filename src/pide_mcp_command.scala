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

    def observe(snapshot: Document.Snapshot, commands: Set[Command]): Unit = {
      val now = Time.now()
      val updates = commands.toList.map(cmd =>
        (cmd.id, eval_exec(snapshot, cmd), snapshot.node_name.is_theory && status(snapshot, cmd).is_running))
      since.change(map => updates.foldLeft(map) {
        case (m, (id, Some(exec), true)) => if (m.contains((id, exec))) m else m + ((id, exec) -> now)
        case (m, (id, _, _)) => m.filterNot(_._1._1 == id)
      })
    }

    def elapsed_ms(snapshot: Document.Snapshot, command: Command): Long =
      eval_exec(snapshot, command).flatMap(exec => since.value.get((command.id, exec)))
        .fold(0L)(t => (Time.now() - t).ms)

    // evaluations interrupted by the step time limit, with their running time then: they are shown as canceled
    private val canceled = Synchronized(Map.empty[(Document_ID.Command, Document_ID.Exec), (Long, Time)])
    private val canceled_max = 2000

    def cancel(snapshot: Document.Snapshot, command: Command, running_ms: Long): Unit =
      eval_exec(snapshot, command).foreach(exec => canceled.change { m =>
        (if (m.size >= canceled_max) m.toList.sortBy(_._2._2.ms).drop(m.size - canceled_max + 1).toMap else m) +
          ((command.id, exec) -> (running_ms, Time.now())) })

    def canceled_ms(snapshot: Document.Snapshot, command: Command): Option[Long] =
      eval_exec(snapshot, command).flatMap(exec => canceled.value.get((command.id, exec))).map(_._1)

    // how long ago the interrupt was sent (for a command that has not stopped yet)
    def interrupt_sent_ago(snapshot: Document.Snapshot, command: Command): Option[Time] =
      eval_exec(snapshot, command).flatMap(exec => canceled.value.get((command.id, exec))).map(p => Time.now() - p._2)
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
    goal_max_lines: Option[Int] = None
  ): List[Option[JSON.Object.Entry]] = {
    val texts = classify_results(elements)
    kinds.map(kind => texts.get(kind).map { messages =>
      kind -> (
        // errors embed goals too (`Failed to apply proof method`, `Failed to finish proof`): capped in the same way
        if (goal_max_lines.nonEmpty && (kind == "goal" || kind == "error" || kind == "warning"))
          messages.map(cap_goal(_, goal_max_lines.get, if (kind == "goal") "goal" else kind))
        else messages)
    })
  }

  /* Goals that a method blew up (thousands of lines, or one line of 100k characters) flood the context: say how large a
     big goal is, and cut it after max_lines lines (and max_lines * 200 characters). */
  val goal_size_note_lines: Int = 30
  val goal_warning_lines: Int = 200

  private val Subgoals = """goal \((\d+) subgoals?\)""".r.unanchored
  private val Subgoal_Line = """ \d+\. .*""".r

  def cap_goal(text: String, max_lines: Int, label: String = "goal"): String = {
    val lines = text.linesIterator.toList
    val n = lines.length
    val chars = text.length
    val max_chars = max_lines.toLong * 200
    val large = n >= goal_warning_lines || chars >= goal_warning_lines * 200
    // from the header "goal (N subgoals)", or else (an interim goal) its numbered subgoals
    val subgoals =
      (text match {
        case Subgoals(k) => Some(k.toInt)
        case _ => Some(lines.count(Subgoal_Line.matches)).filter(_ > 0)
      }).fold("")(k => s", $k subgoal${if (k == 1) "" else "s"}")
    val header =
      Option.when(n >= goal_size_note_lines || large)(
        s"($label: $n lines, $chars characters$subgoals" +
          (if (large) "; VERY LARGE: the method probably blew the term up" else "") + ")")
    val cut =
      if (max_lines <= 0 || n <= max_lines && chars <= max_chars) text
      else {
        val kept = lines.take(max_lines).mkString("\n")
        val kept1 = if (kept.length > max_chars) kept.take(max_chars.toInt) else kept
        val rest = n - kept1.linesIterator.length
        kept1 + s"\n... (+$rest more lines, ${chars - kept1.length} characters$subgoals; raise goal_max_lines)"
      }
    header.fold(cut)(_ + "\n" + cut)
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
        State(snapshot, cmd, restricted, results(snapshot, cmd, 0, restricted).toList, restricted)
      }

    def states(snapshot: Document.Snapshot, range: Option[Text.Range]): Iterator[State] =
      if (!snapshot.node_name.is_theory) blob(snapshot, range).iterator
      else {
        snapshot.node.get_theory match {
          case Some(theory_cmd) => command_spans(snapshot, theory_cmd, range)
          case None => commands(snapshot, range)
        }
      }

    val default_goal_max_lines: Int = 40

    /* Problematic commands (running, warned, failed, canceled, bad, with errors or warnings) are listed once in
       `problems`, with their kinds; the other commands in `commands`, up to limit. The kinds have counts only. */
    def states_json(
      states: Iterator[State],
      doc: Line.Document,
      opts: Options,
      limit: Option[Int]
    ): JSON.Object.T = {
      val counts = mutable.Map[String, Int]().withDefaultValue(0)
      val returned_commands = new mutable.ListBuffer[JSON.Object.T]
      val returned_problems = new mutable.ListBuffer[JSON.Object.T]
      var total_timing_ms = 0L
      var count = 0
      for (state <- states) {
        count += 1
        total_timing_ms += state.timing_ms
        for (flag <- state.status) counts(flag) += 1
        counts("bad") += state.bad.length
        counts("errors") += state.errors
        counts("warnings") += state.warnings
        val problem_kinds = state.problem_kinds
        if (problem_kinds.nonEmpty) returned_problems += (state.json(doc, opts) + ("kinds" -> problem_kinds))
        else if (limit.forall(returned_commands.length < _)) returned_commands += state.json(doc, opts)
      }
      val kinds = List(Status.running, Status.warned, Status.failed, Status.canceled, "bad",
        "errors", "warnings", Status.unprocessed, Status.finished)
      Status.prefix_keys(JSON_Object("total_timing_ms" -> total_timing_ms) ++
        JSON_Object(kinds.map(kind => kind -> counts(kind)))) +
        ("problems" -> JSON_Object("count" -> returned_problems.length, "problems" -> returned_problems.toList)) +
        ("commands" -> JSON_Object("count" -> (count - returned_problems.length),
          "count_returned" -> returned_commands.length, "commands" -> returned_commands.toList))
    }

    sealed case class Options(
      include_types: Boolean = false,
      include_facts: Boolean = false,
      include_infos: Boolean = false,
      include_full_markup: Boolean = false,
      goal_max_lines: Int = default_goal_max_lines
    )
  }

  sealed case class State private(
    snapshot: Document.Snapshot,
    cmd: Command,
    range: Text.Range, // requested part of the command, for types, facts, and markup
    results: List[XML.Elem],
    command_range: Text.Range // the whole command, for status, messages, and source
  ) {
    lazy val cmd_status: Document_Status.Command_Status =
      PIDE_MCP_Command.status(snapshot, cmd)

    private def has_markup(elements: Markup.Elements): Boolean =
      snapshot.select(command_range, elements, _ => { case _ => Some(()) }).nonEmpty

    // interrupted at the step time limit (pide_mcp_step_time_limit), not once it runs again
    lazy val interrupted_at: Option[Double] =
      Option.when(!cmd_status.is_running && Running_Clock.canceled_ms(snapshot, cmd).nonEmpty)(())
        .flatMap(_ => PIDE_MCP_Step_Limit.interrupted_at(snapshot, cmd))
    // interrupted, but still running: seconds since the interrupt was sent
    lazy val interrupt_sent_seconds: Option[Long] =
      if (cmd_status.is_running) Running_Clock.interrupt_sent_ago(snapshot, cmd).map(_.ms / 1000) else None
    // running without the limit (retry_without_limit)
    lazy val no_time_limit: Boolean = PIDE_MCP_Step_Limit.is_unlimited(snapshot, cmd)

    lazy val status: List[String] = {
      val list = status_list(cmd_status).filter {
        case Status.warned => has_markup(Markup.Elements(Markup.WARNING, Markup.LEGACY))
        case Status.failed => has_markup(Markup.Elements(Markup.FAILED, Markup.ERROR))
        case _ => true
      }
      if (interrupted_at.nonEmpty && !list.contains(Status.canceled)) list :+ Status.canceled else list
    }
    lazy val problem_kinds: List[String] =
      status.filter(flag => flag != Status.unprocessed && flag != Status.finished) :::
        List(
          Option.when(interrupted_at.nonEmpty)("interrupted_at_limit"),
          Option.when(bad.nonEmpty)("bad"),
          Option.when(errors > 0)("errors"),
          Option.when(warnings > 0)("warnings")).flatten

    // the real run, also for a forked proof (which PIDE does not time) while it runs
    lazy val timing_ms: Long =
      cmd_status.timings.sum(Date.now()).ms max
        (if (cmd_status.is_running) Running_Clock.elapsed_ms(snapshot, cmd) else 0L)
    lazy val errors: Int = results.count(Protocol.is_error)
    lazy val warnings: Int = results.count(Protocol.is_warning_or_legacy)
    lazy val bad: List[JSON.Object.T] = bad_json(snapshot, command_range)

    def json(doc: Line.Document, opts: State.Options): JSON.Object.T = {
      val source_line = doc.position(command_range.start).line1
      val source = command_range.substring(snapshot.node.source).stripLineEnd
      val entries: List[Option[JSON.Object.Entry]] =
        List(
          Some("status" -> status),
          Some("timing_ms" -> timing_ms),
          interrupted_at.map("interrupted_at_limit_seconds" -> _),
          interrupted_at.map(_ => "hint" -> PIDE_MCP_Step_Limit.retry_note(source_line))) :::
        interrupted_at.toList.flatMap(_ =>
          PIDE_MCP_Step_Limit.interim(snapshot, cmd).toList.flatMap(_.interim_json(source_line, opts.goal_max_lines).toList.map(Some(_)))) :::
        List(
          interrupt_sent_seconds.map("interrupt_sent_seconds" -> _),
          Option.when(no_time_limit)("no_time_limit" -> true),
          Some("source" -> PIDE_MCP_Util.numbered_lines(source, source_line)),
          proper_list(bad).map("bad" -> _)) :::
        message_entries(results.iterator, Message_Kind.important, goal_max_lines = Some(opts.goal_max_lines)) :::
        List(
          Option.when(opts.include_types)(
            proper_list(types_json(snapshot, range)).map("types" -> _)).flatten,
          Option.when(opts.include_facts)(
            proper_list(facts_json(snapshot, range)).map("facts" -> _)).flatten) :::
        (if (opts.include_infos) message_entries(results.iterator, Message_Kind.info) else Nil) :::
        List(
          Option.when(opts.include_full_markup)(
            "markup" -> markup_json(snapshot, range, Markup.Elements.full)))
      JSON_Object.flatten(entries)
    }
  }

}
