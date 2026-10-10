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
    kinds: List[String]
  ): List[Option[JSON.Object.Entry]] = {
    val texts = classify_results(elements)
    kinds.map(kind => texts.get(kind).map(kind -> _))
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
      iterator(snapshot, range).map { case (cmd, command_start) =>
        val restricted = PIDE_MCP_Util.intersect_range(cmd.range + command_start, range)
        State(snapshot, cmd, restricted,
          results(snapshot, cmd, command_start, restricted).toList)
      }

    def command_spans(
      snapshot: Document.Snapshot,
      theory_cmd: Command,
      range: Option[Text.Range]
    ): Iterator[State] =
      snapshot.command_spans(PIDE_MCP_Util.restrict_text_range(snapshot.node.source, range))
        .iterator.map { span =>
          val restricted = PIDE_MCP_Util.intersect_range(span.range, range)
          State(snapshot, theory_cmd, restricted,
            Rendering.text_messages(snapshot, restricted).map(_.info))
        }

    def blob(snapshot: Document.Snapshot, range: Option[Text.Range]): Option[State] =
      snapshot.commands_loading.headOption.map { cmd =>
        val restricted = PIDE_MCP_Util.restrict_text_range(snapshot.node.source, range)
        State(snapshot, cmd, restricted, results(snapshot, cmd, 0, restricted).toList)
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

    val default_source_max_lines: Int = 5
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
        val problem_kinds = state.problem_kinds
        if (problem_kinds.nonEmpty) {
          problems += 1
          if (first_problem.isEmpty && problem_kinds.exists(Set(Status.failed, "errors")))
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
        JSON.optional("first_problem" -> first_problem) +
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
      source_max_lines: Int = default_source_max_lines
    )
  }

  sealed case class State private(
    snapshot: Document.Snapshot,
    cmd: Command,
    range: Text.Range,
    results: List[XML.Elem]
  ) {
    lazy val cmd_status: Document_Status.Command_Status =
      PIDE_MCP_Command.status(snapshot, cmd)

    private def has_markup(elements: Markup.Elements): Boolean =
      snapshot.select(range, elements, _ => { case _ => Some(()) }).nonEmpty

    lazy val status: List[String] =
      status_list(cmd_status).filter {
        case Status.warned => has_markup(Markup.Elements(Markup.WARNING, Markup.LEGACY))
        case Status.failed => has_markup(Markup.Elements(Markup.FAILED, Markup.ERROR))
        case _ => true
      }
    lazy val timing_ms: Long = cmd_status.timings.sum(Date.now()).ms
    lazy val errors: Int = results.count(Protocol.is_error)
    lazy val warnings: Int = results.count(Protocol.is_warning_or_legacy)
    lazy val bad: List[JSON.Object.T] = bad_json(snapshot, range)

    // the problem kinds of this command: relevant status flags, bad, errors, warnings
    lazy val problem_kinds: List[String] =
      status.filter(flag => flag != Status.unprocessed && flag != Status.finished) :::
        List(
          Option.when(bad.nonEmpty)("bad"),
          Option.when(errors > 0)("errors"),
          Option.when(warnings > 0)("warnings")).flatten

    private def source_text: String = range.substring(snapshot.node.source).stripLineEnd

    private def first_message(max_length: Int = 200): Option[String] = {
      def texts(pred: XML.Elem => Boolean) =
        results.iterator.filter(pred).map(PIDE_MCP_Util.elem_body_plain_text)
      texts(Protocol.is_error).nextOption()
        .orElse(texts(Protocol.is_warning_or_legacy).nextOption())
        .orElse(bad.headOption.flatMap(_.get("message")).collect { case s: String => s })
        .map(_.linesIterator.nextOption().getOrElse("").trim)
        .map(line => if (line.length > max_length) line.take(max_length) + "..." else line)
    }

    // compact entry without source text
    def location_json(doc: Line.Document, kinds: List[String]): JSON.Object.T = {
      val line = doc.position(range.start).line1
      val end_line = line + Line.logical_lines(source_text).length - 1
      JSON_Object.flatten(List(
        Some("line" -> line),
        Option.when(end_line > line)("end_line" -> end_line),
        Some("kinds" -> kinds),
        first_message().filter(_.nonEmpty).map("first_message" -> _)))
    }

    def json(doc: Line.Document, opts: State.Options): JSON.Object.T = {
      val source_line = doc.position(range.start).line1
      val source_lines = Line.logical_lines(source_text)
      val source =
        if (source_lines.length > opts.source_max_lines)
          PIDE_MCP_Util.numbered_lines(source_lines.take(opts.source_max_lines), source_line) +
            s"\n... (+${source_lines.length - opts.source_max_lines} lines)"
        else PIDE_MCP_Util.numbered_lines(source_lines, source_line)
      val entries: List[Option[JSON.Object.Entry]] =
        List(
          Some("status" -> status),
          Some("timing_ms" -> timing_ms),
          Some("source" -> source),
          proper_list(bad).map("bad" -> _)) :::
        message_entries(results.iterator, Message_Kind.important) :::
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
