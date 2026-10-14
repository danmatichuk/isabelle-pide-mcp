/*  Title:      PIDE_MCP/tool_edit.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

import scala.util.matching.Regex

object Tool_Edit {
  enum Edit_Mode { case replace, prepend, append }

  object Edit_Mode {
    val default: Edit_Mode = replace
    val names: List[String] = values.toList.map(_.toString)
    val format = PIDE_MCP_Tool_Arg.Format(
      JSON_Object("type" -> "string", "enum" -> names),
      json => JSON.Value.String.unapply(json).flatMap(mode =>
        values.find(_.toString == mode)), _.toString)
  }

  def apply_edit(
    mode: Edit_Mode,
    full_text: String,
    offset: Int,
    old_length: Int,
    new_text: String
  ): String = {
    mode match {
      case Edit_Mode.replace =>
        full_text.slice(0, offset) + new_text + full_text.slice(offset + old_length, full_text.length)
      case Edit_Mode.prepend =>
        full_text.slice(0, offset) + new_text + full_text.slice(offset, full_text.length)
      case Edit_Mode.append =>
        full_text.slice(0, offset + old_length) + new_text + full_text.slice(offset + old_length, full_text.length)
    }
  }

  def edited_text_ranges(
    mode: Edit_Mode,
    offsets: List[Text.Offset],
    old_length: Int,
    new_length: Int
  ): List[Text.Range] = {
    val shift = if (mode == Edit_Mode.replace) new_length - old_length else new_length
    val insert_offset = if (mode == Edit_Mode.append) old_length else 0
    offsets.zipWithIndex.map { case (offset, i) =>
      val start = offset + i * shift + insert_offset
      Text.Range(start, start + new_length)
    }
  }

  // One edit as a replacement of [start, stop) of the (symbol-decoded) old text by `text`.
  def edit_replacements(
    mode: Edit_Mode,
    offsets: List[Text.Offset],
    old_length: Int,
    new_text: String
  ): List[(Text.Offset, Text.Offset, String)] =
    offsets.map { offset =>
      mode match {
        case Edit_Mode.replace => (offset, offset + old_length, new_text)
        case Edit_Mode.prepend => (offset, offset, new_text)
        case Edit_Mode.append => (offset + old_length, offset + old_length, new_text)
      }
    }

  /* New file content: lines that no edit touches keep their raw text from disk (byte for byte,
     e.g. literal Unicode symbols are not turned into \<...>); only lines touched by an edit are
     rebuilt and symbol-encoded. `raw` and `decoded` are the old file text (line-normalised)
     before and after symbol decoding, `replacements` refer to `decoded`. */
  def merge_into_raw(
    raw: String,
    decoded: String,
    replacements: List[(Text.Offset, Text.Offset, String)]
  ): String = {
    val raw_lines = raw.split("\n", -1)
    val line_starts = 0 :: decoded.indices.filter(decoded(_) == '\n').map(_ + 1).toList
    if (raw_lines.length != line_starts.length) Symbol.encode(apply_replacements(decoded, replacements))
    else {
      def line_of(offset: Text.Offset): Int = line_starts.lastIndexWhere(_ <= offset)
      def line_end(line: Int): Int =
        if (line + 1 < line_starts.length) line_starts(line + 1) - 1 else decoded.length
      // group the edits by the (merged) range of old lines that they touch
      val sorted = replacements.sortBy(_._1)
      val groups = sorted.foldLeft(List.empty[(Int, Int, List[(Text.Offset, Text.Offset, String)])]) {
        case ((first, last, rs) :: rest, r) if line_of(r._1) <= last =>
          (first, last max line_of(r._2), r :: rs) :: rest
        case (acc, r) => (line_of(r._1), line_of(r._2), List(r)) :: acc
      }.reverse
      val out = new StringBuilder
      var next = 0
      for ((first, last, rs) <- groups) {
        for (i <- next until first) out ++= raw_lines(i) += '\n'
        val base = line_starts(first)
        val segment = decoded.substring(base, line_end(last))
        val shifted = rs.map { case (start, stop, text) => (start - base, stop - base, text) }
        out ++= Symbol.encode(apply_replacements(segment, shifted))
        if (last + 1 < line_starts.length) out += '\n'
        next = last + 1
      }
      for (i <- next until raw_lines.length) { out ++= raw_lines(i); if (i + 1 < raw_lines.length) out += '\n' }
      out.toString
    }
  }

  private def apply_replacements(text: String, replacements: List[(Text.Offset, Text.Offset, String)]): String =
    replacements.sortBy(_._1).reverseIterator.foldLeft(text) { case (t, (start, stop, new_text)) =>
      t.substring(0, start) + new_text + t.substring(stop) }

  def read_update_edit(
    session: PIDE_MCP_Session,
    mode: Edit_Mode,
    node_name: Document.Node.Name,
    new_text: String,
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    old_text: String,
    edit_all: Boolean,
    progress: Progress,
    ml_check: Boolean = false
  ): (String, Int, Option[(Int, Int)], Option[String], Option[String]) = {
    if (session.is_base_session_theory(node_name))
      error(s"Cannot edit base session theory ${quote(session.origin(node_name))}")
    PIDE_MCP_Tool_Util.require_ordered_lines(opt_start_line, opt_end_line)
    session.with_lock(progress) {
      val current_text = session.read_update(
        List(node_name -> Nil), hide_others = false, progress = progress)(node_name)
      val doc = Line.Document(current_text)
      val (start_line, end_line) =
        PIDE_MCP_Tool_Util.resolve_lines(opt_start_line, opt_end_line, doc.lines.length)
      val edit_range = PIDE_MCP_Util.text_range(doc, start_line, end_line).get
      val range_text = edit_range.substring(current_text)
      val actual_old_text = if (old_text.isEmpty) range_text else old_text
      val offsets = if (old_text.isEmpty) List(edit_range.start)
        else {
          val occurrences = new Regex(Regex.quote(actual_old_text))
            .findAllMatchIn(range_text).map(_.start + edit_range.start).toList
          if (occurrences.isEmpty)
            error("old_text not found in the given range (symbols are matched in Unicode form: `\\<rightarrow>` " +
              "and `→` are both accepted for the same text; check line breaks and leading whitespace)")
          if (!edit_all && occurrences.length > 1)
            error(s"Found ${occurrences.length} occurrences of old_text in the given range. Expected exactly 1. " +
              "Provide more context (larger old_text), restrict the range, or use edit_all.")
          if (edit_all) occurrences else List(occurrences.head)
        }
      val computed_text = offsets.reverseIterator.foldLeft(current_text) { (text, offset) =>
        apply_edit(mode, text, offset, actual_old_text.length, new_text)
      }
      val changed = computed_text != current_text
      val edited_doc = Line.Document(computed_text)
      val visible_lines =
        edited_text_ranges(mode, offsets, actual_old_text.length, new_text.length).map { range =>
          val line_range = edited_doc.range(range)
          (line_range.start.line1, Some(line_range.stop.line1))
        }
      progress.expose_interrupt()
      val new_raw =
        if (changed) Some(merge_into_raw(Line.normalize(File.read(node_name.path)), current_text,
          edit_replacements(mode, offsets, actual_old_text.length, new_text)))
        else None
      // an ML file with a syntax error is not written: the reload of the file and all its dependents (minutes) would only report it
      val ml_result =
        for {
          raw <- new_raw if ml_check && !node_name.is_theory && node_name.path.implode.endsWith(".ML")
          result <- PIDE_MCP_ML_Check.check(raw).toOption
        } yield (result, PIDE_MCP_ML_Check.describe(node_name.path.base.implode, raw, result))
      val ml_blocked = ml_result.filter(_._1.errors.nonEmpty).map(_._2)
      // a written file that was only partly judged says so (a clean pre-check must not read as "checked")
      val ml_note = ml_result.filter(r => r._1.errors.isEmpty && r._1.unjudged > 0).map(r => PIDE_MCP_ML_Check.unjudged_note(r._1.unjudged))
      if (ml_blocked.nonEmpty) (current_text, 0, None, ml_blocked, None)
      else {
      new_raw.foreach(session.write_file_text(node_name.path, _))
      val text = session.read_update(
        List(node_name -> visible_lines), hide_others = true,
        // avoid disk <--> PIDE disagreement
        progress = new Uncancellable_Progress(progress))(node_name)
      session.await_stable_snapshot(progress)
      session.resolve_dependencies(List(node_name), progress)
      (text, if (changed) offsets.length else 0,
        Option.when(visible_lines.nonEmpty)((visible_lines.map(_._1).min, visible_lines.flatMap(_._2).max)), None, ml_note)
      }
    }
  }
}

class Tool_Edit extends PIDE_MCP_Tool("edit") {
  def description: String =
    "Replace, prepend to or append to `old_text` in a line range of a file, and return as soon as the text is written; then " +
      "call `get_state`. `reprocessed` estimates the cost: the theories and commands processed again and their previous time. " +
      "Edits to one file one after the other. Lines outside the edit keep their disk text; `old_text` matches symbols in " +
      "either form (`\\<rightarrow>` = `→`). Base session files cannot be edited."

  private val mode_arg = PIDE_MCP_Tool_Arg.default(
    "mode", "Edit mode",
    Tool_Edit.Edit_Mode.format, Tool_Edit.Edit_Mode.default)
  private val text_arg = PIDE_MCP_Tool_Arg.string("text", "New text to write")
  private val old_text_arg = PIDE_MCP_Tool_Arg.string(
    "old_text",
    "Text to find in the range; empty: the whole range.")
  private val edit_all_arg = PIDE_MCP_Tool_Arg.bool_default(
    "edit_all",
    "Edit every match or just a unique occurrence.", false)

  private val check_wait_arg = PIDE_MCP_Tool_Arg.int_default(
    "check_wait_seconds",
    "With `check`: how long to wait for the file to be processed.",
    30, minimum = Some(0), maximum = Some(600))
  private val check_ml_arg = PIDE_MCP_Tool_Arg.bool_default(
    "check_ml",
    "For an `.ML` file: do not write text with an ML syntax error (as `check_ml`; avoids a reload just to see the error). " +
      "false: write anyway.",
    true)
  private val cancel_after_arg = PIDE_MCP_Tool_Arg.int_default(
    "cancel_after_seconds",
    "Time limit: interrupt (as `cancel`) any edited command still running after this many seconds. 0: none.",
    0, minimum = Some(0), maximum = Some(3600))
  private val check_arg = PIDE_MCP_Tool_Arg.bool_default(
    "check",
    "Opt-in: wait for processing and return the file's status (blocks while slow commands run; prefer `get_state`).",
    false)

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, PIDE_MCP_Tool_Schema.origin_arg, mode_arg,
      text_arg, PIDE_MCP_Tool_Schema.opt_start_line_arg,
      PIDE_MCP_Tool_Schema.opt_end_line_arg, old_text_arg, edit_all_arg, check_arg, check_wait_arg, check_ml_arg,
      cancel_after_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("destructiveHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.text {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      val node_name = PIDE_MCP_Tool_Util.origin_param(session, args)
      val mode = mode_arg.get(args)
      val text = Line.normalize(text_arg.get(args))
      val old_text = Symbol.decode(Line.normalize(old_text_arg.get(args)))
      val edit_all = edit_all_arg.get(args)
      val opt_start_line = PIDE_MCP_Tool_Schema.opt_start_line_arg.get(args)
      val opt_end_line = PIDE_MCP_Tool_Schema.opt_end_line_arg.get(args)
      val reloads = PIDE_MCP_Tool_Util.reload_estimate(session, node_name)
      val (_, count, edited_range, ml_blocked, ml_note) = Tool_Edit.read_update_edit(
        session, mode, node_name, text, opt_start_line, opt_end_line, old_text,
        edit_all = edit_all, progress = progress, ml_check = check_ml_arg.get(args))
      val (status, description) =
        if (ml_blocked.nonEmpty) ("not written", "the new text has an ML syntax error: the file was NOT written and nothing reloads. " +
          ml_blocked.get + "\nFix the text and edit again (check_ml: false writes it anyway).")
        else if (count > 0) ("written", s"Edited $count occurrence(s)" + ml_note.fold("")(n => s". ML syntax pre-check found no error, but $n"))
        else ("unchanged", "Unchanged - did you replace the text by itself?")
      val cancel_after = cancel_after_arg.get(args)
      val time_limit =
        if (cancel_after > 0 && count > 0 && ml_blocked.isEmpty && node_name.is_theory) edited_range else None
      for ((from, to) <- time_limit) Tool_Cancel.cancel_after(session, node_name, from, to, cancel_after)
      val checked =
        if (check_arg.get(args)) check_state(session, node_name, check_wait_arg.get(args), progress, edited_range) else None
      JSON_Object("status" -> status, "description" -> description) ++
        JSON.optional("time_limit" -> time_limit.map { case (from, to) =>
          s"commands of lines $from-$to that run longer than $cancel_after s are cancelled" }) ++
        JSON.optional("provisional" -> checked.flatMap(_._1)) ++
        (if (count > 0) JSON_Object("reprocessed" -> reloads.fold(
          reason => JSON_Object("unavailable" -> reason), identity)) else JSON_Object()) ++
        JSON.optional("state" -> checked.map(_._2))
    }

  private def check_state(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    wait_seconds: Int,
    progress: Progress,
    edited_range: Option[(Int, Int)]
  ): Option[(Option[String], JSON.T)] =
    Exn.result {
      val (snapshot, waiting) =
        if (wait_seconds > 0)
          PIDE_MCP_Tool_Util.await_settled(session, node_name, session.node_snapshot(node_name), wait_seconds, progress)
        else (session.node_snapshot(node_name), Nil)
      val doc = Line.Document(snapshot.node.source)
      val range = PIDE_MCP_Util.text_range(doc, 1, doc.lines.length).get
      val state = PIDE_MCP_Command.State.states_json(
        PIDE_MCP_Command.State.states(snapshot, Some(range)), doc, PIDE_MCP_Command.State.Options(),
        PIDE_MCP_Command.State.Detail.summary, Some(0), 10)
      val (provisional_text, blocked_text) =
        PIDE_MCP_Tool_Util.settle_notes(session, snapshot, node_name, doc, state, waiting)
      // what the edited commands themselves report: problems and warnings (also an open goal's warnings), and output
      val edited = edited_range.flatMap { case (from, to) =>
        val range = PIDE_MCP_Util.text_range(doc, from, to min doc.lines.length).get
        val states = PIDE_MCP_Command.State.states(snapshot, Some(range)).toList
        val locations = PIDE_MCP_Command.State.states_json(
          states.iterator, doc, PIDE_MCP_Command.State.Options(),
          PIDE_MCP_Command.State.Detail.locations, Some(0), 10)
        val problems = locations.get("problems").collect { case JSON.Object(o) => o.get("problems") }.flatten
        // the messages of the edited commands in full, so that no second get_state is needed: at most 40 lines per message and 120 in all
        var echoes = 0
        val candidates = states.zipWithIndex.flatMap { case (s, i) =>
          val kinds = List("error", "warning", "writeln", "information")
          // (the "theorem name: statement" echo of a finished lemma is no output worth a line)
          // and neither is the echo of top-level ML declarations (`val thy = ...` prints the whole theory value)
          val all = s.results.filterNot(r => Protocol.is_writeln(r) && PIDE_MCP_Util.elem_body_plain_text(r).startsWith("theorem "))
          val results = all.filterNot(PIDE_MCP_Command.is_ml_echo)
          echoes += all.length - results.length
          // (a message is cut at 2000 characters too: a long value can sit on one line)
          def cut(m: String): String = if (m.length <= 2000) m else m.take(2000) + s"... (+${m.length - 2000} characters)"
          val entries = PIDE_MCP_Command.message_entries(results.iterator, kinds, Some(40)).flatten.map {
            case (k, l: List[_]) => (k, l.map(m => cut(m.toString)))
            case e => e
          }
          val size = entries.map(_._2 match { case l: List[_] => l.map(m => m.toString.linesIterator.length).sum; case _ => 1 }).sum
          Option.when(entries.nonEmpty)((i, size, entries.exists(e => e._1 == "error" || e._1 == "warning"),
            JSON_Object(("line" -> doc.position(s.command_range.start).line1) :: entries: _*)))
        }
        // errors and warnings take the budget first, then the output, in the order of the file
        var budget = 120
        val chosen = (candidates.filter(_._3) ::: candidates.filterNot(_._3)).filter { c =>
          val fits = c._2 <= budget
          if (fits) budget -= c._2
          fits
        }
        val details = chosen.sortBy(_._1).map(_._4)
        val omitted = candidates.length - chosen.length
        val entries = List(
          problems.map("problems" -> _),
          Option.when(details.nonEmpty)("messages" -> details),
          Option.when(omitted > 0)("output" ->
            s"$omitted command(s) of the edited range have more messages than shown here: get_state with include_infos, lines $from-$to"),
          Option.when(echoes > 0)("ml_echo_omitted" ->
            s"$echoes echo(es) of top-level ML declarations (val x = ...: int) left out: get_state with include_infos, lines $from-$to shows them")).flatten
        Option.when(entries.nonEmpty)(JSON_Object(("lines" -> s"$from-$to") :: entries: _*))
      }
      val hints = PIDE_MCP_Render.message_hints(JSON.Format(state))
      val full_state: JSON.T = state ++ JSON.optional("edited_range" -> edited) ++ JSON.optional("blocked" -> blocked_text) ++
        JSON_Object.if_proper(hints.nonEmpty, JSON_Object("hints" -> hints))
      (provisional_text, full_state)
    } match {
      case Exn.Res(result) => Some(result)
      case Exn.Exn(_) => Some((None, "not available yet; use get_state"))
    }
}

class Tools_Edit extends PIDE_MCP_Tools(new Tool_Edit)
