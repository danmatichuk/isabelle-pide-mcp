/*  Title:      PIDE_MCP/tool_get_state.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Get_State {
  // (hash of the last text, number of identical results in a row) per session, file and range
  val repeats = Synchronized(Map.empty[String, (Int, Int)])
  val repeat_note_after: Int = 3
  // wait after external edits were synced, if no wait_seconds is given
  val implicit_wait_seconds: Int = 5
  // wait after a theory with a failed header was loaded again, if no wait_seconds is given
  val reload_wait_seconds: Int = 30
  // wait_settled without wait_seconds
  val settled_default_seconds: Int = 60
}

class Tool_Get_State extends PIDE_MCP_Tool("get_state") {
  def description: String =
    "State of a file or line range: status counts, problems (failed, errors, `bad` = sorry, warnings, still running with " +
      "their running time), goals and messages. Use it after every edit, without waiting; narrow `start_line`/`end_line` to the " +
      "commands you work on. An ML file shows as the one command that loads it. Loads the file if needed and picks up edits made on disk."

  private val include_types_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_types",
    "Include types (Isabelle and ML) for variables and constants.", false)
  private val include_facts_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_facts",
    "Include facts/theorems used in range.", false)
  private val include_infos_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_infos",
    "Include writeln/information output (`full` detail only; large: use narrow ranges).",
    false)
  private val include_full_markup_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_full_markup",
    "Include full PIDE markup (very large: narrow ranges only).",
    false)
  private val commands_limit_arg = PIDE_MCP_Tool_Arg.opt_int(
    "commands_limit",
    "Maximum number of commands in the plain commands list; a positive value implies `detail: full`, 0 omits the list " +
      "(problems are still listed). Counts always cover all commands.",
    minimum = Some(0))

  private val detail_arg = PIDE_MCP_Tool_Arg.optional(
    "detail",
    "`summary`: counts and the first problem. `locations` (default): one line per problem (lines, kinds, first message). " +
      "`full`: sources and all messages (use it on a line range to read a cut message or a goal).",
    PIDE_MCP_Command.State.Detail.format)
  private val problems_limit_arg = PIDE_MCP_Tool_Arg.int_default(
    "problems_limit",
    "Maximum number of problems listed (in file order); the first failure, usually the cause of later ones, is always reported.",
    PIDE_MCP_Command.State.default_problems_limit, minimum = Some(0))
  private val source_max_lines_arg = PIDE_MCP_Tool_Arg.int_default(
    "source_max_lines",
    "Source lines shown per command (`full` detail).",
    PIDE_MCP_Command.State.default_source_max_lines, minimum = Some(1))

  private val include_ml_echo_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_ml_echo",
    "With `include_infos`: keep the echo of top-level ML declarations (`val x = ...`); false: explicit output only.",
    true)
  private val info_max_lines_arg = PIDE_MCP_Tool_Arg.int_default(
    "info_max_lines",
    "Lines per message with `include_infos`.",
    PIDE_MCP_Command.State.default_info_max_lines, minimum = Some(1))

  private val max_output_lines_arg = PIDE_MCP_Tool_Arg.int_default(
    "max_output_lines",
    "Hard cap on the lines of the text result. 0: no cap.",
    400, minimum = Some(0))
  private val format_arg = PIDE_MCP_Render.Output_Format.arg
  private val wait_seconds_arg = PIDE_MCP_Tool_Arg.int_default(
    "wait_seconds",
    "For a file that is not loaded or processed yet (or waits for its imports, listed as `waiting_for`): wait up to this many " +
      "seconds instead of returning at once. Not needed after an edit. After syncing disk edits it waits up to " +
      Tool_Get_State.implicit_wait_seconds + " s anyway.",
    0, minimum = Some(0), maximum = Some(3600))
  private val wait_settled_arg = PIDE_MCP_Tool_Arg.bool_default(
    "wait_settled",
    "Wait (up to `wait_seconds`, default 60) until the file and its imports are completely processed or a command fails. " +
      "Not needed after an edit.",
    false)
  private val stop_at_error_arg = PIDE_MCP_Tool_Arg.bool_default(
    "stop_at_error",
    "With `wait_settled`: stop at the next failing command.",
    false)

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, PIDE_MCP_Tool_Schema.origin_arg,
      PIDE_MCP_Tool_Schema.opt_start_line_arg, PIDE_MCP_Tool_Schema.opt_end_line_arg,
      include_types_arg, include_facts_arg, include_infos_arg, include_full_markup_arg,
      commands_limit_arg, detail_arg, problems_limit_arg, source_max_lines_arg, info_max_lines_arg, include_ml_echo_arg,
      wait_seconds_arg, wait_settled_arg, stop_at_error_arg, max_output_lines_arg, format_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      val node_name = PIDE_MCP_Tool_Util.origin_param(session, args)
      val synced = PIDE_MCP_Tool_Util.sync_disk_edits(session, progress)
      // after a disk edit was synced, the result of the old text is gone and the new text is not processed
      // yet: wait a bit for it by default
      val reloaded = PIDE_MCP_Tool_Util.reload_failed_header(session, node_name, progress)
      val settled_requested = wait_settled_arg.get(args)
      val implicit_wait = synced.nonEmpty && wait_seconds_arg.get(args) == 0
      val wait_seconds =
        if (wait_seconds_arg.get(args) > 0) wait_seconds_arg.get(args)
        else if (reloaded) Tool_Get_State.reload_wait_seconds  // the file is being loaded again
        else if (settled_requested) Tool_Get_State.settled_default_seconds
        else if (implicit_wait) Tool_Get_State.implicit_wait_seconds
        else 0
      val loaded_snapshot =
        PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress, wait_seconds)
      val (upstream_snapshot, upstream_waiting) = PIDE_MCP_Tool_Util.await_upstream(
        session, node_name, loaded_snapshot, wait_seconds, progress)
      val (snapshot, waiting) =
        if ((settled_requested || implicit_wait) && wait_seconds > 0)
          PIDE_MCP_Tool_Util.await_settled(session, node_name, upstream_snapshot, wait_seconds, progress,
            stop_at_error_arg.get(args))
        else (upstream_snapshot, upstream_waiting)
      val opt_start_line = PIDE_MCP_Tool_Schema.opt_start_line_arg.get(args)
      val opt_end_line = PIDE_MCP_Tool_Schema.opt_end_line_arg.get(args)
      val doc = Line.Document(snapshot.node.source)
      val (start_line, end_line) =
        PIDE_MCP_Tool_Util.resolve_lines(opt_start_line, opt_end_line, doc.lines.length)
      val opts = PIDE_MCP_Command.State.Options(
        include_types_arg.get(args), include_facts_arg.get(args),
        include_infos_arg.get(args), include_full_markup_arg.get(args),
        source_max_lines_arg.get(args), info_max_lines_arg.get(args), include_ml_echo_arg.get(args))
      val commands_limit = commands_limit_arg.get(args)
      val range = PIDE_MCP_Util.text_range(doc, start_line, end_line).get
      val states = PIDE_MCP_Command.State.states(snapshot, Some(range))
      val detail = detail_arg.get(args).getOrElse(commands_limit match {
        case Some(limit) if limit > 0 => PIDE_MCP_Command.State.Detail.full
        case _ => PIDE_MCP_Command.State.Detail.locations
      })
      val state_json = PIDE_MCP_Command.State.states_json(
        states, doc, opts, detail, commands_limit, problems_limit_arg.get(args))
      val infos_note = Option.when(include_infos_arg.get(args) && detail != PIDE_MCP_Command.State.Detail.full)(
        "include_infos has no effect without detail: full; use detail: full with a line range")
      val earlier = PIDE_MCP_Tool_Util.earlier_problems_note(snapshot, doc, start_line)
      val (provisional, blocked) =
        PIDE_MCP_Tool_Util.settle_notes(session, snapshot, node_name, doc, state_json, waiting)
      // an import that failed and does not run is not "still being processed": the BLOCKED text says what it is
      val waiting_shown = if (blocked.exists(_.contains("waits for its import"))) Nil else waiting
      val rendered = PIDE_MCP_Render.result(format_arg.get(args),
        PIDE_MCP_Tool_Util.with_waiting(
          PIDE_MCP_Tool_Util.with_synced(state_json, synced), waiting_shown) ++
          JSON.optional("provisional" -> provisional) ++ JSON.optional("blocked" -> blocked) ++
          JSON.optional("note" -> infos_note) ++
          JSON.optional("earlier_problems" -> earlier),
        json => PIDE_MCP_Render.get_state_text(json, max_output_lines_arg.get(args)))
      rendered match {
        case PIDE_MCP_Tool_Result.Text(text) =>
          // the same answer again and again: say that waiting or repeating changes nothing
          val key = session.id + "|" + node_name.node + "|" + opt_start_line + "|" + opt_end_line
          val repeats = Tool_Get_State.repeats.change_result { table =>
            val n = table.get(key).filter(_._1 == text.hashCode).map(_._2 + 1).getOrElse(1)
            (n, table + (key -> (text.hashCode, n)))
          }
          if (repeats < Tool_Get_State.repeat_note_after) rendered
          else PIDE_MCP_Tool_Result.Text(text +
            s"\nnote: this result is identical to the previous ${repeats - 1} call(s) for this file and range: waiting or repeating it " +
            "changes nothing. Read the first problem, fix it (or change the file), then call again")
        case other => other
      }
    }
}

class Tools_Get_State extends PIDE_MCP_Tools(new Tool_Get_State)
