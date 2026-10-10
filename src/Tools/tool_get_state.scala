/*  Title:      PIDE_MCP/tool_get_state.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Get_State extends PIDE_MCP_Tool("get_state") {
  def description: String =
    "Inspect the state of a (range in a) file: goals, variables, errors, warnings, etc. " +
      "Returns a summary (errors, warnings, commands still running, total timing,...) and details for all commands in range. " +
      "**Use this frequently to check if you are making progress, what is left to be done, and importantly, if certain commands are still processing, potentially even looping.** " +
      "For non-theory files, the response has a single command entry derived from the command that loaded it in the respective theory. " +
      PIDE_MCP_Tool_Schema.implicit_load_file + " " +
      PIDE_MCP_Tool_Schema.sync_disk_edits

  private val include_types_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_types",
    "Include types (Isabelle and ML) for variables and constants.", false)
  private val include_facts_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_facts",
    "Include facts/theorems used in range.", false)
  private val include_infos_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_infos",
    "Include writeln and information output in the state (`full` detail only). This can get large, but often contains useful information. Avoid using it for large ranges if possible.",
    false)
  private val include_full_markup_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_full_markup",
    "Include full PIDE markup information. This gets very large - **use only sparingly and very targeted to get local details**.",
    false)
  private val commands_limit_arg = PIDE_MCP_Tool_Arg.opt_int(
    "commands_limit",
    "Maximum number of commands to return in the plain commands list (`full` detail only). Omit to return all commands. " +
      "Set to 0 to omit the plain commands list; the problems are still listed according to `detail` (use `detail: summary` for counts only). " +
      "Note that the returned summary will count all commands, even the truncated ones. " +
      "Warning: for large ranges, there are thousands of commands that may flood your context. Set the commands limit to 0 if you do not need details.",
    minimum = Some(0))

  private val detail_arg = PIDE_MCP_Tool_Arg.optional(
    "detail",
    "How much to return. " +
      "`summary`: only counts (running, failed, bad, errors,...), total timing and the first failing command. " +
      "`locations`: additionally one compact entry per problematic command (line range, problem kinds, first message line, no source). " +
      "`full`: full command details (source, all messages) for problematic commands and for the plain commands list. " +
      "Each problematic command appears once, with its problem kinds listed. " +
      "Default: `full` if `commands_limit` is positive, `locations` otherwise (in particular for `commands_limit` 0). " +
      "`commands_limit` never hides the messages of listed problems; only `detail` and `problems_limit` do.",
    PIDE_MCP_Command.State.Detail.format)
  private val problems_limit_arg = PIDE_MCP_Tool_Arg.int_default(
    "problems_limit",
    "Maximum number of problematic commands to return (in file order). The summary still counts all of them. " +
      "The first failing command, usually the cause of the following failures, is always reported.",
    PIDE_MCP_Command.State.default_problems_limit, minimum = Some(0))
  private val source_max_lines_arg = PIDE_MCP_Tool_Arg.int_default(
    "source_max_lines",
    "Maximum number of source lines shown per command (`full` detail only); longer commands are cut with a marker. " +
      "You know the file and the line numbers, so the first lines suffice to identify a command.",
    PIDE_MCP_Command.State.default_source_max_lines, minimum = Some(1))

  private val include_ml_echo_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_ml_echo",
    "With `include_infos`: keep the echo of top-level ML declarations (`val x = ...`, `structure ...`, etc.). " +
      "Set to false to see only explicit output (`writeln`, ...). Detected heuristically by the message's first words.",
    true)
  private val info_max_lines_arg = PIDE_MCP_Tool_Arg.int_default(
    "info_max_lines",
    "Maximum number of lines per writeln/information/tracing message with `include_infos`; longer ones are cut with a marker. " +
      "ML top-level bindings echo their values, which can be very long.",
    PIDE_MCP_Command.State.default_info_max_lines, minimum = Some(1))

  private val format_arg = PIDE_MCP_Render.Output_Format.arg
  private val wait_seconds_arg = PIDE_MCP_Tool_Arg.int_default(
    "wait_seconds",
    "If the file is not loaded or not processed yet, wait up to this many seconds for it instead of failing right away. " +
      "This also waits while the file only waits for imported theories that are still being processed (e.g. after editing an imported file); " +
      "the result then lists them as `waiting_for`. " +
      "Use this after the first request for a file to save a round trip.",
    0, minimum = Some(0), maximum = Some(600))

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, PIDE_MCP_Tool_Schema.origin_arg,
      PIDE_MCP_Tool_Schema.opt_start_line_arg, PIDE_MCP_Tool_Schema.opt_end_line_arg,
      include_types_arg, include_facts_arg, include_infos_arg, include_full_markup_arg,
      commands_limit_arg, detail_arg, problems_limit_arg, source_max_lines_arg, info_max_lines_arg, include_ml_echo_arg,
      wait_seconds_arg, format_arg))

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
      val wait_seconds = wait_seconds_arg.get(args)
      val loaded_snapshot =
        PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress, wait_seconds)
      val (snapshot, waiting) = PIDE_MCP_Tool_Util.await_upstream(
        session, node_name, loaded_snapshot, wait_seconds, progress)
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
      PIDE_MCP_Render.result(format_arg.get(args),
        PIDE_MCP_Tool_Util.with_waiting(
          PIDE_MCP_Tool_Util.with_synced(
            PIDE_MCP_Command.State.states_json(
              states, doc, opts, detail, commands_limit, problems_limit_arg.get(args)),
            synced),
          waiting),
        PIDE_MCP_Render.get_state_text)
    }
}

class Tools_Get_State extends PIDE_MCP_Tools(new Tool_Get_State)
