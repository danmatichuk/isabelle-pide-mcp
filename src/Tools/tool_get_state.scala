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
      PIDE_MCP_Tool_Schema.sync_disk_edits + " " +
      s"After such a sync it waits up to ${Tool_Get_State.sync_wait_seconds.toInt} s for the changed lines to be processed, " +
      s"and up to ${Tool_Get_State.load_wait_seconds.toInt} s for a file that was not loaded yet."

  private val include_types_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_types",
    "Include types (Isabelle and ML) for variables and constants.", false)
  private val include_facts_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_facts",
    "Include facts/theorems used in range.", false)
  private val include_infos_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_infos",
    "Include writeln and information output in the state. This can get large, but often contains useful information. Avoid using it for large ranges if possible.",
    false)
  private val include_full_markup_arg = PIDE_MCP_Tool_Arg.bool_default(
    "include_full_markup",
    "Include full PIDE markup information. This gets very large - **use only sparingly and very targeted to get local details**.",
    false)
  private val commands_limit_arg = PIDE_MCP_Tool_Arg.opt_int(
    "commands_limit",
    "Maximum number of unproblematic commands to return in `commands`. Omit to return all commands. " +
      "Problematic commands (running, warned, failed, canceled, bad, with errors or warnings) are always listed once in `problems`, with their `kinds`. " +
      "**Set to 0 whenever you only want to get summary information (e.g. to see if there are any errors, warnings, etc.).** " +
      "Note that the returned summary will count all commands, even the truncated ones. " +
      "Warning: for large ranges, there are thousands of commands that may flood your context. Set the commands limit to 0 if you do not need details.",
    minimum = Some(0))
  private val goal_max_lines_arg = PIDE_MCP_Tool_Arg.int_default(
    "goal_max_lines",
    "Lines shown per goal, interim goal, error, and warning (a goal blown up by a method is cut with a marker; also caps characters, 200 per line). 0: no cap.",
    PIDE_MCP_Command.State.default_goal_max_lines, minimum = Some(0))

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, PIDE_MCP_Tool_Schema.origin_arg,
      PIDE_MCP_Tool_Schema.opt_start_line_arg, PIDE_MCP_Tool_Schema.opt_end_line_arg,
      include_types_arg, include_facts_arg, include_infos_arg, include_full_markup_arg,
      commands_limit_arg, goal_max_lines_arg))

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
      val opt_start_line = PIDE_MCP_Tool_Schema.opt_start_line_arg.get(args)
      val opt_end_line = PIDE_MCP_Tool_Schema.opt_end_line_arg.get(args)
      // a file that is not loaded yet (e.g. just written by the agent's own tools) is loaded and waited for
      val loaded = Exn.result(session.node_snapshot(node_name)) match {
        case Exn.Res(_) => true
        case Exn.Exn(exn) if Exn.is_interrupt(exn) => throw exn
        case Exn.Exn(_) => false
      }
      val load_start = Time.now()
      PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress,
        wait_seconds = if (loaded) 0 else Tool_Get_State.load_wait_seconds)
      // the results of the old text are gone, the new text is not processed yet; a new file is not processed yet
      if (!loaded || synced.exists(_._1.contains(node_name))) {
        val seconds =
          if (loaded) Tool_Get_State.sync_wait_seconds
          else (Tool_Get_State.load_wait_seconds - (Time.now() - load_start).seconds) max 0.0
        PIDE_MCP_Tool_Util.await_processed(session, node_name, opt_start_line.getOrElse(1),
          opt_end_line.getOrElse(Int.MaxValue), seconds, progress)
      }
      val snapshot = session.node_snapshot(node_name)
      val doc = Line.Document(snapshot.node.source)
      val (start_line, end_line) =
        PIDE_MCP_Tool_Util.resolve_lines(opt_start_line, opt_end_line, doc.lines.length)
      val opts = PIDE_MCP_Command.State.Options(
        include_types_arg.get(args), include_facts_arg.get(args),
        include_infos_arg.get(args), include_full_markup_arg.get(args), goal_max_lines_arg.get(args))
      val commands_limit = commands_limit_arg.get(args)
      val range = PIDE_MCP_Util.text_range(doc, start_line, end_line).get
      val states = PIDE_MCP_Command.State.states(snapshot, Some(range))
      PIDE_MCP_Tool_Util.with_synced(
        PIDE_MCP_Command.State.states_json(states, doc, opts, commands_limit), synced)
    }
}

object Tool_Get_State {
  val sync_wait_seconds: Double = 5.0
  val load_wait_seconds: Double = 10.0
}

class Tools_Get_State extends PIDE_MCP_Tools(new Tool_Get_State)
