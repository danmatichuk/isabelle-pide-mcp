/*  Title:      PIDE_MCP/tool_apply_inspect.scala

Diagnostic run of a proof method with apply_inspect, without changing the file: the goal state that the method has
reached after a given time.
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Apply_Inspect extends PIDE_MCP_Tool("apply_inspect") {
  def description: String =
    "Run a proof method with apply_inspect on the goal before the command at `start_line`, for at most `timeout_ms`: " +
      "returns the goal state that the method had reached at the timeout (within a simp call, the term it had rewritten to), the theorems it had " +
      "used, and loop warnings of the simplifier or classical reasoner. **Use it to see what a slow or looping method " +
      "does** (e.g. a step interrupted at the time limit for proof steps), with different `timeout_ms` for different " +
      "moments of its run. Nothing is applied and the file is not changed. Needs a session based on HOL. " +
      PIDE_MCP_Tool_Schema.implicit_load_file

  private val start_line_arg = PIDE_MCP_Tool_Arg.int(
    "start_line", "Line of the proof step to inspect: its goal (the state before it) is used.", minimum = Some(1))
  private val method_arg = PIDE_MCP_Tool_Arg.opt_string(
    "method",
    "The proof method as after `apply`, e.g. `auto`, `(simp add: foo_def)`, `(rule conjI, simp)` " +
      "(default: the method of the step at `start_line`; `by m1 m2` runs `(m1, m2)`).")
  private val timeout_ms_arg = PIDE_MCP_Tool_Arg.int_default(
    "timeout_ms", "apply_inspect's timeout: stop the method after this many milliseconds and return the goal state reached then.",
    200, minimum = Some(1), maximum = Some(60000))
  private val goal_max_lines_arg = PIDE_MCP_Tool_Arg.int_default(
    "goal_max_lines", "Lines shown of the goal state (a blown-up goal is cut with a marker; also caps characters). 0: no cap.", PIDE_MCP_Step_Limit.goal_max_lines,
    minimum = Some(0))
  private val max_uses_arg = PIDE_MCP_Tool_Arg.int_default(
    "max_uses", "Maximum number of used theorems to list.", 30, minimum = Some(0))

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, PIDE_MCP_Tool_Schema.origin_arg, start_line_arg, method_arg,
      timeout_ms_arg, goal_max_lines_arg, max_uses_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      val node_name = PIDE_MCP_Tool_Util.origin_param(session, args)
      if (!node_name.is_theory) error("Not a theory")
      val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress, 10)
      val doc = Line.Document(snapshot.node.source)
      val line = start_line_arg.get(args)
      if (line > doc.lines.length) error(s"Bad start_line $line (the file has ${doc.lines.length} lines)")
      val cmd =
        PIDE_MCP_Util.text_range(doc, line, line).toList
          .flatMap(range => PIDE_MCP_Command.iterator(snapshot, Some(range)))
          .map(_._1).find(!_.is_ignored)
          .getOrElse(error(s"No command at line $line"))
      val method = method_arg.get(args).map(_.trim).filter(_.nonEmpty)
        .orElse(PIDE_MCP_Step_Limit.step_method(cmd))
        .getOrElse(error(s"`${cmd.span.name}` at line $line has no method of its own: give `method`"))
      if (!session.step_limit.helper_loaded)
        error("apply_inspect is not available: it needs a session based on HOL, and its helper theory " +
          PIDE_MCP_Step_Limit.helper + " must load")
      val timeout_ms = timeout_ms_arg.get(args)
      val result = PIDE_MCP_Step_Limit.inspect(session, cmd, method, Some(timeout_ms.toLong),
        Time.seconds(timeout_ms / 1000.0 + 60), progress = progress)
      if (result.outcome == "unavailable") error("apply_inspect: " + result.error)
      val max_uses = max_uses_arg.get(args)
      val shown_uses = result.uses.take(max_uses)
      val line1 = snapshot.node.command_start(cmd).fold(line)(o => doc.position(o).line1)
      JSON_Object("line" -> line1, "command" -> cmd.span.name, "method" -> method, "outcome" -> result.outcome) ++
        (result.outcome match {
          case "traced" => JSON_Object("time_ms" -> result.time_ms)  // when the run ended, shortly after the timeout
          case "finished" => JSON_Object("note" -> s"the method finished within $timeout_ms ms (no intermediate state)")
          case _ => JSON_Object.if_proper(result.error.nonEmpty,
            JSON_Object("error" -> PIDE_MCP_Command.cap_goal(result.error, goal_max_lines_arg.get(args), "error")))
        }) ++
        JSON_Object.if_proper(result.loops.nonEmpty, JSON_Object("loops" -> result.loops)) ++
        JSON_Object("theorems_used" ->
          (shown_uses ::: (if (result.uses.length > shown_uses.length)
            List(s"... (+${result.uses.length - shown_uses.length} more; max_uses)") else Nil))) ++
        JSON_Object.if_proper(result.outcome == "traced",
          JSON_Object("goal" -> PIDE_MCP_Command.cap_goal(result.goal, goal_max_lines_arg.get(args))))
    }
}

class Tools_Apply_Inspect extends PIDE_MCP_Tools(new Tool_Apply_Inspect)
