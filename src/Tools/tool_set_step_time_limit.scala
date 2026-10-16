/*  Title:      PIDE_MCP/tool_set_step_time_limit.scala

Change the time limit for proof steps of a running session (pide_mcp_step_time_limit), without a restart.
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Set_Step_Time_Limit extends PIDE_MCP_Tool("set_step_time_limit") {
  def description: String =
    "Change the time limit for proof steps (`apply`, `by`, `proof`, `qed`, `done`, `.`, `..`; default 5000 ms) of a " +
      "running session from now on, for all files: a step whose real run reaches it is interrupted and shown as " +
      "`canceled`. Running steps are checked against the new limit at once. Steps interrupted at a lower limit run " +
      "again under the new one. For one step only, use `retry_without_limit`."

  private val milliseconds_arg = PIDE_MCP_Tool_Arg.int(
    "milliseconds", "The new limit in milliseconds (0: no limit).", minimum = Some(0))

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, milliseconds_arg))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      val ms = milliseconds_arg.get(args)
      val (old, rerun) = session.step_limit.set_limit(ms / 1000.0, progress)
      val old_ms = (old * 1000).round
      // 0: no limit
      val raised = old_ms > 0 && (ms == 0 || ms > old_ms)
      val rerun_json = rerun.map { case (name, cmd, start) =>
        val snapshot = session.node_snapshot(name)
        JSON_Object("origin" -> name.node, "line" -> Line.Document(snapshot.node.source).position(start).line1,
          "command" -> cmd.span.name)
      }
      JSON_Object("old_milliseconds" -> old_ms, "milliseconds" -> ms) ++
        JSON_Object.if_proper(rerun_json.nonEmpty, JSON_Object("rerun" -> rerun_json)) ++
        JSON_Object("note" ->
          ((if (ms == 0) "no limit from now on" else s"steps are interrupted at $ms ms from now on") +
            (if (rerun_json.nonEmpty)
              "; the steps interrupted at the lower limit run again (in each theory the first one, which re-runs the commands after it): check them with `get_state`"
            else if (raised) "; no step was interrupted at the lower limit"
            else "; running steps past it are interrupted now; steps interrupted before stay interrupted: " +
              "an edit of a step or `retry_without_limit` runs it again")))
    }
}

class Tools_Set_Step_Time_Limit extends PIDE_MCP_Tools(new Tool_Set_Step_Time_Limit)
