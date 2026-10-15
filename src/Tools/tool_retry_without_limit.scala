/*  Title:      PIDE_MCP/tool_retry_without_limit.scala

Run a proof step again that was interrupted at the time limit for proof steps, without the limit.
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Retry_Without_Limit extends PIDE_MCP_Tool("retry_without_limit") {
  def description: String =
    "Run a proof step again that was interrupted at the time limit for proof steps (shown as `canceled`, with " +
      "`interrupted_at_limit_seconds`), without the limit, once: the command at `start_line` runs until it finishes. " +
      "An edit of the command stops it and brings the limit back."

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, PIDE_MCP_Tool_Schema.origin_arg,
      PIDE_MCP_Tool_Schema.start_line_arg))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      val node_name = PIDE_MCP_Tool_Util.origin_param(session, args)
      if (!node_name.is_theory) error("Not a theory")
      val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress)
      val doc = Line.Document(snapshot.node.source)
      val line = PIDE_MCP_Tool_Schema.start_line_arg.get(args)
      val found = PIDE_MCP_Util.text_range(doc, line, line min doc.lines.length).toList.flatMap(range =>
        PIDE_MCP_Command.iterator(snapshot, Some(range)).toList)
        .filter { case (cmd, _) => PIDE_MCP_Command.Running_Clock.canceled_ms(snapshot, cmd).nonEmpty }
      found.headOption match {
        case None =>
          error(s"No command interrupted at the time limit at line $line: `get_state` shows them as `canceled`")
        case Some((cmd, start)) =>
          PIDE_MCP_Step_Limit.retries.change(PIDE_MCP_Step_Limit.Retry(cmd.id, node_name, start,
            cmd.source, Time.now()) :: _)
          session.reevaluate(node_name, Text.Range(start, start + cmd.length), progress)
          JSON_Object("retrying" -> JSON_Object("line" -> doc.position(start).line1, "command" -> cmd.span.name),
            "note" -> ("it runs without the time limit until it finishes; `get_state` shows it; " +
              "an edit of it stops it and brings the limit back"))
      }
    }
}

class Tools_Retry_Without_Limit extends PIDE_MCP_Tools(new Tool_Retry_Without_Limit)
