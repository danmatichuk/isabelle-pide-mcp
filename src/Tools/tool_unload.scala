/*  Title:      PIDE_MCP/tool_unload.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Unload extends PIDE_MCP_Tool("unload") {
  def description: String = "Unload the given origins and their dependents."

  private val origins_arg = PIDE_MCP_Tool_Arg.strings_default(
    "origins",
    "Origins (session-qualified theory names or file paths) to unload.", Nil)
  private val origin_arg = PIDE_MCP_Tool_Arg.opt_string(
    "origin", "A single origin to unload (same as one entry of `origins`: both spellings are accepted).")

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, origins_arg, origin_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("destructiveHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.text {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      val origins = origins_arg.get(args) ::: origin_arg.get(args).toList
      if (origins.isEmpty) error("Missing origins (or origin): give the files or theories to unload")
      val node_names = origins.map(session.node_name)
      val unloaded = session.unload(node_names, progress)
      if (unloaded.nonEmpty) session.await_stable_snapshot(progress)
      JSON_Object("unloaded" -> unloaded.map(session.origin)) ++
        JSON_Object.if_proper(unloaded.isEmpty,
          JSON_Object("note" -> "nothing was loaded under these origins (already unloaded, or never loaded); a file on disk is only deleted for scratch files when the session stops"))
    }
}

class Tools_Unload extends PIDE_MCP_Tools(new Tool_Unload)
