/*  Title:      PIDE_MCP/pide_mcp_tool_schema.scala
    Author:     Kevin Kappelmann

Shared JSON-Schema fragments for MCP tools.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Tool_Schema {
  val implicit_load_file: String = "Loads the file if needed."
  val implicit_reload_file: String = "Loads the file if needed."
  val sync_disk_edits: String = "Picks up edits made on disk."
  val range_visibility: String = "Sets the PIDE perspective on the range."
  val session_restart: String =
    "Only when required (unresponsive session, changed ROOT file, other base session)."

  val running_session_arg = PIDE_MCP_Tool_Arg.opt_string(
    "session", "Session id (optional if only one runs).")
  val running_sessions_arg = PIDE_MCP_Tool_Arg.opt_strings(
    "sessions", "Session ids (default: all).")
  val origin_arg = PIDE_MCP_Tool_Arg.string(
    "origin",
    s"File path or session-qualified theory name (e.g. ${quote("HOL.Nat")}).")
  val start_line_arg = PIDE_MCP_Tool_Arg.int(
    "start_line", "First line to include.", minimum = Some(1))
  val opt_start_line_arg = PIDE_MCP_Tool_Arg.opt_int(
    "start_line", "First line to include.",
    schema_default = Some(PIDE_MCP_Tool_Util.default_start_line), minimum = Some(1))
  val opt_end_line_arg = PIDE_MCP_Tool_Arg.opt_int(
    "end_line", "Last line to include (default: end of file).", minimum = Some(1))

  def input_schema(args: List[PIDE_MCP_Tool_Arg[?]]): JSON.Object.T =
    JSON_Object("type" -> "object", "properties" -> JSON_Object(args.map(_.entry))) ++
      JSON.optional("required" -> proper_list(args.filter(_.required).map(_.name)))
}
