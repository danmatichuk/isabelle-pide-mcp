/*  Title:      PIDE_MCP/tool_check_ml.scala

Tool checking ML text for syntax errors without a session and without running it.
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Check_ML extends PIDE_MCP_Tool("check_ml") {
  def description: String =
    "Check ML text for SYNTAX errors without running it and without a session or a reload (about 0.2 s): the text is compiled by Poly/ML but the compiled code is never run, " +
      "so nothing is declared and nothing happens. **Use it on an ML file before the theories that load it are reloaded** (a reload of a big file and everything that imports it takes minutes " +
      "and then only reports the error), e.g. after a scripted change; `edit` does it automatically for `.ML` files. " +
      "Finds errors like a trailing comma in a record type or a missing `end`; names and types are NOT checked (the earlier declarations of the text are not run), " +
      "and antiquotations are not looked at. Give `origin` (a file on disk) or `code`."

  private val origin_arg = PIDE_MCP_Tool_Arg.opt_string(
    "origin", "Absolute path of the ML file to check (its text on disk is checked).")
  private val code_arg = PIDE_MCP_Tool_Arg.opt_string(
    "code", "ML text to check instead of a file.")

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(List(origin_arg, code_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      val (name, text) = (origin_arg.get(args), code_arg.get(args)) match {
        case (_, Some(code)) => ("the given code", code)
        case (Some(origin), None) =>
          val path = PIDE_MCP_Util.path(origin)
          if (!path.is_file) error("No such file: " + origin)
          (path.base.implode, File.read(path))
        case _ => error("Give `origin` (a file) or `code`")
      }
      PIDE_MCP_Tool_Result.Text(PIDE_MCP_ML_Check.check(text) match {
        case Left(message) => "ML syntax check not possible: " + message
        case Right(result) => PIDE_MCP_ML_Check.describe(name, text, result)
      })
    }
}

class Tools_Check_ML extends PIDE_MCP_Tools(new Tool_Check_ML)
