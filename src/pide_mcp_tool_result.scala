/*  Title:      PIDE_MCP/pide_mcp_tool_result.scala
    Author:     Kevin Kappelmann

Results of MCP tool calls.
*/

package isabelle.pide.mcp

import isabelle._

type PIDE_MCP_Tool_Result = Result[JSON.T, JSON.T]

object PIDE_MCP_Tool_Result {
  export Result.{Res, Error}

  // rendered text: returned as content only, without structured content
  sealed case class Text(text: String)

  def exn_error(exn: Throwable): PIDE_MCP_Tool_Result = Result.Error(Exn.message(exn))

  // readable text as content only, without structuredContent
  def text(body: => JSON.T): PIDE_MCP_Tool_Result =
    result(Text(PIDE_MCP_Render.text(body)))

  def result(body: => JSON.T): PIDE_MCP_Tool_Result =
    Exn.result(body) match {
      case Exn.Res(value) => Result.Res(value)
      case Exn.Exn(exn) => exn_error(exn)
    }
}
