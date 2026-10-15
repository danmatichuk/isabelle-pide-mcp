/*  Title:      PIDE_MCP/pide_mcp_render.scala

Readable YAML-like rendering of tool results (pide_mcp_output_format); `format: json` returns the structured result.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Render {
  enum Output_Format { case yaml, json }

  object Output_Format {
    def default(options: Options): Output_Format = {
      val name = options.string("pide_mcp_output_format")
      values.find(_.toString == name).getOrElse(error(s"Bad pide_mcp_output_format: ${quote(name)}"))
    }
    private val format = PIDE_MCP_Tool_Arg.Format(
      JSON_Object("type" -> "string", "enum" -> values.toList.map(_.toString)),
      { case mode: String => values.find(_.toString == mode); case _ => None },
      _.toString)
    def arg(default: Output_Format): PIDE_MCP_Tool_Arg[Output_Format] =
      PIDE_MCP_Tool_Arg.default("format",
        "Result format: `yaml` (readable text) or `json` (structured content).", format, default)
  }

  // the tool's schema with the format argument
  def input_schema(schema: JSON.Object.T, default: Output_Format): JSON.Object.T =
    schema.get("properties") match {
      case Some(JSON.Object(props)) if !props.contains("format") =>
        schema + ("properties" -> (props + Output_Format.arg(default).entry))
      case _ => schema
    }

  def format(args: JSON.Object.T, default: Output_Format): Output_Format =
    Output_Format.arg(default).get(args)

  // plain strings (and errors) are returned as they are
  def result(format: Output_Format, result: PIDE_MCP_Tool_Result): PIDE_MCP_Tool_Result =
    result match {
      case Result.Res(json) if format == Output_Format.yaml && !json.isInstanceOf[String] =>
        Result.Res(PIDE_MCP_Tool_Result.Text(text(json)))
      case _ => result
    }

  private val inline_limit = 100

  private def is_scalar(x: Any): Boolean =
    x match {
      case s: String => !s.contains('\n')
      case _: Map[?, ?] | _: List[?] => false
      case _ => true
    }

  private def scalar(x: Any): String =
    x match {
      case s: String => s
      case _ => JSON.Format(x)
    }

  private def indent(n: Int, lines: List[String]): List[String] =
    lines.map(l => if (l.isEmpty) l else " " * n + l)

  // generic YAML-like rendering: no quoting, multi-line strings as indented blocks
  def lines(json: Any): List[String] =
    json match {
      case JSON.Object(obj) =>
        obj.toList.flatMap { case (k, v) => entry(k, v) }
      case xs: List[?] =>
        xs.flatMap { x =>
          val ls = lines(x)
          ls match {
            case Nil => List("-")
            case l :: rest => ("- " + l) :: indent(2, rest)
          }
        }
      case s: String => Line.logical_lines(s)
      case _ => List(scalar(json))
    }

  private def entry(key: String, value: Any): List[String] =
    value match {
      case List(single: String) => entry(key, single)
      case xs: List[?] if xs.isEmpty => List(s"$key: []")
      case xs: List[?] if xs.forall(is_scalar) && xs.map(scalar).mkString(", ").length <= inline_limit =>
        List(s"$key: ${xs.map(scalar).mkString(", ")}")
      case JSON.Object(obj) if obj.isEmpty => List(s"$key: {}")
      case _ if is_scalar(value) => List(s"$key: ${scalar(value)}")
      case _ => s"$key:" :: indent(2, lines(value))
    }

  def text(json: JSON.T): String = {
    val result = cat_lines(lines(json))
    if (result.isEmpty) "(empty)" else result
  }
}
