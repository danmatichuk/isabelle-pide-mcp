/*  Title:      PIDE_MCP/pide_mcp_render.scala

Readable text rendering of tool results.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Render {
  enum Output_Format { case text, json }

  object Output_Format {
    val default: Output_Format = text
    val format = PIDE_MCP_Tool_Arg.Format(
      JSON_Object("type" -> "string", "enum" -> values.toList.map(_.toString)),
      { case mode: String => values.find(_.toString == mode); case _ => None },
      _.toString)
    val description: String =
      "Output format. `text` is a compact human-readable rendering, `json` is the structured JSON " +
        "(also returned as structured content)."
    def arg: PIDE_MCP_Tool_Arg[Output_Format] =
      PIDE_MCP_Tool_Arg.default("format", description, format, default)
  }

  // text result without structured content
  def result(format: Output_Format, json: JSON.T, text: JSON.T => String): JSON.T =
    format match {
      case Output_Format.text => PIDE_MCP_Tool_Result.Text(text(json))
      case Output_Format.json => json
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

  def text(json: JSON.T): String = cat_lines(lines(json))

  /* get_state */

  private def obj(x: Any): Map[String, Any] =
    x match { case JSON.Object(o) => o; case _ => Map.empty }

  private def list(x: Any): List[Any] =
    x match { case xs: List[?] => xs; case _ => Nil }

  private def str_list(x: Any): List[String] = list(x).map(scalar)

  private def location_line(loc: Map[String, Any]): String = {
    val range = loc.get("end_line") match {
      case Some(end) => s"${scalar(loc.getOrElse("line", "?"))}-${scalar(end)}"
      case None => scalar(loc.getOrElse("line", "?"))
    }
    val kinds = str_list(loc.getOrElse("kinds", Nil)).mkString(",")
    s"line $range  [$kinds]" + loc.get("first_message").fold("")(m => "  " + scalar(m))
  }

  private val entry_skip = Set("status", "timing_ms", "source", "kinds")

  private def command_lines(cmd: Map[String, Any]): List[String] = {
    val source = scalar(cmd.getOrElse("source", "")).linesIterator.toList
    val status = str_list(cmd.getOrElse("status", Nil)).mkString(",")
    val kinds = str_list(cmd.getOrElse("kinds", Nil)).mkString(",")
    val info = List(
      Option.when(status.nonEmpty)(status),
      Option.when(kinds.nonEmpty && kinds != status)(s"kinds: $kinds"),
      cmd.get("timing_ms").map(t => s"${scalar(t)} ms")).flatten.mkString("; ")
    val extra = cmd.toList.filterNot(p => entry_skip.contains(p._1)).flatMap {
      case ("bad", bad) =>
        list(bad).map { b =>
          val o = obj(b)
          "bad: " + scalar(o.getOrElse("source", "")) +
            o.get("message").map(scalar).filter(_.nonEmpty).fold("")(m => s" ($m)")
        }
      case (key, v) => entry(key, v)
    }
    source ::: indent(3, s"[$info]" :: extra)
  }

  private def is_full(cmd: Map[String, Any]): Boolean = cmd.contains("source")

  def get_state_text(json: JSON.T): String = {
    val state = obj(json)
    def count(key: String): Int = state.get(key).collect { case n: Int => n; case n: Long => n.toInt }.getOrElse(0)
    val kinds = List("still_running_possibly_nonterminating", "warned", "failed", "canceled",
      "bad", "unprocessed", "finished").map(k => k -> count("commands_" + k)) :::
      List("errors" -> count("errors"), "warnings" -> count("warnings"))
    val total = scalar(obj(state.getOrElse("commands", Map.empty)).getOrElse("count", 0))
    val header = s"$total commands" + s", ${scalar(state.getOrElse("total_timing_ms", 0))} ms" +
      ": " + kinds.filter(_._2 > 0).map { case (k, n) => s"$n $k" }.mkString(", ")
    val first = state.get("first_problem").map(p => List("first problem: " + location_line(obj(p))))
    def section(key: String, title: String): List[String] =
      state.get(key).map(obj).filter(_.contains(key)).toList.flatMap { sec =>
        val shown = list(sec(key)).map(obj)
        val total = scalar(sec.getOrElse("count", shown.length))
        "" :: s"$title (${shown.length} of $total):" ::
          shown.flatMap(c => if (is_full(c)) command_lines(c) else List(location_line(c)))
      }
    val synced = state.get("external_edits_synced").toList.flatMap { x =>
      val sync = obj(x)
      val files = list(sync.getOrElse("files", Nil)).map(obj).map { f =>
        s"${scalar(f.getOrElse("origin", "?"))} (${scalar(f.getOrElse("changed_lines", 0))} of " +
          s"${scalar(f.getOrElse("total_lines", 0))} lines in ${scalar(f.getOrElse("spans", 0))} spans; " +
          s"lines ${str_list(f.getOrElse("lines", Nil)).mkString(", ")})"
      }
      List("external edits synced: " + files.mkString(", ") +
        sync.get("diff_file").fold("") { f =>
          val path = scalar(f)
          s"\nfull diff: $path\nACTION REQUIRED: the user cannot see this tool output. In your next reply, include this link verbatim: " +
            s"[sync diff]($path). Do not read the file."
        })
    }
    cat_lines(header :: synced ::: first.getOrElse(Nil) ::: section("problems", "problems") :::
      section("commands", "commands"))
  }
}
