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

  def text(json: JSON.T): String = {
    val result = cat_lines(lines(json))
    if (result.isEmpty) "(empty)" else result
  }

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

  // hints for typical error messages, found in the (JSON) text of a result
  def message_hints(json_format: String): List[String] = {
    val json_text = json_format.replace("\\\"", "\"")  // messages with their quotes unescaped
    val import_hint = """Bad theory import "([^".]+)"""".r.findFirstMatchIn(json_text).map(m =>
      "theory imports are qualified by their session (e.g. `Box.Box`, not `" + m.group(1) +
        "`): `get_session_state` lists the sessions and their directories")
    val interrupt_hint = Option.when(json_text.contains("Interrupt"))(
      "an Interrupt means a time limit was hit or the command was cancelled (e.g. by a newer edit), not a logic error: " +
        "retry once; if it repeats, the proof method is too slow: give it fewer rules or use `sledgehammer`")
    import_hint.toList ::: interrupt_hint.toList
  }

  // A goal or message block (4 lines or more, from its `goal (` line on) that appears again in the same reply is shown once;
  // the later place refers to the first. Exact text matches only; the structured result is not touched.
  private val dedupe_min_lines = 4

  def dedupe_blocks(lines: List[String]): List[String] = {
    val seen = scala.collection.mutable.Map.empty[List[String], String]
    val header = """^(\d+: .*)$""".r
    val label = """^   ([a-z_]+):$""".r
    val out = new scala.collection.mutable.ListBuffer[String]
    var where = ""
    var section = ""
    var rest = lines
    while (rest.nonEmpty) {
      val line = rest.head
      line match {
        case header(h) => where = s"$section above, command ${h.take(60)}"; out += line; rest = rest.tail
        case l if l.startsWith("problems (") || l.startsWith("commands (") => section = l.takeWhile(_ != ' '); out += line; rest = rest.tail
        case label(_) =>
          val body = rest.tail.takeWhile(l => l.startsWith("     ") || l.isEmpty && false)
          val start = body.indexWhere(_.trim.startsWith("goal ("))
          val key = body.map(_.drop(5)).drop(if (start >= 0) start else 0)
          out += line
          if (key.length >= dedupe_min_lines && seen.contains(key)) {
            out ++= body.take(if (start >= 0) start else 0)
            out += "     (same text as in " + seen(key) + ")"
          }
          else {
            if (key.length >= dedupe_min_lines) seen(key) = where
            out ++= body
          }
          rest = rest.drop(1 + body.length)
        case _ => out += line; rest = rest.tail
      }
    }
    out.toList
  }

  def get_state_text(json: JSON.T, max_lines: Int = 0): String = {
    val state = obj(json)
    def count(key: String): Int = state.get(key).collect { case n: Int => n; case n: Long => n.toInt }.getOrElse(0)
    val kinds = List("still_running_possibly_nonterminating", "warned", "failed", "canceled",
      "bad", "unprocessed", "finished").map(k => k -> count("commands_" + k)) :::
      List("errors" -> count("errors"), "warnings" -> count("warnings"))
    val total = scalar(obj(state.getOrElse("commands", Map.empty)).getOrElse("count", 0))
    val header = s"$total commands" + s", ${scalar(state.getOrElse("total_timing_ms", 0))} ms" +
      ": " + kinds.filter(_._2 > 0).map { case (k, n) => s"$n $k" }.mkString(", ")
    val provisional = state.get("provisional").map(p => List("*** " + scalar(p) + " ***")).orElse(
      state.get("blocked").map(b => List("*** " + scalar(b) + " ***")))
    val note = state.get("note").map(n => List("note: " + scalar(n))).toList.flatten :::
      state.get("earlier_problems").map(n => "*** earlier problems: " + scalar(n) + " ***").toList
    val note_lines = note
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
          s"lines ${str_list(f.getOrElse("lines", Nil)).mkString(", ")})" +
          f.get("reprocessed").collect { case JSON.Object(r) =>
            s"; reprocesses ${scalar(r.getOrElse("theories", 0))} theories, ${scalar(r.getOrElse("commands", 0))} commands" }.getOrElse("")
      }
      val ml_errors = list(sync.getOrElse("files", Nil)).map(obj).flatMap(_.get("ml_syntax_error")).map(scalar)
      List("external edits synced: " + files.mkString(", ") +
        " [tip: `edit` with check:true applies a change, waits and checks it in one call]" +
        ml_errors.map("\n*** " + _ + " ***").mkString +
        sync.get("diff_file").fold("") { f =>
          val path = scalar(f)
          s"\nfull diff: $path\nACTION REQUIRED: the user cannot see this tool output. In your next reply, include this link verbatim: " +
            s"[sync diff]($path). Do not read the file."
        })
    }
    val waiting = list(state.getOrElse("waiting_for", Nil)).map(obj).map(w =>
      s"${scalar(w.getOrElse("origin", "?"))} (${scalar(w.getOrElse("percentage_commands_processed", 0))}%)")
    val waiting_line = Option.when(waiting.nonEmpty)(
      "waiting for imported theories still being processed: " + waiting.mkString(", "))
    val hint = Option.when(count("commands_failed") + count("errors") > 0)(
      "hint: before guessing another method for a failing proof, run `try0`, then `sledgehammer [timeout = 10]`; " +
      "use `find_theorems` / `suggest_facts` for lemmas, and a scratch theory to test term shapes. " +
      "In a `Failed to apply proof method` error the goal is the state BEFORE that method (it failed as a whole); " +
      "`Failed to finish proof` shows the state where the proof got stuck, after its methods ran. " +
      "Commands after a failed command of the same proof run on the state before it: their `finished` is no evidence. " +
      "A failed or sorry'd LEMMA is still usable by later commands, which then also show `finished`: everything after the first problem may rest on it. " +
      "If a method fails without a reason, see \"Basic idioms and traps\" in the isabelle-proof-development skill " +
      "(missing class on a type variable, unknown identifier = free variable, open proof, looping simp)")
    val message_hint_lines = message_hints(JSON.Format(json)).map("hint: " + _)
    val main_lines = header :: provisional.getOrElse(Nil) ::: note_lines ::: waiting_line.toList ::: synced ::: first.getOrElse(Nil) :::
      dedupe_blocks(section("problems", "problems") ::: section("commands", "commands"))
    val hint_lines = hint.toList ::: message_hint_lines
    val long_output_lines = 150
    if (max_lines > 0 && main_lines.length > max_lines)  // hard cap: keep the start, say what was cut
      cat_lines(main_lines.take(max_lines) :::
        List(s"... (output cut after $max_lines of ${main_lines.length} lines: narrow start_line/end_line, use detail: summary or locations, " +
          "or raise max_output_lines)") ::: hint_lines)
    else if (main_lines.length + hint_lines.length <= long_output_lines) cat_lines(main_lines ::: hint_lines)
    else cat_lines(main_lines ::: hint_lines :::
      List(s"hint: long output (${main_lines.length + hint_lines.length} lines): for an overview use detail: summary or locations, " +
        "and with include_infos restrict start_line/end_line to the commands you need"))
  }
}
