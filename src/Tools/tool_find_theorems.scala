/*  Title:      PIDE_MCP/tool_find_theorems.scala

Tools running Isabelle's find_theorems and find_consts as PIDE queries, without modifying any file.
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Find {
  val default_timeout_seconds: Int = 30
  val load_wait_seconds: Int = 30

  private val start_line_arg = PIDE_MCP_Tool_Arg.opt_int(
    "start_line",
    "Position: the context (and goal) after this line. Default: end of the file (everything it defines, no goal).",
    minimum = Some(1))
  private val prefix_arg = PIDE_MCP_Tool_Arg.opt_string(
    "prefix",
    "Run after this prefix of the line instead of its end.")
  private val timeout_arg = PIDE_MCP_Tool_Arg.int_default(
    "timeout", "Seconds before the query is cancelled.", default_timeout_seconds,
    minimum = Some(1))
  private val format_arg = PIDE_MCP_Render.Output_Format.arg

  val common_args: List[PIDE_MCP_Tool_Arg[?]] =
    List(PIDE_MCP_Tool_Schema.running_session_arg, PIDE_MCP_Tool_Schema.origin_arg,
      start_line_arg, prefix_arg, timeout_arg)

  def run(
    session: PIDE_MCP_Session,
    args: JSON.Object.T,
    operation: String,
    query_args: List[String],
    progress: Progress,
    refine: String => String = identity  // post-processing of the text rendering
  ): JSON.T = {
    val node_name = PIDE_MCP_Tool_Util.origin_param(session, args)
    // a file that is not loaded yet is loaded (and waited for) instead of failing the first query
    val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress,
      Tool_Find.load_wait_seconds)
    val default_note =
      if (start_line_arg.get(args).isEmpty) PIDE_MCP_Tool_Util.default_query_position(snapshot)._2 else None
    val offset = PIDE_MCP_Tool_Util.query_offset(
      snapshot, start_line_arg.get(args), prefix_arg.get(args))
    val result = new PIDE_MCP_Query(session, PIDE_MCP_Editor.Context(node_name, offset), operation, progress)
      .run(query_args, Some(Time.seconds(timeout_arg.get(args))))
    val default_line = Option.when(start_line_arg.get(args).isEmpty)(
      Line.Document(snapshot.node.source).position(offset).line1)
    val note =
      (if (result.outcome == PIDE_MCP_Query.Outcome.timeout)
        "\n(query cancelled after the timeout: results may be incomplete)" else "") +
      default_line.fold("")(line => s"\n(searched at line $line, the default: end of file" +
        default_note.fold("")("; " + _) + "; pass start_line to search elsewhere)") +
      (if (result.text.contains("Unknown context"))
        "\n(Unknown context: the position has no theory context, e.g. it is after the theory's `end` or its command failed: " +
          "pass a start_line inside the theory, at a processed command)" else "")
    val empty_hint =
      if (result.text.isEmpty || result.text.contains("found nothing")) "\n(an entity of a theory that this file does not import is not found: grep the " +
        "session directories listed by get_session_state, or import the theory)" else ""
    PIDE_MCP_Render.result(format_arg.get(args),
      result.json(PIDE_MCP_Command.Message_Kind.all) ++ JSON.optional("searched_at_line" -> default_line) ++
        JSON.optional("searched_at_note" -> default_note),
      _ => (if (result.text.isEmpty) "(no results)" else refine(result.text)) + note + empty_hint)
  }

  val format: PIDE_MCP_Tool_Arg[PIDE_MCP_Render.Output_Format] = format_arg

  // A locale's facts are stated again for the locale's parameters (`class.foo ?plus ?minus ... ==> ...`, 20 schematic
  // parameters): where another result has the same last name component, they are left out and listed in one line.
  def collapse_locale_duplicates(text: String): String = {
    val lines = split_lines(text)
    val start = lines.indexWhere(_.startsWith("found ")) + 1
    if (start == 0) text
    else {
      val (head, body) = lines.splitAt(start)
      val entries = body.foldLeft(List.empty[List[String]]) {
        case (acc, l) if l.startsWith("  ") && !l.startsWith("   ") => List(l) :: acc
        case (x :: acc, l) if l.startsWith("   ") => (x :+ l) :: acc
        case (acc, l) => List(l) :: acc
      }.reverse
      def name(e: List[String]): String = e.head.trim.takeWhile(_ != ':')
      def last(n: String): String = n.substring(n.lastIndexOf('.') + 1)
      def statement(e: List[String]): String = (e.head.trim.dropWhile(_ != ':').drop(1) :: e.tail).map(_.trim).find(_.nonEmpty).getOrElse("")
      val lasts = entries.map(e => last(name(e)))
      val hidden = entries.zipWithIndex.filter { case (e, i) =>
        statement(e).startsWith("class.") && lasts.zipWithIndex.exists { case (l, j) => j != i && l == lasts(i) } }.map(_._1)
      if (hidden.isEmpty) text
      else {
        val by_prefix = hidden.map(name).groupBy(n => n.take(n.lastIndexOf('.'))).toList.sortBy(_._1).map {
          case (prefix, names) => prefix + ": " + names.map(last).sorted.mkString(", ") }
        cat_lines(head ::: entries.filterNot(hidden.contains).flatten :::
          List(s"(${hidden.length} more result(s) not shown: restatements for a locale's parameters, statement starting with `class.`, " +
            "of results shown above with the same name; pass locale_duplicates: true to list them. " + by_prefix.mkString("; ") + ")"))
      }
    }
  }
}

class Tool_Find_Theorems extends PIDE_MCP_Tool("find_theorems") {
  def description: String =
    "Isabelle's `find_theorems` at a position of a file, without modifying it: **use it instead of grepping sources or guessing lemma names.** " +
      "Criteria: a pattern in quotes (**schematic `?x` or `_` for variables, not free names**), " +
      "`name: <ident>`, `intro`, `elim`, `dest`, `solves`, `simp: \"<pattern>\"`, each negatable with `-`. " +
      "Examples: `\"rev (rev ?xs) = ?xs\"`, `name: rev \"?xs @ _\"`, `-name: foo \"_ + _ = _ + _\"`. " +
      "**Quote names that contain dots: `name: \"List.rev\"`**; to cut generated facts of a datatype, negate them: " +
      "`name: foo -name: \"foo.rel_\" -name: \"foo.set_\"`. " +
      PIDE_MCP_Tool_Schema.implicit_load_file

  private val query_arg = PIDE_MCP_Tool_Arg.string(
    "query", "Search criteria, as after `find_theorems` in a theory (without the limit).")
  private val limit_arg = PIDE_MCP_Tool_Arg.int_default(
    "limit", "Maximum number of theorems to return.", 20, minimum = Some(1))
  private val allow_dups_arg = PIDE_MCP_Tool_Arg.bool_default(
    "allow_dups", "Also return theorems with identical statements.", false)

  private val locale_dups_arg = PIDE_MCP_Tool_Arg.bool_default(
    "locale_duplicates", "Also list the restatements of a result for the parameters of a locale (statement starting with `class.`), which are left out by default.", false)

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    Tool_Find.common_args ::: List(query_arg, limit_arg, allow_dups_arg, locale_dups_arg, Tool_Find.format))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      Tool_Find.run(session, args, "find_theorems",
        List(limit_arg.get(args).toString, allow_dups_arg.get(args).toString, query_arg.get(args)),
        progress, refine = if (locale_dups_arg.get(args)) identity else Tool_Find.collapse_locale_duplicates)
    }
}

class Tools_Find_Theorems extends PIDE_MCP_Tools(new Tool_Find_Theorems)

class Tool_Find_Consts extends PIDE_MCP_Tool("find_consts") {
  def description: String =
    "Isabelle's `find_consts` at a position of a file, without modifying it (instead of grepping sources). " +
      "Criteria: a type pattern in quotes (e.g. `\"_ list => nat\"`), " +
      "`name: <ident>`, each negatable with `-`. " +
      PIDE_MCP_Tool_Schema.implicit_load_file

  private val query_arg = PIDE_MCP_Tool_Arg.string(
    "query", "Search criteria, as after `find_consts` in a theory.")

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    Tool_Find.common_args ::: List(query_arg, Tool_Find.format))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      Tool_Find.run(session, args, "find_consts", List(query_arg.get(args)), progress)
    }
}

class Tools_Find_Consts extends PIDE_MCP_Tools(new Tool_Find_Consts)
