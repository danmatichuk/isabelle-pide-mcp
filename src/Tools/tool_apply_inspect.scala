/*  Title:      PIDE_MCP/tool_apply_inspect.scala

Tool running a proof method on the goal at a position and reporting the theorems it used, without modifying any file.
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Apply_Inspect {
  val operation: String = "pide_mcp_apply_inspect"
  val file_dir: String = ".pide_mcp/apply_inspect"  // relative to the server's working directory: project-local
  val file_keep: Int = 100

  // the complete list in a file of its own, for grep; the newest file_keep files are kept
  def write_file(session_id: String, text: String): String = {
    val dir = Path.explode(file_dir).absolute
    Isabelle_System.make_directory(dir)
    val ignore = dir + Path.basic(".gitignore")  // keep the files out of the project's git status
    if (!ignore.is_file) File.write(ignore, "*\n")
    val stamp = Date.Format("uuuuMMdd-HHmmss-SSS")(Date.now())
    val file = dir + Path.basic(s"apply_inspect_${session_id}_$stamp.txt")
    File.write(file, text)
    Option(dir.file.listFiles).toList.flatten.filter(_.getName.endsWith(".txt"))
      .sortBy(_.getName).dropRight(file_keep).foreach(_.delete())
    File.platform_path(file)
  }
}

class Tool_Apply_Inspect extends PIDE_MCP_Tool("apply_inspect") {
  def description: String =
    "Run a proof method on the goal at a position and report the theorems it used, without modifying the file: " +
      "**for a method that fails, it shows what the method tried** (simp rules it rewrote with, rules of the " +
      "classical reasoner, local facts and assumptions), which `Failed to apply proof method` does not say. " +
      "It also warns when the simplifier, the classical reasoner or a repeated method seems to loop, with the rules involved. " +
      "`start_line` must be a line inside a proof with an open goal (the goal after that line's command is used). " +
      "The method runs on an instrumented copy of the goal; nothing is applied. Needs a session based on HOL. " +
      PIDE_MCP_Tool_Schema.implicit_load_file

  private val method_arg = PIDE_MCP_Tool_Arg.string(
    "method", "The proof method as after `apply`, e.g. `auto`, `(simp add: foo_def)`, `(rule conjI, simp)`.")
  private val criteria_arg = PIDE_MCP_Tool_Arg.opt_string(
    "criteria",
    "Only report theorems that meet these `find_theorems` criteria, e.g. `name: List` or `\"rev _\"`.")
  private val max_uses_arg = PIDE_MCP_Tool_Arg.int_default(
    "max_uses", "Maximum number of theorems to list.", 100, minimum = Some(1))

  private val method_timeout_arg = PIDE_MCP_Tool_Arg.int_default(
    "method_timeout",
    "Seconds after which the method is stopped (a looping method never finishes); the theorems it used until then " +
      "are still reported. Keep it below `timeout`.", 10, minimum = Some(1), maximum = Some(600))

  private val to_file_arg = PIDE_MCP_Tool_Arg.bool_default(
    "to_file",
    "Write the complete list of used theorems, one per line with its statement (`name: statement`), to a file and " +
      "return only its path, the outcome and the count: grep the file (by name, constant or shape) instead of reading a " +
      "long list. `criteria` and `max_uses` do not limit the file unless given.", false)

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    Tool_Find.common_args ::: List(method_arg, criteria_arg, max_uses_arg, method_timeout_arg, to_file_arg,
      Tool_Find.format))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      if (!args.contains("start_line"))
        error("Missing start_line: apply_inspect needs a line inside a proof with an open goal")
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      Tool_Suggest_Facts.ensure_helper(session, progress, "$PIDE_MCP_HOME/ml/PIDE_MCP_Apply_Inspect.thy",
        "apply_inspect")
      val to_file = to_file_arg.get(args)
      val max_uses =
        if (to_file && !args.contains("max_uses")) Int.MaxValue else max_uses_arg.get(args)
      // to a file: the list goes there, the result keeps the outcome, the loop warnings and the count
      def refine(text: String): String =
        if (!to_file) text
        else {
          val (entries, rest) = split_lines(text).partition(_.startsWith("  "))
          val path = Tool_Apply_Inspect.write_file(session.id, cat_lines(entries.map(_.drop(2))) + "\n")
          cat_lines(rest) + s"\nlist (one per line, `name: statement`): $path"
        }
      Tool_Find.run(session, args, Tool_Apply_Inspect.operation,
        List(method_arg.get(args), criteria_arg.get(args).getOrElse(""),
          max_uses.toString, method_timeout_arg.get(args).toString, to_file.toString), progress, refine)
    }
}

class Tools_Apply_Inspect extends PIDE_MCP_Tools(new Tool_Apply_Inspect)
