/*  Title:      PIDE_MCP/tool_eval_ml.scala

Tool evaluating ML code in the context at a position of a theory, without modifying any file.
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Eval_ML {
  val operation: String = "pide_mcp_eval_ml"
}

class Tool_Eval_ML extends PIDE_MCP_Tool("eval_ml") {
  def description: String =
    "Evaluate ML code in the context at a position of a loaded theory, with all ML structures, functions, and data " +
      "of the theories imported there (e.g. those defined by `ML_file`) and the theory/proof context of that position " +
      "(`Context.the_generic_context ()`, `Proof_Context.theory_of`, ... are available). " +
      "Returns the output of the code: echoed ML values (like `ML` in a theory), and `writeln`/`tracing`/`warning` calls. " +
      "The file and the theory are not modified: bindings of the code are discarded, so **use this instead of inserting " +
      "`tracing` calls into an ML file (which reloads it and everything that depends on it).** " +
      "The code runs with the full rights of the server; side effects outside the theory (files, global ML references) persist. " +
      PIDE_MCP_Tool_Schema.implicit_load_file

  private val code_arg = PIDE_MCP_Tool_Arg.string(
    "code", "ML source text to evaluate, e.g. `val t = Syntax.read_term (Context.the_local_context ()) \"x + y\"`.")

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    Tool_Find.common_args ::: List(code_arg, Tool_Find.format))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      Tool_Suggest_Facts.ensure_helper(session, progress, "$PIDE_MCP_HOME/ml/PIDE_MCP_Eval.thy",
        "ML evaluation", needs_hol = false)
      Tool_Find.run(session, args, Tool_Eval_ML.operation, List(code_arg.get(args)), progress)
    }
}

class Tools_Eval_ML extends PIDE_MCP_Tools(new Tool_Eval_ML)
