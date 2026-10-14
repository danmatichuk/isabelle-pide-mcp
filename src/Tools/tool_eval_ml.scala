/*  Title:      PIDE_MCP/tool_eval_ml.scala

Tool evaluating ML code in the context at a position of a theory, without modifying any file.
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Eval_ML {
  val operation: String = "pide_mcp_eval_ml"
}

class Tool_Eval_ML extends PIDE_MCP_Tool("eval_ml") {
  override def start(sessions: PIDE_MCP_Sessions, session: PIDE_MCP_Session, progress: Progress): Unit =
    if (session.options.string("pide_mcp_qualify_names") != "none") Tool_Eval_ML_Base_Consts.load(session)

  def description: String =
    "Evaluate ML code at a position of a theory, with the ML structures of its imports and the context there " +
      "(`Context.the_generic_context ()`); returns echoed values and `writeln`/`tracing` output. Nothing is modified and " +
      "bindings are discarded: **use it instead of inserting `tracing` into an ML file** (which reloads it and its dependents). " +
      "Side effects outside the theory (files, global references) persist. " +
      PIDE_MCP_Tool_Schema.implicit_load_file

  private val code_arg = PIDE_MCP_Tool_Arg.string(
    "code", "ML source text to evaluate.")

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

object Tool_Eval_ML_Base_Consts {
  /* the constants of the session image (its base sessions) that its plain base name refers to, one full name per
     line: in a leaf theory of the image, the base name interns to the constant (not `hide_const (open)`, like
     Limited_Sequence.map_seq) */
  val code: String =
    "let val thys = map Thy_Info.get_theory (Thy_Info.get_names ()); " +
      "val parents = fold (fn thy => fold (fn p => Symtab.update (Context.theory_long_name p, ())) " +
        "(Theory.parents_of thy)) thys Symtab.empty; " +
      "val leaves = filter (fn thy => not (Symtab.defined parents (Context.theory_long_name thy))) thys; " +
      "fun add thy tab = let val consts = Sign.consts_of thy; val space = Consts.space_of consts " +
        "in fold (fn (c, _) => fn tab => if Name_Space.intern space (Long_Name.base_name c) = c " +
          "then Symtab.update (c, ()) tab else tab) (#constants (Consts.dest consts)) tab end " +
      "in writeln (cat_lines (Symtab.keys (fold add leaves Symtab.empty))) end"

  // in the background after a session start: the base-session constants, for PIDE_MCP_Util.Qualify_Names (shadowing)
  def load(session: PIDE_MCP_Session): Unit =
    Isabelle_Thread.fork(name = s"pide_mcp_base_consts_${session.id}", daemon = true) {
      Exn.result {
        val progress = new Progress
        Tool_Suggest_Facts.ensure_helper(session, progress, "$PIDE_MCP_HOME/ml/PIDE_MCP_Eval.thy",
          "ML evaluation", needs_hol = false)
        val node_name = session.node_name(Path.explode("$PIDE_MCP_HOME/ml/PIDE_MCP_Eval.thy").expand.implode)
        val snapshot = session.node_snapshot(node_name)
        val offset = PIDE_MCP_Tool_Util.default_query_position(snapshot)._1
        val result = new PIDE_MCP_Query(session, PIDE_MCP_Editor.Context(node_name, offset), Tool_Eval_ML.operation, progress)
          .run(List(code), Some(Time.seconds(120)))
        if (result.outcome == PIDE_MCP_Query.Outcome.finished)
          PIDE_MCP_Util.Qualify_Names.add_base_consts(split_lines(result.text).map(_.trim).filter(_.nonEmpty))
      }
      ()
    }
}
