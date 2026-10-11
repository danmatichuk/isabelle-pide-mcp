/*  Title:      PIDE_MCP/tool_suggest_facts.scala

Tool listing the facts that Sledgehammer's relevance filter selects for a goal.
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Suggest_Facts {
  val operation: String = "pide_mcp_suggest_facts"
  private val helper = "$PIDE_MCP_HOME/ml/PIDE_MCP_Suggest.thy"

  // load a helper theory (registers a query operation) and wait until it is processed
  def ensure_helper(
    session: PIDE_MCP_Session,
    progress: Progress,
    helper: String = helper,
    what: String = "fact suggestions",
    needs_hol: Boolean = true,
    wait_seconds: Int = 180
  ): Unit = {
    // a helper that imports Main would load all of HOL from source without HOL in the base session
    if (needs_hol && !List("Main", "HOL.Main").exists(session.resources.loaded_theory))
      error(s"Fact suggestions need Sledgehammer: the session ${quote(session.id)}" +
        " is not based on HOL (theory Main is not part of its base session).")
    val node_name = session.node_name(Path.explode(helper).expand.implode)
    var snapshot =
      PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress, wait_seconds)
    val deadline = Time.now() + Time.seconds(wait_seconds)
    def status = PIDE_MCP_Theory.Status.statuses(session, snapshot, List(node_name)).head
    while (status.overall_status == Document_Status.Overall_Status.pending && Time.now() < deadline) {
      progress.expose_interrupt()
      Time.seconds(0.5).sleep()
      snapshot = session.node_snapshot(node_name)
    }
    if (status.overall_status != Document_Status.Overall_Status.ok) {
      val failure = PIDE_MCP_Theory.failures_json(snapshot, 1)
      error(s"The helper theory for $what could not be loaded" +
        (if (needs_hol) " (Sledgehammer must be available, i.e. the session must build on HOL)" else "") +
        ": " + JSON.Format(failure))
    }
  }
}

class Tool_Suggest_Facts extends PIDE_MCP_Tool("suggest_facts") {
  def description: String =
    "List the facts (theorems) that Sledgehammer's relevance filter (MePo/MaSh) considers most relevant for the goal at a position, " +
      "most relevant first, without running any prover and without modifying the file. " +
      "Use it to see which lemmas might close a goal, e.g. before a manual proof or to pass them to `try0`/`simp:`/`using`. " +
      "`start_line` must be a line inside a proof with an open goal (the goal after that line's command is used). " +
      "Needs a session based on HOL. " +
      PIDE_MCP_Tool_Schema.implicit_load_file

  private val max_facts_arg = PIDE_MCP_Tool_Arg.int_default(
    "max_facts", "Maximum number of facts to return.", 20, minimum = Some(1))
  private val statements_arg = PIDE_MCP_Tool_Arg.bool_default(
    "statements", "Also show the statement of each fact (cut at 300 characters).", true)

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    Tool_Find.common_args ::: List(max_facts_arg, statements_arg, Tool_Find.format))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("readOnlyHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.result {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      Tool_Suggest_Facts.ensure_helper(session, progress)
      Tool_Find.run(session, args, Tool_Suggest_Facts.operation,
        List(max_facts_arg.get(args).toString, statements_arg.get(args).toString), progress)
    }
}

class Tools_Suggest_Facts extends PIDE_MCP_Tools(new Tool_Suggest_Facts)
