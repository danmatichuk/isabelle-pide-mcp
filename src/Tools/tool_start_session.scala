/*  Title:      PIDE_MCP/tool_start_session.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

class Tool_Start_Session extends PIDE_MCP_Tool("start_session") {
  def description: String =
    "Start a PIDE session and return its id once it is ready; building the base session **may take many minutes**. " +
      PIDE_MCP_Tool_Schema.session_restart + " " +
      "Edits in a directory reach every session using it."

  private val default_spec: PIDE_MCP_Session.Spec = PIDE_MCP_Session.Spec()

  private val session_arg = PIDE_MCP_Tool_Arg.opt_string(
    "session",
    s"Session id to use (default: generated based on ${quote("logic")}).")
  private val logic_arg = PIDE_MCP_Tool_Arg.string_default(
    "logic",
    "Base session, or with `session_requirements` the session whose theories you edit.",
    default_spec.logic)
  private val dirs_arg = PIDE_MCP_Tool_Arg.strings_default(
    "dirs",
    "Directories with the project's ROOT files.",
    default_spec.dirs.map(_.implode))
  private val options_arg = PIDE_MCP_Tool_Arg.strings_default(
    "options",
    s"Isabelle system options to override, each as ${quote("NAME=VAL")} or ${quote("NAME")}.",
    default_spec.options.map(_.print))
  private val session_ancestor_arg = PIDE_MCP_Tool_Arg.opt_string(
    "session_ancestor",
    s"With `session_requirements`: the ancestor to build on (default: parent of ${quote("logic")}).")
  private val session_requirements_arg = PIDE_MCP_Tool_Arg.bool_default(
    "session_requirements",
    s"Build the base from the imports of ${quote("logic")}, so that its own theories can be edited.",
    default_spec.session_requirements)
  private val fresh_build_arg = PIDE_MCP_Tool_Arg.bool_default(
    "fresh_build",
    "Force rebuild of the base session.", default_spec.fresh_build)
  private val no_build_arg = PIDE_MCP_Tool_Arg.bool_default(
    "no_build",
    "Fail instead of building the base session (to check whether a start is cheap).",
    default_spec.no_build)

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(session_arg, logic_arg, dirs_arg, options_arg, session_ancestor_arg,
      session_requirements_arg, fresh_build_arg, no_build_arg, ignore_memory_check_arg, min_available_mb_arg))

  private val ignore_memory_check_arg = PIDE_MCP_Tool_Arg.bool_default(
    "ignore_memory_check",
    "Start even with less than `min_available_mb` MB free.",
    false)
  private val min_available_mb_arg = PIDE_MCP_Tool_Arg.int_default(
    "min_available_mb",
    "Refuse to start with less free memory (MB).",
    PIDE_MCP_Machine.default_min_available_mb, minimum = Some(0))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    Exn.result {
      PIDE_MCP_Machine.require_memory(ignore_memory_check_arg.get(args), min_available_mb_arg.get(args))
      PIDE_MCP_Session.Spec(
        logic = logic_arg.get(args),
        id = session_arg.get(args),
        dirs = dirs_arg.get(args).map(PIDE_MCP_Session.Spec.dir),
        options = options_arg.get(args).map(PIDE_MCP_Session.Spec.option),
        session_ancestor = session_ancestor_arg.get(args),
        session_requirements = session_requirements_arg.get(args),
        fresh_build = fresh_build_arg.get(args),
        no_build = no_build_arg.get(args))
    } match {
      case Exn.Exn(exn) => PIDE_MCP_Tool_Result.exn_error(exn)
      case Exn.Res(requested) =>
        // a bare start of a session name that the server was configured with reuses that configuration; a bare start of an
        // unknown name starts a plain session, which is said
        val bare = requested.copy(id = None) == default_spec
        val (spec, note) = requested.id match {
          case Some(id) if sessions.configured_spec(id).nonEmpty && bare =>
            val conf = sessions.configured_spec(id).get
            (conf, Some(s"reused the configuration of the session ${quote(id)} that the server was started with " +
              s"(logic ${conf.logic}${if (conf.session_requirements) ", with requirements" else ""}" +
              s"${conf.session_ancestor.fold("")(a => ", ancestor " + a)}" +
              s"${if (conf.dirs.nonEmpty) ", dirs " + conf.dirs.map(_.implode).mkString(", ") else ""})"))
          case Some(id) if sessions.configured_spec(id).nonEmpty =>
            (requested, Some(s"the given arguments override the configuration of the session ${quote(id)} that the server was started with"))
          case Some(id) if bare && sessions.configured_ids.nonEmpty =>
            (requested, Some(s"started a plain ${quote(requested.logic)} session: no session ${quote(id)} was configured at server start " +
              s"(configured: ${commas_quote(sessions.configured_ids)})"))
          case _ => (requested, None)
        }
        PIDE_MCP_Status.with_reports(progress, "start_session " + spec.id.getOrElse(spec.logic)) {
          sessions.start_session(spec, progress)
        } match {
          case Result.Res(session) =>
            PIDE_MCP_Tool_Result.Res(PIDE_MCP_Tool_Result.Text(
              PIDE_MCP_Render.text(JSON_Object("session" -> session.id) ++ JSON.optional("note" -> note))))
          case Result.Error(exn) => PIDE_MCP_Tool_Result.exn_error(exn)
        }
    }
}

class Tools_Start_Session extends PIDE_MCP_Tools(new Tool_Start_Session)
