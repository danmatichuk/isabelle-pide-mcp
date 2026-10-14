/*  Title:      PIDE_MCP/tool_create_scratch.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

import java.io.{File => JFile}

class Tool_Create_Scratch extends PIDE_MCP_Tool("create_scratch") {
  def description: String =
    "Create a temporary file for experiments (proofs, term shapes, ML) and return its path and `theory` name; the " +
      "server writes the theory header. `copy_from` copies a theory (up to `up_to_line`) with its imports and context " +
      "blocks. Imports are session-qualified (`Box.Box`, not `Box`). Copy final results to the real files; scratch files " +
      "are deleted when the session stops."

  private val name_suffix_arg = PIDE_MCP_Tool_Arg.opt_string(
    "name_suffix",
    "Label for the file name.")
  private val extension_arg = PIDE_MCP_Tool_Arg.opt_string(
    "extension",
    "\".thy\" (default) or \".ML\".")
  private val imports_arg = PIDE_MCP_Tool_Arg.strings_default(
    "imports",
    "Theories to import (default: Main, or Pure without HOL).",
    Nil)

  private val content_arg = PIDE_MCP_Tool_Arg.opt_string(
    "content",
    "Initial text: a whole theory (its `theory` name is replaced by the scratch name) or the body between `begin` " +
      "and `end` (the header is added).")

  private val copy_from_arg = PIDE_MCP_Tool_Arg.opt_string(
    "copy_from",
    "Theory to copy (session-qualified name or file path): the scratch gets its text with the same imports, " +
      "`context ... begin` blocks and everything else.")

  private val up_to_line_arg = PIDE_MCP_Tool_Arg.opt_int(
    "up_to_line",
    "With `copy_from`: copy up to the command at this line; open proofs are closed with `oops`, open blocks with `end`.",
    minimum = Some(1))

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, name_suffix_arg, extension_arg, imports_arg, content_arg,
      copy_from_arg, up_to_line_arg))

  private val scratch_prefix: String = "tmp_pide_mcp_scratch_"
  private val scratch_tmpdir_prefix: String = "tmp_pide_mcp_scratch"
  private val scratch_dirs = Synchronized[Map[String, JFile]](Map.empty)

  private def get_scratch_dir(session_id: String): JFile =
    scratch_dirs.change_result { dirs =>
      dirs.get(session_id) match {
        case Some(dir) if dir.isDirectory => (dir, dirs)
        case _ =>
          val dir = Isabelle_System.tmp_dir(scratch_tmpdir_prefix)
          (dir, dirs + (session_id -> dir))
      }
    }

  override def stop(
    sessions: PIDE_MCP_Sessions,
    session: PIDE_MCP_Session,
    progress: Progress
  ): Unit = {
    val dir = scratch_dirs.change_result(dirs => (dirs.get(session.id), dirs - session.id))
    dir.foreach(Isabelle_System.rm_tree)
  }

  def create_scratch(
    session_id: String,
    name_suffix: Option[String] = None,
    extension: Option[String] = None,
    imports: List[String] = Nil,
    content: Option[String] = None
  ): (Path, String, Boolean) = {
    val suffix = name_suffix.getOrElse(Date.now().format(Date.Format("yyyy_MM_dd_HH_mm_ss_SSS")))
    val base_name = scratch_prefix + suffix
    val ext = extension.map(_.trim).filter(_.nonEmpty) match {
      case Some(ext) => if (ext.startsWith(".")) ext else "." + ext
      case None => ".thy"
    }
    val file_path =
      (File.path(get_scratch_dir(session_id)) + Path.basic(base_name + ext)).canonical
    val created = file_path.file.createNewFile()
    if (created && ext == ".thy") {
      val text =
        content match {
          case Some(text) =>
            Tool_Create_Scratch.rename_theory(text, base_name).getOrElse(
              Tool_Create_Scratch.header(base_name, imports) + text.stripLineEnd + "\n\nend\n")
          case None => Tool_Create_Scratch.header(base_name, imports) + "\nend\n"
        }
      File.write(file_path, text)
    }
    else if (created && content.nonEmpty) File.write(file_path, content.get)
    (file_path, base_name, created)
  }

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.text {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      val name_suffix = name_suffix_arg.get(args)
      val extension = extension_arg.get(args)
      val default_imports =
        if (List("Main", "HOL.Main").exists(session.resources.loaded_theory)) List("Main") else List("Pure")
      val imports = imports_arg.get(args) match { case Nil => default_imports case l => l }
      val copy = copy_from_arg.get(args).map { origin =>
        val node_name = session.node_name(origin)
        if (!node_name.is_theory) error("copy_from: not a theory: " + quote(origin))
        val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress, 30)
        Tool_Create_Scratch.copy(session, snapshot, up_to_line_arg.get(args))
      }
      val (path, theory, created) =
        create_scratch(session.id, name_suffix, extension.orElse(copy.map(_ => ".thy")), imports,
          copy.map(_._1).orElse(content_arg.get(args)))
      JSON_Object("path" -> path.implode) ++
        JSON_Object.if_proper(path.implode.endsWith(".thy"), JSON_Object("theory" -> theory)) ++
        JSON.optional("copied" -> copy.map { case (_, lines, closed) =>
          JSON_Object("lines" -> lines) ++ JSON_Object.if_proper(closed.nonEmpty, JSON_Object("closed_with" -> closed.mkString(" "))) }) ++
        JSON_Object("message" -> (if (created) "File created" else "Path already existed"))
    }
}

object Tool_Create_Scratch {
  def header(theory: String, imports: List[String]): String =
    "theory " + theory + "\n  imports " + imports.map(quote).mkString(" ") + "\nbegin\n\n"

  // a whole theory: its name replaced; None: no `theory` header in the text
  def rename_theory(text: String, theory: String): Option[String] =
    """(?s)\A((?:\s|\(\*.*?\*\)|\\<comment>\s*\\<open>.*?\\<close>)*theory\s+)("[^"]*"|\S+)""".r
      .findFirstMatchIn(text).map(m => text.substring(0, m.start(2)) + theory + text.substring(m.end(2)))

  /* A copy of a theory, up to the command at a line: imports resolved (a relative import would not be found from
     the scratch directory), proofs left open closed with `oops`, blocks left open (`context ... begin`, the theory)
     with `end`. Returns the text without the new name, the number of lines copied, and what was appended. */
  def copy(session: PIDE_MCP_Session, snapshot: Document.Snapshot, up_to_line: Option[Int])
    : (String, Int, List[String]) = {
    val node = snapshot.node
    val source = node.source
    val doc = Line.Document(source)
    val keywords = node.syntax.map(_.keywords)
    def kind(cmd: Command): Option[String] = keywords.flatMap(_.kinds.get(cmd.span.name))
    val commands = node.command_iterator().toList
    val included =
      up_to_line match {
        case None => commands
        case Some(line) => commands.takeWhile { case (_, start) => doc.position(start).line1 <= line }
      }
    val stop = included.lastOption.fold(0) { case (cmd, start) => start + cmd.length }
    var goals = 0
    var blocks = 0
    for ((cmd, _) <- included if !cmd.is_ignored) {
      val k = kind(cmd)
      if (k.exists(k => Keyword.theory_goal(k) || Keyword.proof_goal(k))) goals += 1
      else if (k.exists(k => Keyword.qed(k) || Keyword.qed_global(k))) goals = (goals - 1) max 0
      if (k.exists(Keyword.qed_global)) goals = 0  // oops, sorry at the top: the whole theorem
      if (cmd.span.content.exists(_.is_begin)) blocks += 1
      else if (cmd.span.name == "end") blocks = (blocks - 1) max 0
    }
    val closed = (if (goals > 0) List("oops") else Nil) ::: List.fill(blocks)("end")
    val imports_resolved = node.header.imports.map { name =>
      if (session.is_base_session_theory(name)) name.theory
      else name.path.expand.implode.stripSuffix(".thy")
    }
    val text0 = source.substring(0, stop)
    val text1 =
      """(?s)\bimports\s+(.*?)(?=\s(?:keywords|abbrevs|begin)\b)""".r.findFirstMatchIn(text0) match {
        case Some(m) if imports_resolved.nonEmpty =>
          text0.substring(0, m.start(1)) + imports_resolved.map(quote).mkString(" ") + text0.substring(m.end(1))
        case _ => text0
      }
    (text1.replaceAll("\\s+\\z", "") + "\n" + closed.map(_ + "\n").mkString, doc.position(stop).line1, closed)
  }
}

class Tools_Create_Scratch extends PIDE_MCP_Tools(new Tool_Create_Scratch)
