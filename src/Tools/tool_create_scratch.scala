/*  Title:      PIDE_MCP/tool_create_scratch.scala
    Author:     Kevin Kappelmann
*/

package isabelle.pide.mcp

import isabelle._

import java.io.{File => JFile}

class Tool_Create_Scratch extends PIDE_MCP_Tool("create_scratch") {
  def description: String =
    "Create a temporary file for experimentation that does not interfere with user accessible files. " +
      "Use this whenever you think you need to do iterative developments or when you want to find and explore theorems, syntax, concepts, commands, ML code, etc. Write back final results to files accessible to the user. " +
      "The theory name of a \".thy\" file (to be used in its `theory` header) is returned. " +
      "Imports are theory names qualified by their session unless they belong to the base session's own top-level namespace, " +
      "e.g. `Box.Box`, not `Box` (`Bad theory import \"Box\"`): `get_session_state` lists the sessions and their directories. " +
      "Temporary files are cleaned up when the session stops."

  private val name_suffix_arg = PIDE_MCP_Tool_Arg.opt_string(
    "name_suffix",
    "Label to identify the scratch file (auto-generated if omitted)")
  private val extension_arg = PIDE_MCP_Tool_Arg.opt_string(
    "extension",
    "File extension, with or without the leading dot (default: \".thy\"). PIDE only loads files with the extension \".thy\" or \".ML\".")
  private val imports_arg = PIDE_MCP_Tool_Arg.strings_default(
    "imports",
    "For a new \".thy\" file: theories to import (e.g. [\"Main\"]). If given, the file is initialised with a theory skeleton " +
      "(header, `begin`, `end`) that you then extend; otherwise the file is empty and you have to write the header yourself.",
    Nil)

  private val content_arg = PIDE_MCP_Tool_Arg.opt_string(
    "content",
    "Complete initial text of the file (including the `theory` header for a \".thy\" file; the theory name must be the returned `theory`, " +
      "so use `imports` instead if you only need the skeleton). If given, `imports` is ignored.")

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, name_suffix_arg, extension_arg, imports_arg, content_arg))

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
    if (created && content.nonEmpty) File.write(file_path, content.get)
    else if (created && ext == ".thy" && imports.nonEmpty)
      File.write(file_path,
        "theory " + base_name + "\n  imports " + imports.map(quote).mkString(" ") + "\nbegin\n\nend\n")
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
      val (path, theory, created) =
        create_scratch(session.id, name_suffix, extension, imports_arg.get(args), content_arg.get(args))
      val header_note = content_arg.get(args).flatMap { text =>
        if (!path.implode.endsWith(".thy")) None
        else """(?m)^\s*theory\s+(\S+)""".r.findFirstMatchIn(text) match {
          case Some(m) if m.group(1) != theory =>
            Some(s"the theory name in the content is ${quote(m.group(1))}, but it must be ${quote(theory)} (the file name): " +
              "otherwise the header fails with `Bad theory name` and nothing in the file is processed")
          case None => Some("the content has no `theory` header line")
          case _ => None
        }
      }
      JSON_Object("path" -> path.implode) ++ JSON.optional("note" -> header_note) ++
        JSON_Object.if_proper(path.implode.endsWith(".thy"), JSON_Object("theory" -> theory)) ++
        JSON_Object("message" -> (if (created) "File created" else "Path already existed"))
    }
}

class Tools_Create_Scratch extends PIDE_MCP_Tools(new Tool_Create_Scratch)
