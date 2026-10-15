/*  Title:      PIDE_MCP/pide_mcp_tool_util.scala
    Author:     Kevin Kappelmann

Utilities for PIDE MCP Tools.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Tool_Util {
  val retry_soon_message: String = "Please retry soon."
  val default_start_line: Int = 1
  val all_tools_option: String = "*"

  def make_tool_table(tool_names: String = all_tools_option): Map[String, PIDE_MCP_Tool] = {
    val tools =
      Isabelle_System.make_services(classOf[PIDE_MCP_Tools]).flatMap(_.entries).
        foldLeft(Map.empty[String, PIDE_MCP_Tool]) {
          case (tools, tool) =>
            if (tools.isDefinedAt(tool.name))
              error(s"Duplicate PIDE_MCP tool: ${Library.quote(tool.name)}")
            else tools + (tool.name -> tool)
        }
    if (tool_names == all_tools_option) tools
    else {
      val names = space_explode(',', tool_names)
      val unknown = names.filterNot(tools.isDefinedAt)
      if (unknown.nonEmpty)
        error(s"Unknown PIDE_MCP tool(s): ${commas_quote(unknown)}. " +
          s"Available tool(s): ${commas_quote(tools.keys.toList.sorted)}")
      tools.filter { case (name, _) => names.contains(name) }
    }
  }

  def running_session_param(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T
  ): PIDE_MCP_Session = {
    def available = s"Available session(s): ${JSON.Format(sessions.state_json())}"
    val session_ids = PIDE_MCP_Tool_Schema.running_session_arg.get(args).map(List(_))
    Result.release(sessions.running_sessions(session_ids)) match {
      case List(session) => session
      case Nil => error(s"No running session. $available")
      case _ => error(s"Multiple running sessions: specify ${quote("session")}. $available")
    }
  }

  def running_sessions_param(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T
  ): List[PIDE_MCP_Session] =
    Result.release(
      sessions.running_sessions(PIDE_MCP_Tool_Schema.running_sessions_arg.get(args)))

  def origin_param(session: PIDE_MCP_Session, args: JSON.Object.T): Document.Node.Name =
    session.node_name(PIDE_MCP_Tool_Schema.origin_arg.get(args))

  /* The snapshot of a loaded origin; an origin that is not loaded yet is loaded. wait_seconds: wait this long for
     the snapshot of a newly loaded (or not yet processed) origin instead of failing at once. */
  def require_loaded_origin_snapshot(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    progress: Progress,
    wait_seconds: Double = 0
  ): Document.Snapshot = {
    val deadline = Time.now() + Time.seconds(wait_seconds)
    def await_snapshot(message: String): Document.Snapshot = {
      var result: Option[Document.Snapshot] = None
      while (result.isEmpty && Time.now() < deadline) {
        progress.expose_interrupt()
        Time.seconds(0.1).sleep()
        result = Exn.result(session.node_snapshot(node_name)) match {
          case Exn.Res(snapshot) => Some(snapshot)
          case Exn.Exn(exn) if Exn.is_interrupt(exn) => throw exn
          case Exn.Exn(_) => None
        }
      }
      result.getOrElse(error(message))
    }
    val snapshot =
      Exn.result { session.node_snapshot(node_name) } match {
        case Exn.Res(snapshot) => snapshot
        case Exn.Exn(_) =>
          if (PIDE_MCP_Util.is_loaded_dynamic(session.tip_version(progress).nodes(node_name)))
            await_snapshot(s"The origin ${quote(session.origin(node_name))} is loaded but has not been " +
              "processed yet. " + retry_soon_message)
          else {
            session.read_update_resolve(node_name, List((1, Some(0))), // pass empty range to load empty nodes
              await_stable_before_resolve = false, hide_others = false, range_context = 0,
              progress = progress)
            await_snapshot(s"The origin ${quote(session.origin(node_name))} was not loaded and has now " +
              "been queued for loading" +
              (if (wait_seconds > 0) s" (not ready after ${wait_seconds.toInt} s). " else ". ") + retry_soon_message)
          }
      }
    if (!node_name.is_theory && snapshot.commands_loading.isEmpty)
      error(s"File ${quote(session.origin(node_name))} is not loaded by any theory. " +
        "Load the containing theory first.")
    snapshot
  }

  /* Synchronise loaded files that were edited on disk outside of the server (e.g. by scripts).
     Waits until the edit is part of the current document version (not until it is processed),
     so that the state never shows results of the replaced text. Returns the synced changes. */
  private val sync_lines_limit = 10

  // changed lines of the new text, e.g. "11", "151-152", "30 (removed)"
  private def sync_lines(change: PIDE_MCP_Session#Sync_Change): List[String] = {
    val ranges = change.hunks.map { h =>
      if (h.new_lines == 0) s"${h.new_line} (removed)"
      else if (h.new_lines == 1) h.new_line.toString
      else s"${h.new_line}-${h.new_line + h.new_lines - 1}"
    }
    if (ranges.length > sync_lines_limit)
      ranges.take(sync_lines_limit) :+ s"... (+${ranges.length - sync_lines_limit} more spans)"
    else ranges
  }

  def sync_disk_edits(
    session: PIDE_MCP_Session,
    progress: Progress
  ): Option[(List[Document.Node.Name], JSON.Object.T)] = {
    val changes = session.sync_disk(progress)
    Option.when(changes.nonEmpty) {
      val node_names = changes.map(_.node_name)
      session.await_stable_snapshot(progress)
      session.resolve_dependencies(node_names, progress)
      (node_names, JSON_Object(
        "files" -> changes.map(change => JSON_Object(
          "origin" -> session.origin(change.node_name),
          "changed_lines" -> change.changed_lines,
          "lines" -> sync_lines(change),
          "total_lines" -> change.total_lines))))
    }
  }

  def with_synced(
    json: JSON.Object.T,
    synced: Option[(List[Document.Node.Name], JSON.Object.T)]
  ): JSON.Object.T =
    json ++ JSON.optional("external_edits_synced" -> synced.map(_._2))

  /* After a disk edit was synced, the results of the old text are gone and the new text is not processed yet:
     wait a little (until the lines are processed, at most `seconds`), such that the state shows the new text. */
  def await_processed(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    start_line: Int,
    end_line: Int,
    seconds: Double,
    progress: Progress
  ): Unit = {
    val deadline = Time.now() + Time.seconds(seconds)
    def pending: Boolean =
      Exn.result(session.node_snapshot(node_name)) match {
        case Exn.Res(snapshot) =>
          val doc = Line.Document(snapshot.node.source)
          snapshot.is_outdated ||
            PIDE_MCP_Util.text_range(doc, start_line, end_line min doc.lines.length).exists(range =>
              PIDE_MCP_Command.iterator(snapshot, Some(range)).exists { case (cmd, _) =>
                val status = PIDE_MCP_Command.status(snapshot, cmd)
                status.is_unprocessed || status.is_running
              })
        case Exn.Exn(exn) if Exn.is_interrupt(exn) => throw exn
        case Exn.Exn(_) => true
      }
    while (Time.now() < deadline && pending) {
      progress.expose_interrupt()
      Time.seconds(0.1).sleep()
    }
  }

  def require_ordered_lines(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int]
  ): Unit = {
    for (start_line <- opt_start_line; end_line <- opt_end_line if end_line < start_line)
      error(s"end_line $end_line < start_line $start_line")
  }

  def require_lines_in_bounds(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    total_lines: Int
  ): Unit = {
    for (start_line <- opt_start_line 
        if start_line < 1 || start_line > (total_lines max default_start_line))
      error(s"start_line $start_line out of bounds (file has $total_lines lines)")
    for (end_line <- opt_end_line if end_line < 1 || end_line > total_lines)
      error(s"end_line $end_line out of bounds (file has $total_lines lines)")
  }

  def require_valid_lines(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    total_lines: Int
  ): Unit = {
    require_ordered_lines(opt_start_line, opt_end_line)
    require_lines_in_bounds(opt_start_line, opt_end_line, total_lines)
  }

  def resolve_lines(
    opt_start_line: Option[Int],
    opt_end_line: Option[Int],
    total_lines: Int
  ): (Int, Int) = {
    require_valid_lines(opt_start_line, opt_end_line, total_lines)
    (opt_start_line.getOrElse(default_start_line), opt_end_line.getOrElse(total_lines))
  }
}
