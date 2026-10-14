/*  Title:      PIDE_MCP/tool_cancel.scala

Interrupt running commands of a theory without unloading it.
*/

package isabelle.pide.mcp

import isabelle._

object Tool_Cancel {
  sealed case class Canceled(line: Int, end_line: Int, name: String, running_ms: Long) {
    def json: JSON.Object.T =
      JSON_Object("line" -> line, "command" -> name, "running_seconds" -> running_ms / 1000) ++
        JSON_Object.if_proper(end_line > line, JSON_Object("end_line" -> end_line))
  }

  // the running commands of a theory node that intersect the lines and have run for at least min_ms
  def running(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    start_line: Int,
    end_line: Int,
    min_ms: Long = 0
  ): List[(PIDE_MCP_Command.State, Canceled)] = {
    val snapshot = session.node_snapshot(node_name)
    val doc = Line.Document(snapshot.node.source)
    PIDE_MCP_Util.text_range(doc, start_line, end_line min doc.lines.length).toList.flatMap { range =>
      PIDE_MCP_Command.State.commands(snapshot, Some(range)).toList
        .filter(state => state.status.contains(PIDE_MCP_Command.Status.running) && state.running_ms >= min_ms)
        .map { state =>
          val line = doc.position(state.command_range.start).line1
          val end = doc.position(state.command_range.stop).line1
          (state, Canceled(line, end max line, state.cmd.span.name, state.running_ms))
        }
    }
  }

  // interrupt the running commands (their evaluation and their print functions); returns what was interrupted
  def cancel_running(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    start_line: Int,
    end_line: Int,
    min_ms: Long = 0
  ): List[Canceled] = {
    val found = running(session, node_name, start_line, end_line, min_ms)
    for ((state, canceled) <- found) cancel_command(session, state.snapshot, state.cmd, canceled.running_ms)
    found.map(_._2)
  }

  // interrupt a command: its evaluation and its print functions
  def cancel_command(session: PIDE_MCP_Session, snapshot: Document.Snapshot, cmd: Command, running_ms: Long): Unit = {
    PIDE_MCP_Command.Running_Clock.cancel(snapshot, cmd, running_ms)
    val execs = snapshot.state.assignments.get(snapshot.version.id)
      .flatMap(_.command_execs.get(cmd.id)).getOrElse(Nil)
    execs.foreach(session.session.cancel_exec)
  }

  // after an edit: cancel the commands of the edited lines that run longer than the limit, until the lines are processed,
  // the file text changes (a later edit), or the session stops
  def cancel_after(
    session: PIDE_MCP_Session,
    node_name: Document.Node.Name,
    start_line: Int,
    end_line: Int,
    seconds: Int
  ): Unit = {
    val source = session.node_snapshot(node_name).node.source
    val horizon = Time.now() + Time.seconds(seconds + 600)
    Isabelle_Thread.fork(name = "pide_mcp_cancel_after", daemon = true) {
      Exn.result {
        var active = true
        Time.seconds(1).sleep()
        while (active && Time.now() < horizon) {
          val snapshot = session.node_snapshot(node_name)
          if (snapshot.node.source != source) active = false
          else {
            cancel_running(session, node_name, start_line, end_line, min_ms = seconds * 1000L)
            val doc = Line.Document(snapshot.node.source)
            val pending = PIDE_MCP_Util.text_range(doc, start_line, end_line min doc.lines.length).exists(range =>
              PIDE_MCP_Command.State.commands(snapshot, Some(range)).exists(state =>
                state.status.exists(Set(PIDE_MCP_Command.Status.running, PIDE_MCP_Command.Status.unprocessed))))
            if (pending) Time.seconds(1).sleep() else active = false
          }
        }
      }
      ()
    }
  }

  val after_note: String =
    "a cancelled command shows as `canceled`; in a proof that runs in parallel (`by` of a lemma) the file goes on, otherwise the " +
      "commands after it stay unprocessed. Edit the cancelled command " +
      "(e.g. replace the slow method by a cheaper one, or by `sorry` for now); the edit reprocesses it and everything after it"
}

class Tool_Cancel extends PIDE_MCP_Tool("cancel") {
  def description: String =
    "Interrupt running commands of a theory (a slow or looping method) without unloading it: those intersecting " +
      "`start_line`/`end_line`, or all. They show as `canceled`; edit them (cheaper method) to go on."

  private val wait_arg = PIDE_MCP_Tool_Arg.int_default(
    "wait_seconds", "Wait this long for the commands to stop.",
    3, minimum = Some(0), maximum = Some(30))

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, PIDE_MCP_Tool_Schema.origin_arg,
      PIDE_MCP_Tool_Schema.opt_start_line_arg, PIDE_MCP_Tool_Schema.opt_end_line_arg, wait_arg))

  override def annotations: Option[JSON.Object.T] = Some(JSON_Object("destructiveHint" -> true))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.text {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      val node_name = PIDE_MCP_Tool_Util.origin_param(session, args)
      if (!node_name.is_theory) error("Not a theory: to stop an ML file, cancel the `ML_file` command of the theory that loads it")
      val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress, 0)
      val doc = Line.Document(snapshot.node.source)
      val (start_line, end_line) = PIDE_MCP_Tool_Util.resolve_lines(
        PIDE_MCP_Tool_Schema.opt_start_line_arg.get(args), PIDE_MCP_Tool_Schema.opt_end_line_arg.get(args), doc.lines.length)
      val canceled = Tool_Cancel.cancel_running(session, node_name, start_line, end_line)
      if (canceled.isEmpty) {
        // the command may have finished just before this call: say so, with its status
        val snapshot = session.node_snapshot(node_name)
        val doc = Line.Document(snapshot.node.source)
        val recent = PIDE_MCP_Util.text_range(doc, start_line, end_line min doc.lines.length).toList.flatMap { range =>
          PIDE_MCP_Command.State.commands(snapshot, Some(range)).toList.flatMap { state =>
            PIDE_MCP_Command.Running_Clock.stopped_ago(snapshot, state.cmd).filter(_._1 < Time.seconds(60)).map { case (ago, ran) =>
              val kinds = state.problem_kinds
              s"line ${doc.position(state.command_range.start).line1} (`${state.cmd.span.name}`) finished " +
                f"${ago.seconds}%.1f s ago after ${ran.seconds}%.1f s: " +
                (if (kinds.isEmpty) "ok" else kinds.mkString(","))
            }
          }
        }
        JSON_Object("canceled" -> Nil,
          "note" -> (s"no command is running in lines $start_line-$end_line" +
            (if (recent.isEmpty) " (`get_state` shows the running ones)" else "; recently finished: " + recent.mkString("; "))))
      }
      else {
        val deadline = Time.now() + Time.seconds(wait_arg.get(args))
        while (Time.now() < deadline &&
          Tool_Cancel.running(session, node_name, start_line, end_line).exists(r => canceled.exists(_.line == r._2.line)))
          Time.seconds(0.1).sleep()
        val still = Tool_Cancel.running(session, node_name, start_line, end_line).map(_._2)
          .filter(r => canceled.exists(_.line == r.line))
        JSON_Object("canceled" -> canceled.map(_.json), "note" -> Tool_Cancel.after_note) ++
          JSON_Object.if_proper(still.nonEmpty, JSON_Object("still_running" -> still.map(_.json),
            "still_running_note" -> "these did not stop yet (an ML loop that does not check for interrupts may not stop at all): call `get_state` later"))
      }
    }
}

class Tool_Retry_Without_Limit extends PIDE_MCP_Tool("retry_without_limit") {
  // the step time limit, one watcher per session
  private val limits = Synchronized(Map.empty[String, PIDE_MCP_Step_Limit])

  override def start(sessions: PIDE_MCP_Sessions, session: PIDE_MCP_Session, progress: Progress): Unit =
    if (session.options.real("pide_mcp_step_time_limit") > 0)
      limits.change(_ + (session.id -> new PIDE_MCP_Step_Limit(session)))

  override def stop(sessions: PIDE_MCP_Sessions, session: PIDE_MCP_Session, progress: Progress): Unit =
    limits.change_result(m => (m.get(session.id), m - session.id)).foreach(_.stop())

  def description: String =
    "Run a proof step again that was interrupted at the time limit for proof steps (or cancelled), without the " +
      "limit and without automatic inspection, once: the command at `start_line` runs until it finishes or `cancel` " +
      "stops it. An edit of the command brings the limit back."

  def input_schema: JSON.Object.T = PIDE_MCP_Tool_Schema.input_schema(
    List(PIDE_MCP_Tool_Schema.running_session_arg, PIDE_MCP_Tool_Schema.origin_arg,
      PIDE_MCP_Tool_Schema.start_line_arg))

  def handle(
    sessions: PIDE_MCP_Sessions,
    args: JSON.Object.T,
    progress: Progress
  ): PIDE_MCP_Tool_Result =
    PIDE_MCP_Tool_Result.text {
      val session = PIDE_MCP_Tool_Util.running_session_param(sessions, args)
      val node_name = PIDE_MCP_Tool_Util.origin_param(session, args)
      if (!node_name.is_theory) error("Not a theory")
      val snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress, 0)
      val doc = Line.Document(snapshot.node.source)
      val line = PIDE_MCP_Tool_Schema.start_line_arg.get(args)
      val found = PIDE_MCP_Util.text_range(doc, line, line).toList.flatMap(range =>
        PIDE_MCP_Command.State.commands(snapshot, Some(range)).toList)
        .filter(state => PIDE_MCP_Command.Running_Clock.canceled_ms(snapshot, state.cmd).nonEmpty)
      found.headOption match {
        case None =>
          error(s"No interrupted or cancelled command at line $line: `get_state` shows them as `canceled`")
        case Some(state) =>
          val start = state.command_range.start
          PIDE_MCP_Step_Limit.retries.change(PIDE_MCP_Step_Limit.Retry(state.cmd.id, node_name, start,
            state.cmd.source, Time.now()) :: _)
          session.reevaluate(node_name, state.command_range, progress)
          JSON_Object("retrying" -> JSON_Object("line" -> doc.position(start).line1, "command" -> state.cmd.span.name),
            "note" -> ("it runs without the time limit and without automatic inspection until it finishes; " +
              "`get_state` shows it, `cancel` stops it; an edit of it brings the limit back"))
      }
    }
}

class Tools_Cancel extends PIDE_MCP_Tools(new Tool_Cancel, new Tool_Retry_Without_Limit)
