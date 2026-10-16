/*  Title:      PIDE_MCP/pide_mcp_step_limit.scala

A time limit for proof steps (pide_mcp_step_time_limit, default 5 s; 0: none): a step (`apply`, `by`, `proof`,
`qed`, `done`, `.`, `..`) whose real run reaches the limit is interrupted and shown as canceled.
`set_step_time_limit` changes the limit of a running session.
`retry_without_limit` lets the command run again without the limit, once: the permission belongs to that
evaluation of the command, an edit of it (a new command) or a re-evaluation has the limit again. Only the MCP's
PIDE sessions are affected, not batch builds or the user's editor.

Sometimes the interrupted evaluation stops, but its closing status never arrives and PIDE keeps showing the command
as running: then the server sends that status itself (PIDE_MCP.close_exec of the helper theory
ml/PIDE_MCP_Close.thy), once the prover confirms that the evaluation has stopped.

Once a step has run for pide_mcp_interim_goal_after_ms (default 5000; 0: never), or when it is interrupted, its method
runs again on the goal before the step, like `apply_inspect` with the limits of the helper theory
ml/PIDE_MCP_Apply_Inspect.thy (goal state after 200 ms): `get_state` shows that goal state, the interim goal, also
while the step still runs. The step itself is not interrupted for it.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Step_Limit {
  val commands: Set[String] = Set("apply", "by", "proof", "qed", "done", ".", "..")

  type Key = (Document_ID.Command, Document_ID.Exec)

  // evaluations interrupted at the limit (seconds), and evaluations without the limit (a retry)
  private val interrupted = Synchronized(Map.empty[Key, Double])
  private val unlimited = Synchronized(Set.empty[Key])
  private val keep = 2000

  private def key(snapshot: Document.Snapshot, cmd: Command): Option[Key] =
    PIDE_MCP_Command.eval_exec(snapshot, cmd).map(exec => (cmd.id, exec))

  def interrupted_at(snapshot: Document.Snapshot, cmd: Command): Option[Double] =
    key(snapshot, cmd).flatMap(interrupted.value.get)

  def is_unlimited(snapshot: Document.Snapshot, cmd: Command): Boolean =
    key(snapshot, cmd).exists(unlimited.value)

  private def interrupt(key: Key, limit: Double): Unit =
    interrupted.change(m => (if (m.size >= keep) m.drop(m.size - keep + 1) else m) + (key -> limit))

  private def allow(key: Key): Unit =
    unlimited.change(s => (if (s.size >= keep) s.drop(s.size - keep + 1) else s) + key)

  // interrupt a command: its evaluation and its print functions
  def cancel_command(session: PIDE_MCP_Session, snapshot: Document.Snapshot, cmd: Command, running_ms: Long): Unit = {
    PIDE_MCP_Command.Running_Clock.cancel(snapshot, cmd, running_ms)
    val execs = snapshot.state.assignments.get(snapshot.version.id)
      .flatMap(_.command_execs.get(cmd.id)).getOrElse(Nil)
    execs.foreach(session.session.cancel_exec)
  }

  // retries asked for: the old command, its node, offset and source; matched by the new command of the same text
  sealed case class Retry(old: Document_ID.Command, node_name: Document.Node.Name, offset: Text.Offset, source: String,
    asked: Time)
  private[mcp] val retries = Synchronized(List.empty[Retry])

  def retry_note(line: Int): String =
    s"retry this step once without the limit: `retry_without_limit` with this file and start_line $line; " +
      "raise the limit for all steps: `set_step_time_limit` (the interim goal still comes after " +
      "interim_goal_after_milliseconds, also below a raised limit); or replace the step by a cheaper one"

  /* interim goals: the goal state that the method of an interrupted step has reached after a moment (apply_inspect) */

  val close_helper: String = "$PIDE_MCP_HOME/ml/PIDE_MCP_Close.thy"
  val close_grace: Time = Time.seconds(2)
  val close_horizon: Time = Time.seconds(120)

  val helper: String = "$PIDE_MCP_HOME/ml/PIDE_MCP_Apply_Inspect.thy"
  val helper_import: String = "\"$PIDE_MCP_HOME/ml/PIDE_MCP_Apply_Inspect\""
  val operation: String = "pide_mcp_apply_inspect"
  val goal_max_lines: Int = PIDE_MCP_Command.State.default_goal_max_lines
  val uses_shown: Int = 10

  sealed case class Interim(
    method: String,
    outcome: String,  // pending, traced, finished, failed, unavailable
    time_ms: Long = 0,
    goal: String = "",
    error: String = "",
    uses: List[String] = Nil,
    loops: List[String] = Nil
  ) {
    // as part of an interrupted step (line) in get_state
    def interim_json(line: Int, goal_max_lines: Int = goal_max_lines): JSON.Object.T = {
      val within = if (time_ms > 0) s" within $time_ms ms" else ""  // a finished run has no time
      // theorems used in the diagnostic run, most frequent first, with how often
      val uses_json =
        JSON_Object.if_proper(uses.nonEmpty, JSON_Object("interim_theorems_used" ->
          (uses.take(uses_shown) ::: (if (uses.length > uses_shown)
            List(s"... (+${uses.length - uses_shown} more; the `apply_inspect` tool lists more)") else Nil))))
      (outcome match {
        case "pending" =>
          JSON_Object("interim_goal" -> s"pending: `$method` runs again with apply_inspect")
        case "traced" =>
          JSON_Object("interim_goal_time_ms" -> time_ms, "interim_goal" -> PIDE_MCP_Command.cap_goal(goal, goal_max_lines)) ++
            JSON_Object.if_proper(loops.nonEmpty, JSON_Object("interim_loops" -> loops)) ++
            JSON_Object("interim_goal_hint" ->
              (s"the goal state that `$method` had reached after about $time_ms ms (apply_inspect; within a simp " +
                "call the term it had rewritten to). For the state at another time, without changing the file: " +
                s"the `apply_inspect` tool with this file, start_line $line and timeout_ms (e.g. 1000)"))
        case "finished" if goal.nonEmpty =>
          JSON_Object("interim_goal" -> PIDE_MCP_Command.cap_goal(goal, goal_max_lines),
            "interim_goal_hint" ->
              s"`$method` finished in the diagnostic run (apply_inspect)$within: this is its resulting goal state")
        case "finished" =>
          JSON_Object("interim_goal" -> s"`$method` finished in the diagnostic run$within")
        case "failed" =>
          val failed = s"`$method` failed in the diagnostic run" +
            (if (error.nonEmpty) ": " + PIDE_MCP_Command.cap_goal(error, goal_max_lines, "error") else "")
          if (goal.nonEmpty)
            JSON_Object("interim_goal" -> PIDE_MCP_Command.cap_goal(goal, goal_max_lines),
              "interim_goal_hint" -> (failed + "; this is the goal state after its last step"))
          else JSON_Object("interim_goal" -> failed)
        case _ =>
          JSON_Object("interim_goal" -> ("unavailable" + (if (error.nonEmpty) ": " + error else "")))
      }) ++ (if (outcome == "pending") JSON_Object() else uses_json)
    }
  }

  // the command before cmd (the state after it is the goal of cmd)
  def previous(snapshot: Document.Snapshot, cmd: Command): Option[Command] =
    snapshot.node.commands.iterator.takeWhile(_ != cmd).filterNot(_.is_ignored).toList.lastOption

  /* Run the method like apply_inspect on the goal before cmd and stop it after time_ms (None: the helper theory's
     apply_inspect_trace_start): the goal state that it has reached then. */
  def inspect(
    session: PIDE_MCP_Session,
    cmd: Command,
    method: String,
    time_ms: Option[Long],
    timeout: Time,
    stop: () => Boolean = () => false,
    progress: Progress = new Progress
  ): Interim = {
    val snapshot = session.node_snapshot(cmd.node_name)
    val result =
      for {
        prev <- previous(snapshot, cmd)
        offset <- snapshot.node.command_start(prev)
      } yield {
        val res = new PIDE_MCP_Query(session, PIDE_MCP_Editor.Context(cmd.node_name, offset), operation, progress)
          .run(List(method, time_ms.fold("")(_.toString)), Some(timeout), _ => stop())
        val text = res.output.messages.filter(Protocol.is_writeln).map(PIDE_MCP_Util.elem_body_plain_text).mkString("\n")
        val errors = res.output.messages.filter(Protocol.is_error).map(PIDE_MCP_Util.elem_body_plain_text)
        if (res.outcome != PIDE_MCP_Query.Outcome.finished)
          Interim(method, "unavailable", error = s"the diagnostic run ${res.outcome}")
        else if (errors.nonEmpty) Interim(method, "failed", error = errors.head)
        else {
          val (head, goal) = split_lines(text).span(_ != "goal:")
          val (fields, uses) = head.span(_ != "uses:")
          def field(name: String): String =
            fields.collectFirst { case l if l.startsWith(name + ": ") => l.drop(name.length + 2) }.getOrElse("")
          Interim(method, field("outcome") match { case "" => "unavailable" case o => o },
            time_ms = field("time_ms").toLongOption.getOrElse(time_ms.getOrElse(0L)),
            goal = goal.drop(1).mkString("\n"), error = field("error"),
            uses = uses.drop(1).map(_.trim).filter(_.nonEmpty),
            loops = fields.filter(_.startsWith("loop: ")).map(_.stripPrefix("loop: ")))
        }
      }
    result.getOrElse(Interim(method, "unavailable", error = "no goal before this command"))
  }

  private val interims = Synchronized(Map.empty[Key, Interim])

  def interim(snapshot: Document.Snapshot, cmd: Command): Option[Interim] =
    key(snapshot, cmd).flatMap(interims.value.get)

  private def has_interim(key: Key): Boolean = interims.value.contains(key)

  private def set_interim(key: Key, interim: Interim): Unit =
    interims.change(m => (if (m.size >= keep) m.drop(m.size - keep + 1) else m) + (key -> interim))

  // the methods of a step: `apply (simp add: x)` -> `(simp add: x)`, `by auto simp` -> `auto`, `simp`
  def methods(source: String): List[String] = {
    @annotation.tailrec
    def loop(rest: String, acc: List[String]): List[String] =
      first_method(rest) match {
        case Some(m) if acc.length < 2 => loop(rest.trim.drop(m.length), m :: acc)
        case _ => acc.reverse
      }
    loop(source.trim.dropWhile(c => !c.isWhitespace && c != '('), Nil)
  }

  private def first_method(text: String): Option[String] = {
    val rest = text.trim
    if (rest.isEmpty || rest.startsWith("(*") || rest.startsWith("\\<comment>")) None
    else if (rest.head == '(') {
      var depth = 0
      var i = 0
      var end = -1
      while (i < rest.length && end < 0) {
        rest(i) match {
          case '(' => depth += 1
          case ')' => depth -= 1; if (depth == 0) end = i
          case _ =>
        }
        i += 1
      }
      Option.when(end > 0)(rest.substring(0, end + 1))
    }
    else Some(rest.takeWhile(c => !c.isWhitespace)).filter(_.nonEmpty)
  }

  // the method that the step runs on the goal before it (`done` has none of its own)
  def step_method(cmd: Command): Option[String] = {
    val ms = methods(cmd.source)
    cmd.span.name match {
      case "apply" | "qed" => ms.headOption
      case "proof" => ms.headOption.orElse(Some("standard"))
      case "by" => ms match {
        case List(m1, m2) => Some(s"($m1, $m2)")
        case m :: _ => Some(m)
        case Nil => None
      }
      case "." => Some("this")
      case ".." => Some("standard")
      case _ => None
    }
  }
}

class PIDE_MCP_Step_Limit(session: PIDE_MCP_Session) {
  import PIDE_MCP_Step_Limit._

  @volatile private var _limit: Double = session.options.real("pide_mcp_step_time_limit")
  def limit: Double = _limit

  // when the interim goal of a running step is computed (ms; 0: never, also not at an interrupt)
  @volatile private var _interim_after_ms: Long = session.options.int("pide_mcp_interim_goal_after_ms").toLong max 0L
  def interim_after_ms: Long = _interim_after_ms

  @volatile private var active = true

  // running steps of the current evaluations
  private val running = Synchronized(Map.empty[Key, (Command, Document.Node.Name)])

  // interim goals to run, and whether the helper theory is loaded (None while loading)
  private val helper_state = Synchronized(Option.empty[Boolean])
  private val pending = Synchronized(List.empty[(Key, Command, String)])
  private val query_timeout = Time.seconds(30)

  // interrupted steps whose closing status may still be missing: when interrupted, when last closed
  private val stopping = Synchronized(Map.empty[Key, (Command, Document.Node.Name, Time, Time)])
  private val close_state = Synchronized(Option.empty[Boolean])

  private val consumer =
    Session.Consumer[Session.Commands_Changed]("pide_mcp_step_limit") {
      case changed if active =>
        val snapshot = session.snapshot()
        val now = Time.now()
        retries.change(_.filter(r => now - r.asked < Time.seconds(120)))
        for {
          cmd <- changed.commands.iterator
          if commands(cmd.span.name) && cmd.node_name.is_theory && !session.is_base_session_theory(cmd.node_name)
          node_snapshot = snapshot.switch(cmd.node_name)
          k <- PIDE_MCP_Step_Limit.key(node_snapshot, cmd)
        } {
          // the new command of a retry: same text at the same place
          val retry = retries.value.find(r => r.old != cmd.id && r.node_name == cmd.node_name &&
            r.source == cmd.source && node_snapshot.node.command_start(cmd).contains(r.offset))
          for (r <- retry) { allow(k); retries.change(_.filterNot(_ == r)) }
          if (PIDE_MCP_Command.status(node_snapshot, cmd).is_running) running.change(_ + (k -> (cmd, cmd.node_name)))
          else running.change(_ - k)
        }
      case _ =>
    }

  // also without a limit: it can be set later (set_step_time_limit), and the steps running then are known
  session.session.commands_changed += consumer

  // the interim goal of a step, once: for the evaluation k
  private def queue_interim(k: Key, cmd: Command): Unit =
    if (interim_after_ms > 0 && !has_interim(k)) {
      for (m <- step_method(cmd) if helper_state.value != Some(false)) {
        set_interim(k, Interim(m, "pending"))
        pending.change(_ :+ (k, cmd, m))
      }
    }

  // the threads start with the first limit or interim threshold > 0
  private lazy val watcher: Thread =
    Isabelle_Thread.fork(name = s"pide_mcp_step_limit_${session.id}", daemon = true) {
      Exn.result {
        while (active) {
          val limit = this.limit
          val interim_after = interim_after_ms
          for {
            (k, (cmd, node_name)) <- running.value
            if (limit > 0 || interim_after > 0) && !interrupted.value.contains(k)
          } {
            Exn.result(session.node_snapshot(node_name)) match {
              case Exn.Res(snapshot) =>
                val status = PIDE_MCP_Command.status(snapshot, cmd)
                // the real run, also for a forked proof (`by` of a lemma), which PIDE does not time
                lazy val running_ms =
                  status.timings.sum(Date.now()).ms max PIDE_MCP_Command.Running_Clock.elapsed_ms(snapshot, cmd)
                val current = PIDE_MCP_Step_Limit.key(snapshot, cmd).contains(k) && status.is_running
                if (current && limit > 0 && !unlimited.value(k) && running_ms >= (limit * 1000).toLong) {
                  interrupt(k, limit)
                  cancel_command(session, snapshot, cmd, running_ms)
                  val now = Time.now()
                  stopping.change(_ + (k -> (cmd, node_name, now, now)))
                  queue_interim(k, cmd)
                }
                else if (current && interim_after > 0 && running_ms >= interim_after) queue_interim(k, cmd)
              case Exn.Exn(exn) if Exn.is_interrupt(exn) => throw exn
              case Exn.Exn(_) => running.change(_ - k)  // the node is gone (unloaded)
            }
          }
          close_stopped()
          Time.seconds(0.2).sleep()
        }
      }
      ()
    }

  /* An interrupted step that still shows as running after close_grace may have stopped without its closing status:
     the helper's protocol command sends that status if the prover confirms that the evaluation has stopped (otherwise
     nothing is sent), again every close_grace until the status ends or close_horizon has passed. */
  private def close_stopped(): Unit =
    if (close_state.value.contains(true)) {
      val now = Time.now()
      for ((k, (cmd, node_name, since, last)) <- stopping.value) {
        val snapshot = Exn.result(session.node_snapshot(node_name)) match {
          case Exn.Res(snapshot) => Some(snapshot)
          case Exn.Exn(exn) if Exn.is_interrupt(exn) => throw exn
          case Exn.Exn(_) => None
        }
        val still_running =
          snapshot.exists(snapshot => PIDE_MCP_Step_Limit.key(snapshot, cmd).contains(k) &&
            PIDE_MCP_Command.status(snapshot, cmd).is_running)
        if (!still_running || now - since > close_horizon) stopping.change(_ - k)
        else if (now - last >= close_grace) {
          for (st <- snapshot.flatMap(_.state.lookup_id(k._2))) {
            session.session.protocol_command_raw("PIDE_MCP.close_exec",
              List(Bytes(k._2.toString), Bytes(st.document_status.runs.toString),
                Bytes(st.document_status.forks.toString)))
          }
          stopping.change(_ + (k -> (cmd, node_name, since, now)))
        }
      }
    }

  /* interim goals of interrupted steps, one after the other */

  // load a helper theory of the server and wait until it is processed: whether it is ok
  private def load_theory(path: String): Boolean =
    Exn.result {
      val progress = new Progress
      val node_name = session.node_name(Path.explode(path).expand.implode)
      val deadline = Time.now() + Time.seconds(300)
      var snapshot = PIDE_MCP_Tool_Util.require_loaded_origin_snapshot(session, node_name, progress, 300)
      def status = PIDE_MCP_Theory.Status.statuses(session, snapshot, List(node_name)).head.overall_status
      while (active && status == Document_Status.Overall_Status.pending && Time.now() < deadline) {
        Time.seconds(0.5).sleep()
        snapshot = session.node_snapshot(node_name)
      }
      status == Document_Status.Overall_Status.ok
    } match {
      case Exn.Res(ok) => ok
      case Exn.Exn(_) => false
    }

  // the helper theory imports Main: only for sessions based on HOL (otherwise it would load all of HOL)
  lazy val helper_loaded: Boolean =
    List("Main", "HOL.Main").exists(session.resources.loaded_theory) && load_theory(helper)

  private lazy val interim_worker: Thread =
    Isabelle_Thread.fork(name = s"pide_mcp_interim_goal_${session.id}", daemon = true) {
      Exn.result {
        // the close helper (Pure) first, in every session
        close_state.change(_ => Some(load_theory(close_helper)))
        // loaded eagerly: the first interrupt should not wait for Apply_Inspect to load
        val ok = helper_loaded
        helper_state.change(_ => Some(ok))
        if (!ok) {
          for ((k, _, m) <- pending.change_result(q => (q, Nil)))
            set_interim(k, Interim(m, "unavailable", error = "the helper theory with apply_inspect could not be loaded"))
        }
        while (active && ok) {
          pending.change_result(q => (q.headOption, q.drop(1))) match {
            case Some((k, cmd, m)) =>
              val interim = Exn.result(inspect(session, cmd, m, None, query_timeout,
                  () => !PIDE_MCP_Step_Limit.key(session.node_snapshot(cmd.node_name), cmd).contains(k))) match {
                case Exn.Res(interim) => interim
                case Exn.Exn(exn) => Interim(m, "unavailable", error = Exn.message(exn))
              }
              set_interim(k, interim)
            case None => Time.seconds(0.2).sleep()
          }
        }
      }
      ()
    }

  private def start(): Unit = synchronized { watcher; interim_worker }

  if (limit > 0 || interim_after_ms > 0) start()

  // from now on (0: never); the old value
  def set_interim_after(ms: Long): Long = {
    val old = synchronized {
      val old = _interim_after_ms
      _interim_after_ms = ms max 0L
      old
    }
    if (ms > 0) start()
    old
  }

  /* Change the limit from now on: running steps are checked against it at once; steps interrupted at a lower
     limit than the new one run again (the first one in each theory, which re-runs the commands after it). Result:
     the old limit, and the re-run steps (theory, command, offset). */
  def set_limit(seconds: Double, progress: Progress): (Double, List[(Document.Node.Name, Command, Text.Offset)]) = {
    val old = synchronized {
      val old = _limit
      _limit = seconds
      old
    }
    if (seconds > 0) start()
    def below(at: Double): Boolean = seconds <= 0 || at < seconds
    val rerun =
      if (seconds > 0 && seconds <= old) Nil
      else {
        val nodes = session.snapshot().version.nodes.iterator.map(_._1)
          .filter(name => name.is_theory && !session.is_base_session_theory(name)).toList
        nodes.flatMap { name =>
          Exn.result(session.node_snapshot(name)) match {
            case Exn.Res(snapshot) =>
              snapshot.node.command_iterator().find { case (cmd, _) =>
                interrupted_at(snapshot, cmd).exists(below) && !PIDE_MCP_Command.status(snapshot, cmd).is_running
              }.map { case (cmd, start) => (name, cmd, start) }
            case Exn.Exn(exn) if Exn.is_interrupt(exn) => throw exn
            case Exn.Exn(_) => None
          }
        }
      }
    for ((name, cmd, start) <- rerun) session.reevaluate(name, Text.Range(start, start + cmd.length), progress)
    (old, rerun)
  }

  def stop(): Unit = {
    active = false
    session.session.commands_changed -= consumer
  }
}
