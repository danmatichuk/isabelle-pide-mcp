/*  Title:      PIDE_MCP/pide_mcp_step_limit.scala

A time limit for proof steps (pide_mcp_step_time_limit, default 5 s; 0: none): a step (`apply`, `by`, `proof`,
`qed`, `done`, `.`, `..`) whose real run reaches the limit is interrupted and shown as canceled.
`retry_without_limit` lets the command run again without the limit, once: the permission belongs to that
evaluation of the command, an edit of it (a new command) or a re-evaluation has the limit again. Only the MCP's
PIDE sessions are affected, not batch builds or the user's editor.

After the interrupt, the step's method runs again on the goal before the step, like `apply_inspect` with the
limits of the helper theory ml/PIDE_MCP_Apply_Inspect.thy (at most 5 s, goal states recorded from 200 ms on):
`get_state` shows the first recorded goal state, the interim goal.
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
    s"retry without the limit: `retry_without_limit` with this file and start_line $line; " +
      "or replace the step by a cheaper one"

  /* interim goals: the goal state that the method of an interrupted step has reached at apply_inspect's trace start */

  val helper: String = "$PIDE_MCP_HOME/ml/PIDE_MCP_Apply_Inspect.thy"
  val helper_import: String = "\"$PIDE_MCP_HOME/ml/PIDE_MCP_Apply_Inspect\""
  val operation: String = "pide_mcp_interim_goal"
  val goal_max_lines: Int = 40

  sealed case class Interim(
    method: String,
    outcome: String,  // pending, traced, finished, failed, unavailable
    time_ms: Long = 0,
    timeout_ms: Long = 0,
    start_ms: Long = 0,
    interval_ms: Long = 0,
    goal: String = "",
    error: String = ""
  ) {
    def json: JSON.Object.T = {
      val goal_lines = Line.logical_lines(goal)
      val shown_goal =
        if (goal_lines.length <= goal_max_lines) goal
        else (goal_lines.take(goal_max_lines) :+ s"... (+${goal_lines.length - goal_max_lines} lines)").mkString("\n")
      outcome match {
        case "pending" =>
          JSON_Object("interim_goal" -> s"pending: `$method` runs again with apply_inspect (a few seconds)")
        case "traced" =>
          JSON_Object("interim_goal_after_ms" -> time_ms, "interim_goal" -> shown_goal,
            "interim_goal_hint" ->
              (s"the goal state that `$method` had reached after about $time_ms ms, traced by apply_inspect " +
                "(within a simp call: the term it had rewritten to). " +
                s"To get it in your proof: add $helper_import to the theory imports (it sets the same limits), " +
                s"write `apply_inspect $method` in place of the step, and add `back` lines after it: each shows the " +
                s"goal state $interval_ms ms later, with the theorems used in between, up to $timeout_ms ms. " +
                "Remove them and the import afterwards"))
        case "finished" =>
          JSON_Object("interim_goal" -> s"`$method` finished within the apply_inspect limit in the diagnostic run")
        case "failed" =>
          JSON_Object("interim_goal" -> (s"`$method` failed in the diagnostic run" + (if (error.nonEmpty) ": " + error else "")))
        case _ =>
          JSON_Object("interim_goal" -> ("unavailable" + (if (error.nonEmpty) ": " + error else "")))
      }
    }
  }

  private val interims = Synchronized(Map.empty[Key, Interim])

  def interim(snapshot: Document.Snapshot, cmd: Command): Option[Interim] =
    key(snapshot, cmd).flatMap(interims.value.get)

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

  val limit: Double = session.options.real("pide_mcp_step_time_limit")

  @volatile private var active = true

  // running steps of the current evaluations
  private val running = Synchronized(Map.empty[Key, (Command, Document.Node.Name)])

  // interim goals to run, and whether the helper theory is loaded (None while loading)
  private val helper_state = Synchronized(Option.empty[Boolean])
  private val pending = Synchronized(List.empty[(Key, Command, String)])
  private val query_timeout = Time.seconds(30)

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

  if (limit > 0) session.session.commands_changed += consumer

  private val watcher =
    if (limit <= 0) None
    else Some(Isabelle_Thread.fork(name = s"pide_mcp_step_limit_${session.id}", daemon = true) {
      Exn.result {
        while (active) {
          for ((k, (cmd, node_name)) <- running.value if !unlimited.value(k) && !interrupted.value.contains(k)) {
            Exn.result(session.node_snapshot(node_name)) match {
              case Exn.Res(snapshot) =>
                val status = PIDE_MCP_Command.status(snapshot, cmd)
                // the real run, also for a forked proof (`by` of a lemma), which PIDE does not time
                lazy val running_ms =
                  status.timings.sum(Date.now()).ms max PIDE_MCP_Command.Running_Clock.elapsed_ms(snapshot, cmd)
                if (PIDE_MCP_Step_Limit.key(snapshot, cmd).contains(k) && status.is_running &&
                    running_ms >= (limit * 1000).toLong) {
                  interrupt(k, limit)
                  cancel_command(session, snapshot, cmd, running_ms)
                  for (m <- step_method(cmd) if helper_state.value != Some(false)) {
                    set_interim(k, Interim(m, "pending"))
                    pending.change(_ :+ (k, cmd, m))
                  }
                }
              case Exn.Exn(exn) if Exn.is_interrupt(exn) => throw exn
              case Exn.Exn(_) => running.change(_ - k)  // the node is gone (unloaded)
            }
          }
          Time.seconds(0.2).sleep()
        }
      }
      ()
    })

  /* interim goals of interrupted steps, one after the other */


  // the helper theory imports Main: only for sessions based on HOL (otherwise it would load all of HOL)
  private def load_helper(): Boolean =
    List("Main", "HOL.Main").exists(session.resources.loaded_theory) &&
      (Exn.result {
        val progress = new Progress
        val node_name = session.node_name(Path.explode(helper).expand.implode)
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
      })

  private def run_interim(k: Key, cmd: Command, method: String): Interim = {
    val snapshot = session.node_snapshot(cmd.node_name)
    val before = snapshot.node.commands.iterator.takeWhile(_ != cmd).filterNot(_.is_ignored).toList
    val result =
      for {
        prev <- before.lastOption
        offset <- snapshot.node.command_start(prev)
      } yield {
        val res = new PIDE_MCP_Query(session, PIDE_MCP_Editor.Context(cmd.node_name, offset), operation, new Progress)
          .run(List(method), Some(query_timeout),
            _ => !PIDE_MCP_Step_Limit.key(session.node_snapshot(cmd.node_name), cmd).contains(k))
        val text = res.output.messages.filter(Protocol.is_writeln).map(PIDE_MCP_Util.elem_body_plain_text).mkString("\n")
        val errors = res.output.messages.filter(Protocol.is_error).map(PIDE_MCP_Util.elem_body_plain_text)
        if (res.outcome != PIDE_MCP_Query.Outcome.finished)
          Interim(method, "unavailable", error = s"the diagnostic run ${res.outcome}")
        else if (errors.nonEmpty) Interim(method, "failed", error = errors.head)
        else {
          val (head, goal) = split_lines(text).span(_ != "goal:")
          def field(name: String): String =
            head.collectFirst { case l if l.startsWith(name + ": ") => l.drop(name.length + 2) }.getOrElse("")
          def ms(name: String): Long = field(name).toLongOption.getOrElse(0L)
          Interim(method, field("outcome") match { case "" => "unavailable" case o => o },
            time_ms = ms("time_ms"), timeout_ms = ms("timeout_ms"), start_ms = ms("start_ms"),
            interval_ms = ms("interval_ms"),
            goal = goal.drop(1).mkString("\n"), error = field("error"))
        }
      }
    result.getOrElse(Interim(method, "unavailable", error = "no goal before the step"))
  }

  private val interim_worker =
    if (limit <= 0) None
    else Some(Isabelle_Thread.fork(name = s"pide_mcp_interim_goal_${session.id}", daemon = true) {
      Exn.result {
        // loaded eagerly: the first interrupt should not wait for Apply_Inspect to load
        val ok = load_helper()
        helper_state.change(_ => Some(ok))
        if (!ok) {
          for ((k, _, m) <- pending.change_result(q => (q, Nil)))
            set_interim(k, Interim(m, "unavailable", error = "the helper theory with apply_inspect could not be loaded"))
        }
        while (active && ok) {
          pending.change_result(q => (q.headOption, q.drop(1))) match {
            case Some((k, cmd, m)) =>
              val interim = Exn.result(run_interim(k, cmd, m)) match {
                case Exn.Res(interim) => interim
                case Exn.Exn(exn) => Interim(m, "unavailable", error = Exn.message(exn))
              }
              set_interim(k, interim)
            case None => Time.seconds(0.2).sleep()
          }
        }
      }
      ()
    })

  def stop(): Unit = {
    active = false
    session.session.commands_changed -= consumer
  }
}
