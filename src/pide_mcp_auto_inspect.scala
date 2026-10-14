/*  Title:      PIDE_MCP/pide_mcp_auto_inspect.scala

Automatic inspection of slow or failed proof steps: the method of an `apply`/`by` step that runs for
pide_mcp_auto_inspect_after seconds, or that fails, is run again in the background by the apply_inspect
instrumentation on the same goal (the state after the command before the step). get_state shows the most
used rules, loop warnings and a log file with every use in order.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Auto_Inspect {
  val operation: String = "pide_mcp_apply_trace"
  val step_commands: Set[String] = Set("apply", "by")
  val top_rules: Int = 5
  val trace_max: Int = 5000
  val results_keep: Int = 200
  val pending_max: Int = 8

  type Key = (Document_ID.Command, Document_ID.Exec)

  sealed case class Result(
    trigger: String,
    outcome: String,
    loops: List[String],
    top: List[String],
    uses: Int,
    log: Option[String],
    elapsed: Time = Time.zero
  ) {
    def json: JSON.Object.T =
      JSON_Object("trigger" -> trigger, "outcome" -> outcome, "top_rules" -> top, "uses" -> uses,
        "probe_seconds" -> (elapsed.ms / 100).toDouble / 10) ++
        JSON_Object.if_proper(loops.nonEmpty, JSON_Object("loops" -> loops)) ++
        JSON.optional("log" -> log)
  }

  // results by evaluation: an edited (re-evaluated) command has a new key; None: the probe is running
  private val results = Synchronized(Map.empty[Key, Option[Result]])
  private val order = Synchronized(Vector.empty[Key])

  def key(snapshot: Document.Snapshot, cmd: Command): Option[Key] =
    PIDE_MCP_Command.eval_exec(snapshot, cmd).map(exec => (cmd.id, exec))

  def get(snapshot: Document.Snapshot, cmd: Command): Option[Option[Result]] =
    key(snapshot, cmd).flatMap(results.value.get)

  def known(key: Key): Boolean = results.value.contains(key)

  def json(snapshot: Document.Snapshot, cmd: Command): Option[JSON.Object.T] =
    get(snapshot, cmd).map {
      case None => JSON_Object("status" -> "running")
      case Some(result) => result.json
    }

  private[mcp] def start(key: Key): Boolean =
    results.change_result { map =>
      if (map.contains(key)) (false, map)
      else {
        order.change(keys => (keys :+ key).takeRight(results_keep))
        val keep = order.value.toSet
        (true, map.filter(p => keep(p._1)) + (key -> None))
      }
    }

  private[mcp] def finish(key: Key, result: Option[Result]): Unit =
    results.change(map => result.fold(map - key)(r => map + (key -> Some(r))))

  // the first method of a step: `apply (simp add: x)` -> `(simp add: x)`, `by auto simp` -> `auto`
  def method_text(source: String): Option[String] = {
    val rest = source.trim.dropWhile(c => !c.isWhitespace && c != '(').trim
    if (rest.isEmpty) None
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

  // the text of the trace operation: outcome, loop lines, "counts:" lines, "trace:" lines
  def parse(trigger: String, text: String, session_id: String): Result = {
    val lines = split_lines(text)
    val (head, rest) = lines.span(_ != "counts:")
    val (counts, trace) = rest.drop(1).span(_ != "trace:")
    val count_lines = counts.filter(_.startsWith("  ")).map(_.trim)
    val top = count_lines.take(top_rules).map { l =>
      val (n, name) = l.span(_ != ' ')
      s"${name.trim} ×$n"
    }
    val uses = count_lines.flatMap(_.takeWhile(_ != ' ').toIntOption).sum
    val log = Option.when(count_lines.nonEmpty)(Exn.result(Tool_Apply_Inspect.write_file(session_id,
      cat_lines(List(s"trigger: $trigger", "outcome: " + head.headOption.getOrElse("?")) :::
        head.drop(1) ::: ("counts (uses, theorem):" :: count_lines.map("  " + _)) :::
        ("trace (in order, `source: theorem`):" :: trace.drop(1))) + "\n", prefix = "auto_inspect")) match {
      case Exn.Res(path) => Some(path)
      case Exn.Exn(_) => None
    }).flatten
    Result(trigger, head.headOption.getOrElse("no result").stripPrefix("error: "),
      head.drop(1).filter(_.startsWith("***")), top, uses, log)
  }
}

class PIDE_MCP_Auto_Inspector(session: PIDE_MCP_Session) {
  import PIDE_MCP_Auto_Inspect._

  private val after: Time = Time.seconds(session.options.real("pide_mcp_auto_inspect_after"))
  private val method_timeout: Int = session.options.int("pide_mcp_auto_inspect_timeout")

  @volatile private var active = true
  @volatile private var ready = false  // helper theory loaded

  // steps that run, since when (by evaluation)
  private val running = Synchronized(Map.empty[Key, (Command, Time)])
  // probes to run, oldest first (bounded: the oldest are dropped)
  private val pending = Synchronized(Vector.empty[(Key, Command, String)])

  private def enqueue(key: Key, cmd: Command, trigger: String): Unit =
    pending.change(queue =>
      if (queue.exists(_._1 == key) || known(key)) queue
      else (queue :+ (key, cmd, trigger)).takeRight(pending_max))

  private val consumer =
    Session.Consumer[Session.Commands_Changed]("pide_mcp_auto_inspect") {
      case changed if active =>
        val snapshot = session.snapshot()
        val now = Time.now()
        for {
          cmd <- changed.commands.iterator
          if step_commands(cmd.span.name) && cmd.node_name.is_theory && !session.is_base_session_theory(cmd.node_name)
          node_snapshot = snapshot.switch(cmd.node_name)
          key <- PIDE_MCP_Auto_Inspect.key(node_snapshot, cmd)
        } {
          val status = PIDE_MCP_Command.status(node_snapshot, cmd)
          if (status.is_running) running.change(m => if (m.contains(key)) m else m + (key -> (cmd, now)))
          else {
            running.change(_ - key)
            if (status.is_failed && PIDE_MCP_Command.Running_Clock.canceled_ms(node_snapshot, cmd).isEmpty)
              enqueue(key, cmd, "failed")
          }
        }
      case _ =>
    }
  session.session.commands_changed += consumer

  // the probe is still wanted: same evaluation of a command still in its node, not cancelled
  private def valid(key: Key, cmd: Command): Boolean = {
    val snapshot = session.node_snapshot(cmd.node_name)
    snapshot.node.commands.contains(cmd) &&
      PIDE_MCP_Auto_Inspect.key(snapshot, cmd).contains(key) &&
      PIDE_MCP_Command.Running_Clock.canceled_ms(snapshot, cmd).isEmpty
  }

  private def probe(key: Key, cmd: Command, trigger: String): Unit = {
    val snapshot = session.node_snapshot(cmd.node_name)
    val method = method_text(cmd.source)
    // the goal of the step: the state after the command before it
    val before = snapshot.node.commands.iterator.takeWhile(_ != cmd).filterNot(_.is_ignored).toList.lastOption
    for (m <- method; prev <- before; offset <- snapshot.node.command_start(prev) if start(key)) {
      val result = Exn.result {
        val query = new PIDE_MCP_Query(session, PIDE_MCP_Editor.Context(cmd.node_name, offset), operation, new Progress)
        val res = query.run(List(m, method_timeout.toString, trace_max.toString),
          Some(Time.seconds(method_timeout + 20)), _ => !valid(key, cmd))
        Option.when(res.outcome == PIDE_MCP_Query.Outcome.finished && valid(key, cmd))(
          parse(trigger, res.text, session.id).copy(elapsed = res.elapsed))
      }
      finish(key, result match { case Exn.Res(r) => r; case Exn.Exn(_) => None })
    }
  }

  private val worker = Isabelle_Thread.fork(name = s"pide_mcp_auto_inspect_${session.id}", daemon = true) {
    Exn.result {
      // load the helper eagerly: its first on-demand load took about 15 s, longer than a probe may take
      ready = Exn.result(Tool_Suggest_Facts.ensure_helper(session, new Progress,
        "$PIDE_MCP_HOME/ml/PIDE_MCP_Apply_Inspect.thy", "automatic inspection")) match {
        case Exn.Res(_) => true
        case Exn.Exn(_) => false
      }
      while (active && ready) {
        val now = Time.now()
        for ((key, (cmd, since)) <- running.value if now - since >= after) {
          running.change(_ - key)
          enqueue(key, cmd, s"running for ${after.seconds.toInt} s")
        }
        pending.change_result(queue => (queue.headOption, queue.drop(1))) match {
          case Some((key, cmd, trigger)) => if (valid(key, cmd)) probe(key, cmd, trigger)
          case None => Time.seconds(0.5).sleep()
        }
      }
    }
    ()
  }

  def stop(): Unit = {
    active = false
    session.session.commands_changed -= consumer
  }
}
