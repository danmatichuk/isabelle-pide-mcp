/*  Title:      PIDE_MCP/pide_mcp_auto_inspect.scala

Automatic inspection of slow or failed proof steps: the method of an `apply`/`by` step that runs for
pide_mcp_auto_inspect_after seconds, or of any proof command that fails (`apply`, `by`, `proof`, `qed`, `.`,
`..`; for `done` the last `apply` before it), is run again in the background by the apply_inspect
instrumentation on the same goal (the state after the command before it). For a failure, Sledgehammer's
relevance filter also suggests facts for that goal. get_state shows the most used rules, loop warnings,
possibly relevant facts and a log file with every use in order and all of them.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Auto_Inspect {
  val operation: String = "pide_mcp_apply_trace"
  val step_commands: Set[String] = Set("apply", "by")  // probed when they run long
  val fail_commands: Set[String] = Set("apply", "by", "done", "qed", "proof", ".", "..")  // probed when they fail
  val terminal_commands: Set[String] = Set("by", "done", "qed", ".", "..")  // fail when goals are left
  val suggest_max: Int = 20
  val suggest_shown: Int = 5
  val top_rules: Int = 5
  val trace_max: Int = 5000
  val results_keep: Int = 200
  val pending_max: Int = 16

  type Key = (Document_ID.Command, Document_ID.Exec)

  sealed case class Result(
    trigger: String,
    outcome: String,
    loops: List[String],
    top: List[String],
    uses: Int,
    log: Option[String],
    elapsed: Time = Time.zero,
    method: Option[String] = None,
    suggested: List[String] = Nil,
    dominant: Option[String] = None,
    subgoals_left: Option[Int] = None,  // a terminal step: the goals left after its (successful) method
    timed_out: Option[Int] = None,  // the diagnostic stopped at its time limit (seconds)
    scenario: Option[Int] = None,  // a failed terminal step, by the real methods: 1, 2, 3 (see scenario_name)
    two_methods: Boolean = false,  // `by m1 m2`
    goal_subgoals: Option[Int] = None,  // the number of subgoals of the scenario's goal
    goal_state: Option[String] = None  // the goal of the scenario (capped)
  ) {
    def scenario_name: Option[String] =
      scenario.map { case 1 => "method_fails" case 2 => "second_method_fails" case _ => "proof_not_finished" }

    private def subgoals: String = subgoals_left.fold("subgoals")(n => s"$n subgoal${if (n == 1) "" else "s"}")

    // the goal of the scenario, for the suggestions and the goal shown
    def goal_label: Option[String] =
      scenario.map {
        case 1 => if (two_methods) "the goal before the first method" else "the goal before the method"
        case 2 => "the goal after the first method"
        case _ => s"the $subgoals left"
      }

    // the relevance filter looks at subgoal 1 only
    def suggested_for: Option[String] =
      if (scenario.nonEmpty)
        goal_subgoals.orElse(subgoals_left) match {
          case Some(n) if n > 1 =>
            if (scenario.contains(3)) Some(s"subgoal 1 of the $n left") else goal_label.map("subgoal 1 of " + _)
          case _ => goal_label
        }
      else subgoals_left.filter(_ > 0).map(n => if (n > 1) s"subgoal 1 of the $n left" else s"the $subgoals left")

    /* times: of the real run (finished_ms, real_finished_ms; the trigger) or of the diagnostic, the instrumented
       copy of the method, which is slower (probe_seconds, diagnostic_*) */
    def json(finished_ms: Option[Long] = None): JSON.Object.T = {
      val real_ms = finished_ms.filter(_ => timed_out.nonEmpty)
      val outcome1 =
        real_ms match {
          case Some(ms) =>
            "the step finished OK" + (if (ms > 0) s" in ${ms / 1000} s (real run)" else "") +
              s"; diagnostic for slow method timed out at ${timed_out.get}s"
          case None => outcome
        }
      JSON_Object("trigger" -> trigger, "uses" -> uses, "probe_seconds" -> (elapsed.ms / 100).toDouble / 10) ++
        JSON.optional("method" -> method) ++
        JSON.optional("diagnostic_timed_out_at_seconds" -> timed_out) ++
        JSON.optional("real_finished_ms" -> real_ms) ++
        JSON_Object.if_proper(outcome.nonEmpty, JSON_Object("outcome" -> outcome1, "top_rules" -> top)) ++
        JSON.optional("dominant_rule" -> dominant) ++
        JSON.optional("subgoals_left" -> subgoals_left) ++
        JSON.optional("scenario" -> scenario_name) ++
        JSON.optional("goal_state" -> goal_state) ++
        JSON.optional("goal_state_for" -> goal_label.filter(_ => goal_state.nonEmpty)) ++
        JSON_Object.if_proper(loops.nonEmpty, JSON_Object("loops" -> loops)) ++
        JSON_Object.if_proper(suggested.nonEmpty, JSON_Object("suggested_facts" -> suggested)) ++
        JSON.optional("suggested_for" -> suggested_for.filter(_ => suggested.nonEmpty)) ++
        JSON.optional("log" -> log)
    }
  }

  /* infrastructure rules: of Pure or the theory HOL itself, class relations and arities (`*.super`, `arity_*`,
     `classrel_*`), unnamed rules, and the sort membership rules the trace marks (OFCLASS statements, the rules
     Axclass records for a class): counted, but not shown as top rules */
  def infrastructure(key: String): Boolean = {
    val names = Long_Name.explode(key)
    val base = names.lastOption.getOrElse("")
    key.startsWith("(") || names.headOption.exists(Set("Pure", "HOL")) ||
      names.exists(n => n == "super" || n.startsWith("super(")) || base.startsWith("arity_") || base.startsWith("classrel_")
  }

  // results by evaluation: an edited (re-evaluated) command has a new key; None: the probe is running (since when)
  private val results = Synchronized(Map.empty[Key, Option[Result]])
  private val started = Synchronized(Map.empty[Key, Time])
  private val order = Synchronized(Vector.empty[Key])

  def key(snapshot: Document.Snapshot, cmd: Command): Option[Key] =
    PIDE_MCP_Command.eval_exec(snapshot, cmd).map(exec => (cmd.id, exec))

  def get(snapshot: Document.Snapshot, cmd: Command): Option[Option[Result]] =
    key(snapshot, cmd).flatMap(results.value.get)

  def known(key: Key): Boolean = results.value.contains(key)

  // probed while running, now failed: the suggestions are still to come
  def needs_suggestions(key: Key): Boolean =
    results.value.get(key).exists(_.exists(r => r.suggested.isEmpty && r.trigger.startsWith("real run")))

  private[mcp] def add_suggestions(key: Key, suggestions: List[String], session_id: String): Unit =
    if (suggestions.nonEmpty) {
      val log = Exn.result(Tool_Apply_Inspect.write_file(session_id,
        cat_lines("possibly relevant facts (Sledgehammer's relevance filter, most relevant first):" :: suggestions.map("  " + _)) + "\n",
        prefix = "auto_suggest")) match { case Exn.Res(p) => Some(p); case Exn.Exn(_) => None }
      results.change(map => map.get(key) match {
        case Some(Some(r)) =>
          map + (key -> Some(r.copy(trigger = r.trigger + ", then failed",
            suggested = suggestions.take(suggest_shown).map(_.takeWhile(_ != ':').trim),
            log = r.log.map(l => l + log.fold("")(" and " + _)).orElse(log))))
        case _ => map
      })
    }

  def json(snapshot: Document.Snapshot, cmd: Command, finished_ms: Option[Long] = None): Option[JSON.Object.T] =
    get(snapshot, cmd).map {
      case None =>
        JSON_Object("status" -> "running") ++
          JSON.optional("diagnostic_running_seconds" ->
            key(snapshot, cmd).flatMap(started.value.get).map(t => (Time.now() - t).ms / 1000))
      case Some(result) => result.json(finished_ms)
    }

  private[mcp] def start(key: Key): Boolean =
    results.change_result { map =>
      if (map.contains(key)) (false, map)
      else {
        order.change(keys => (keys :+ key).takeRight(results_keep))
        val keep = order.value.toSet
        started.change(m => m.filter(p => keep(p._1)) + (key -> Time.now()))
        (true, map.filter(p => keep(p._1)) + (key -> None))
      }
    }

  private[mcp] def finish(key: Key, result: Option[Result]): Unit =
    results.change(map => result.fold(map - key)(r => map + (key -> Some(r))))

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

  def method_text(source: String): Option[String] = methods(source).headOption

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

  /* the text of the trace operation: outcome, loop lines, "subgoals left: N", "counts:" lines, "trace:" lines,
     "suggested:" lines (for the goal left); suggestions: for the goal before the step, used if there are none for
     the goal left */
  def parse(trigger: String, text: String, session_id: String, suggestions0: List[String] = Nil,
    methods: Int = 0, done: Boolean = false
  ): Result = {
    // the goal comes last, with all its lines
    val (lines, goal_lines) = split_lines(text).span(_ != "goal:")
    val goal_state = Option.when(goal_lines.length > 1)(
      PIDE_MCP_Command.cap_goal(cat_lines(goal_lines.drop(1)), PIDE_MCP_Command.State.default_goal_max_lines))
    def head_int(prefix: String): Option[Int] =
      lines.collectFirst { case l if l.startsWith(prefix) => l.stripPrefix(prefix).trim.toIntOption }.flatten
    val scenario = head_int("scenario: ")
    val goal_subgoals = head_int("subgoals: ")
    val (head, rest) = lines.span(_ != "counts:")
    val (counts, rest1) = rest.drop(1).span(_ != "trace:")
    val (trace, suggested_left) = rest1.span(_ != "suggested:")
    val subgoals_left = head.collectFirst { case l if l.startsWith("subgoals left: ") => l.stripPrefix("subgoals left: ").trim.toIntOption }.flatten
    val left_suggestions = suggested_left.drop(1).filter(_.startsWith("  ")).map(_.trim)
    val suggestions = if (left_suggestions.nonEmpty) left_suggestions else suggestions0
    // "N display<TAB>full name"
    val entries = counts.filter(_.startsWith("  ")).map(_.trim).flatMap { l =>
      val (n, rest) = l.span(_ != ' ')
      val (display, key_flag) = rest.trim.span(_ != '\t')
      val (key, flag) = key_flag.drop(1).span(_ != '\t')
      n.toIntOption.map(k => (k, display.trim, key.trim, flag.trim.nonEmpty || infrastructure(key.trim)))
    }
    // the non-infrastructure theorems first (most used first), then the infrastructure ones under their own header
    val count_lines = {
      val (infra, proper) = entries.partition(_._4)
      proper.map { case (n, display, _, _) => s"$n $display" } :::
        (if (infra.isEmpty) Nil
         else "infrastructure (Pure/HOL, class/sort rules; not shown in top rules):" ::
          infra.map { case (n, display, _, _) => s"  $n $display" })
    }
    val proper = entries.filterNot(_._4)
    val top = proper.take(top_rules).map { case (n, display, _, _) => s"$display ×$n" }
    val uses = entries.map(_._1).sum
    val proper_uses = proper.map(_._1).sum
    // one rule doing most of the work: a rewrite or step repeated on many instances
    val dominant = proper match {
      case (n1, display, _, _) :: rest if n1 >= 10 && (2 * n1 >= proper_uses || rest.headOption.forall(n1 >= 3 * _._1)) =>
        Some(s"most uses: `$display` ×$n1 (of $proper_uses non-infrastructure uses): consider a more direct rule for its instances")
      case _ => None
    }
    val inspected = text.nonEmpty
    val outcome0 = head.headOption.getOrElse("no result").stripPrefix("error: ")
    // the time limit of the diagnostic: says nothing about the real run, which is not instrumented
    val timed_out = """did not finish within (\d+) s""".r.findPrefixMatchOf(outcome0).flatMap(_.group(1).toIntOption)
    val left = subgoals_left.fold("")(n => s"$n subgoal${if (n == 1) "" else "s"} left")
    val two = methods == 2
    // a failed terminal step: what the real methods did, rather than the outcome of the diagnostic
    val outcome =
      scenario match {
        case Some(1) => if (two) "the first method fails" else "the method fails"
        case Some(2) => "the first method applies; the second method fails"
        case Some(_) =>
          (if (two) "both methods apply" else if (done) "the last `apply` applies" else "the method applies") +
            s", but the proof is not finished: $left"
        case None =>
          subgoals_left match {
            case Some(n) if n > 0 && outcome0 == "succeeds" => s"succeeds but does not finish the proof: $left"
            case _ => timed_out.fold(outcome0)(n => s"diagnostic for slow method timed out at ${n}s")
          }
      }
    val result0 =
      Result(trigger, if (!inspected) "" else outcome,
        head.drop(1).filter(_.startsWith("***")), top, uses, None,
        suggested = suggestions.take(suggest_shown).map(s => s.takeWhile(_ != ':').trim), dominant = dominant,
        subgoals_left = subgoals_left, timed_out = timed_out, scenario = scenario, two_methods = two,
        goal_subgoals = goal_subgoals, goal_state = goal_state)
    val goal_for = result0.suggested_for.filter(_ => left_suggestions.nonEmpty)
    val suggested_lines =
      if (suggestions.isEmpty) Nil
      else (s"possibly relevant facts${goal_for.fold("")(" for " + _)} (Sledgehammer's relevance filter, most relevant first):" ::
        suggestions.map("  " + _))
    val goal_log_lines =
      goal_state.toList.flatMap(g => s"${result0.goal_label.getOrElse("goal")}:" :: split_lines(g).map("  " + _))
    val log = Option.when(count_lines.nonEmpty || suggestions.nonEmpty)(Exn.result(Tool_Apply_Inspect.write_file(session_id,
      cat_lines(List(s"trigger: $trigger") :::
        (if (!inspected) Nil
         else ("outcome: " + outcome) :: head.drop(1).filterNot(l => l.startsWith("scenario: ") || l.startsWith("subgoals")) :::
          goal_log_lines :::
          ("counts (uses, theorem):" :: count_lines.map("  " + _))) :::
        suggested_lines :::
        (if (!inspected) Nil else "trace (in order, `source: theorem`):" :: trace.drop(1))) + "\n",
      prefix = "auto_inspect")) match {
      case Exn.Res(path) => Some(path)
      case Exn.Exn(_) => None
    }).flatten
    result0.copy(log = log)
  }
}

class PIDE_MCP_Auto_Inspector(session: PIDE_MCP_Session) {
  import PIDE_MCP_Auto_Inspect._

  private val after: Time = Time.seconds(session.options.real("pide_mcp_auto_inspect_after"))
  private val method_timeout: Int = session.options.int("pide_mcp_auto_inspect_timeout")

  @volatile private var active = true
  @volatile private var ready = false  // helper theory loaded
  @volatile private var suggest_ready = false

  // steps that run, since when (by evaluation)
  private val running = Synchronized(Map.empty[Key, (Command, Time)])
  // probes to run, oldest first (bounded: the oldest are dropped)
  private val pending = Synchronized(Vector.empty[(Key, Command, String)])

  // one probe per evaluation and kind (failed, or slow: running long or interrupted at the step time limit)
  private def enqueue(key: Key, cmd: Command, trigger: String): Unit =
    pending.change(queue =>
      if (queue.exists(q => q._1 == key && (q._3 == "failed") == (trigger == "failed")) || skipped.value(key) ||
          known(key) && !(trigger == "failed" && needs_suggestions(key))) queue
      else (queue :+ (key, cmd, trigger)).takeRight(pending_max))

  private val consumer =
    Session.Consumer[Session.Commands_Changed]("pide_mcp_auto_inspect") {
      case changed if active =>
        val snapshot = session.snapshot()
        val now = Time.now()
        for {
          cmd <- changed.commands.iterator
          if (step_commands(cmd.span.name) || fail_commands(cmd.span.name)) && cmd.node_name.is_theory &&
            !session.is_base_session_theory(cmd.node_name)
          node_snapshot = snapshot.switch(cmd.node_name)
          key <- PIDE_MCP_Auto_Inspect.key(node_snapshot, cmd)
        } {
          val status = PIDE_MCP_Command.status(node_snapshot, cmd)
          if (PIDE_MCP_Step_Limit.is_unlimited(node_snapshot, cmd)) running.change(_ - key)  // a retry: no inspection
          else if (status.is_running) {
            if (step_commands(cmd.span.name)) running.change(m => if (m.contains(key)) m else m + (key -> (cmd, now)))
          }
          else {
            running.change(_ - key)
            val limit = PIDE_MCP_Step_Limit.interrupted_at(node_snapshot, cmd)
            if (limit.nonEmpty && fail_commands(cmd.span.name))
              enqueue(key, cmd, s"interrupted at the ${PIDE_MCP_Util.seconds_text(limit.get)} s limit")
            else if (fail_commands(cmd.span.name) && status.is_failed &&
                PIDE_MCP_Command.Running_Clock.canceled_ms(node_snapshot, cmd).isEmpty)
              enqueue(key, cmd, "failed")
          }
        }
      case _ =>
    }
  session.session.commands_changed += consumer

  // the probe is still wanted: same evaluation of a command still in its node, not cancelled (except at the step limit)
  private def valid(key: Key, cmd: Command): Boolean = {
    val snapshot = session.node_snapshot(cmd.node_name)
    snapshot.node.commands.contains(cmd) &&
      PIDE_MCP_Auto_Inspect.key(snapshot, cmd).contains(key) &&
      (PIDE_MCP_Command.Running_Clock.canceled_ms(snapshot, cmd).isEmpty ||
        PIDE_MCP_Step_Limit.interrupted_at(snapshot, cmd).nonEmpty)
  }

  // follow-on failures (an earlier command of the same proof failed) are not probed: they would repeat its result
  private val skipped = Synchronized(Set.empty[Key])

  private def kind(snapshot: Document.Snapshot, cmd: Command): Option[String] =
    snapshot.node.syntax.flatMap(_.keywords.kinds.get(cmd.span.name))

  /* what to replay for a command, at which command (the state after it is the goal), a note, and for a terminal
     step its methods (`by m1 m2`: both) for the real replay that tells the scenario: None = nothing to replay */
  private def plan(snapshot: Document.Snapshot, cmd: Command, before: List[Command])
    : Option[(String, Command, String, List[String])] = {
    def at_prev(m: String, ms: List[String] = Nil) = before.lastOption.map(prev => (m, prev, "", ms))
    val ms = methods(cmd.source)
    cmd.span.name match {
      case "apply" | "proof" => ms.headOption.flatMap(at_prev(_))
      case "qed" => ms.headOption.flatMap(m => at_prev(m, List(m)))
      case "by" => ms match {
        case List(m1, m2) => at_prev(s"($m1, $m2)", ms)
        case m :: _ => at_prev(m, List(m))
        case Nil => None
      }
      case "." => at_prev("this", List("this"))
      case ".." => at_prev("standard", List("standard"))
      case "done" =>  // no method of its own: the last `apply` of the proof, on the goal it got
        val proof = before.reverse.takeWhile(c => !kind(snapshot, c).exists(k => Keyword.theory_goal(k) || Keyword.proof_goal(k)))
        proof.find(_.span.name == "apply").flatMap { apply =>
          val i = before.lastIndexOf(apply)
          for (m <- method_text(apply.source); prev <- before.take(i).lastOption)
          yield (m, prev, s"the last `apply` before it, line ${line_of(snapshot, apply)}", List(m))
        }
      case _ => None
    }
  }

  private def line_of(snapshot: Document.Snapshot, cmd: Command): Int =
    snapshot.node.command_start(cmd).fold(0)(o => Line.Document(snapshot.node.source).position(o).line1)

  private def query(cmd: Command, at: Command, operation: String, args: List[String], key: Key): Option[PIDE_MCP_Query.Result] = {
    val snapshot = session.node_snapshot(cmd.node_name)
    snapshot.node.command_start(at).flatMap { offset =>
      val res = new PIDE_MCP_Query(session, PIDE_MCP_Editor.Context(cmd.node_name, offset), operation, new Progress)
        .run(args, Some(Time.seconds(method_timeout + 20)), _ => !valid(key, cmd))
      Option.when(res.outcome == PIDE_MCP_Query.Outcome.finished && valid(key, cmd))(res)
    }
  }

  private def probe(key: Key, cmd: Command, trigger: String): Unit = {
    val snapshot = session.node_snapshot(cmd.node_name)
    val failed = trigger == "failed"
    val before = snapshot.node.commands.iterator.takeWhile(_ != cmd).filterNot(_.is_ignored).toList
    val proof = before.reverse.takeWhile(c => !kind(snapshot, c).exists(k => Keyword.theory_goal(k) || Keyword.proof_goal(k)))
    val follow_on = failed && proof.exists(c => PIDE_MCP_Command.status(snapshot, c).is_failed)
    if (follow_on) skipped.change(s => (s + key).takeRight(results_keep))
    else if (failed && before.nonEmpty && needs_suggestions(key)) {
      if (suggest_ready)
        add_suggestions(key,
          query(cmd, before.last, Tool_Suggest_Facts.operation, List(suggest_max.toString, "true"), key).toList
            .flatMap(res => split_lines(res.text).filter(_.startsWith("  ")).map(_.trim)), session.id)
    }
    else if (before.nonEmpty && start(key)) {
      val result = Exn.result {
        val steps = plan(snapshot, cmd, before)
        // a failed terminal step: the trace also replays its methods without instrumentation (the scenario)
        val terminal = failed && terminal_commands(cmd.span.name)
        val trace = steps.flatMap { case (m, at, _, ms) =>
          query(cmd, at, operation,
            List(m, method_timeout.toString, trace_max.toString) :::
              (if (terminal && ms.nonEmpty) suggest_max.toString :: ms else Nil), key) }
        // the trace has the suggestions for the goal of the scenario
        val left = trace.exists(res => split_lines(res.text).contains("suggested:"))
        // the failing goal: the state after the command before the failed one
        val suggestions =
          if (!failed || !suggest_ready || left) Nil
          else query(cmd, before.last, Tool_Suggest_Facts.operation, List(suggest_max.toString, "true"), key).toList
            .flatMap(res => split_lines(res.text).filter(_.startsWith("  ")).map(_.trim))
        val note = steps.map(_._3).filter(_.nonEmpty)
        Option.when(valid(key, cmd) && (trace.nonEmpty || suggestions.nonEmpty)) {
          val trigger1 = trigger + (if (failed && cmd.span.name != "apply") s" `${cmd.span.name}`" else "") +
            note.fold("")(n => s"; replayed $n")
          parse(trigger1, trace.fold("")(_.text), session.id, suggestions,
            methods = steps.fold(0)(_._4.length), done = cmd.span.name == "done")
            .copy(elapsed = trace.fold(Time.zero)(_.elapsed), method = steps.map(_._1))
        }
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
      suggest_ready = ready && (Exn.result(Tool_Suggest_Facts.ensure_helper(session, new Progress)) match {
        case Exn.Res(_) => true
        case Exn.Exn(_) => false
      })
      while (active && ready) {
        val now = Time.now()
        for ((key, (cmd, since)) <- running.value if now - since >= after) {
          running.change(_ - key)
          enqueue(key, cmd, s"real run over ${after.seconds.toInt} s")
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
