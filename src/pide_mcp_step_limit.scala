/*  Title:      PIDE_MCP/pide_mcp_step_limit.scala

A time limit for proof steps (pide_mcp_step_time_limit, default 5 s; 0: none): a step (`apply`, `by`, `proof`,
`qed`, `done`, `.`, `..`) whose real run reaches the limit is interrupted, like `cancel`. The automatic
inspection still runs its diagnostic on the step's goal. `retry_without_limit` lets the command run again
without the limit and without the automatic inspection, once: the permission belongs to that evaluation of the
command, an edit of it (a new command) or a re-evaluation has the limit again. Only the MCP's PIDE sessions are
affected, not batch builds or the user's editor.
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

  private[mcp] def interrupt(key: Key, limit: Double): Unit =
    interrupted.change(m => (if (m.size >= keep) m.drop(m.size - keep + 1) else m) + (key -> limit))

  private[mcp] def allow(key: Key): Unit =
    unlimited.change(s => (if (s.size >= keep) s.drop(s.size - keep + 1) else s) + key)

  // retries asked for: the old command, its node, offset and source; matched by the new command of the same text
  sealed case class Retry(old: Document_ID.Command, node_name: Document.Node.Name, offset: Text.Offset, source: String,
    asked: Time)
  private[mcp] val retries = Synchronized(List.empty[Retry])

  def retry_note(line: Int): String =
    s"retry without the limit: `retry_without_limit` with this file and start_line $line " +
      "(no automatic diagnostic; `cancel` stops it)"
}

class PIDE_MCP_Step_Limit(session: PIDE_MCP_Session) {
  import PIDE_MCP_Step_Limit._

  private val limit: Double = session.options.real("pide_mcp_step_time_limit")

  @volatile private var active = true

  // running steps of the current evaluations
  private val running = Synchronized(Map.empty[Key, (Command, Document.Node.Name)])

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
            val snapshot = session.node_snapshot(node_name)
            val status = PIDE_MCP_Command.status(snapshot, cmd)
            // the real run, also for a forked proof (`by` of a lemma), which PIDE does not time
            lazy val running_ms =
              status.timings.sum(Date.now()).ms max PIDE_MCP_Command.Running_Clock.elapsed_ms(snapshot, cmd)
            if (PIDE_MCP_Step_Limit.key(snapshot, cmd).contains(k) && status.is_running &&
                running_ms >= (limit * 1000).toLong) {
              interrupt(k, limit)
              Tool_Cancel.cancel_command(session, snapshot, cmd, running_ms)
            }
          }
          Time.seconds(0.2).sleep()
        }
      }
      ()
    })

  def stop(): Unit = {
    active = false
    session.session.commands_changed -= consumer
  }
}
