/*  Title:      PIDE_MCP/pide_mcp_status.scala

Progress lines for the human watching a long call: the editor shows only "Isabelle [get_state]" and a spinner while a call waits.
A line every 10 seconds is sent as MCP progress notification (shown by terminal clients) and appended to <server cwd>/.pide_mcp/status.log
(the file to watch in editors that show no progress).
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Status {
  val interval: Time = Time.seconds(10)
  private val max_log_bytes = 200000L
  private val log_lock = new Object

  def log_path: Path = Path.explode(".pide_mcp/status.log").absolute

  // one timestamped line; never fails (a status file must not break a tool call)
  def log(line: String): Unit = log_lock.synchronized {
    Exn.result {
      val file = log_path
      val dir = file.dir
      if (!dir.is_dir) Isabelle_System.make_directory(dir)
      val ignore = dir + Path.basic(".gitignore")
      if (!ignore.is_file) File.write(ignore, "*\n")
      if (file.is_file && file.file.length > max_log_bytes) {
        val keep = split_lines(File.read(file)).takeRight(500)
        File.write(file, cat_lines(keep) + "\n")
      }
      File.append(file, Date.Format("HH:mm:ss")(Date.now()) + " " + line + "\n")
    }
    ()
  }

  // calls tick() in a polling loop: reports at most once per interval
  class Ticker(progress: Progress, what: String) {
    private val start = Time.now()
    private var last = start
    def tick(status: => String): Unit = {
      val now = Time.now()
      if (now - last >= interval) {
        last = now
        val text = Exn.result(status) match {
          case Exn.Res(s) => s
          case Exn.Exn(_) => "waiting"
        }
        val line = s"$what: $text, ${(now - start).seconds.round} s"
        Exn.result(progress.echo(line))
        log(line)
      }
    }
  }

  // runs `body` while a daemon thread reports `status` every interval (for blocking calls like building a session)
  def with_reports[A](progress: Progress, what: String)(body: => A): A = {
    val ticker = new Ticker(progress, what)
    @volatile var running = true
    val thread = new Thread(() => {
      while (running) { Time.seconds(1).sleep(); ticker.tick("still working") }
    })
    thread.setDaemon(true)
    thread.start()
    try { body } finally { running = false }
  }
}
