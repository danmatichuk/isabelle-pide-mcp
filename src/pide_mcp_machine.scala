/*  Title:      PIDE_MCP/pide_mcp_machine.scala

Memory of the machine and of all Isabelle processes on it (not only of this server), so that an agent
can see what another session costs before it starts one.
*/

package isabelle.pide.mcp

import isabelle._

import scala.jdk.OptionConverters._

object PIDE_MCP_Machine {
  // starting a session while less memory is available is refused (unless asked otherwise)
  val default_min_available_mb: Int = 4096

  private val page_mb_divisor = 1024L * 1024L / 4096L  // statm counts 4 KiB pages

  // /proc files report size 0, which Isabelle's File.read takes literally: read until EOF instead
  private def read(path: String): Option[String] =
    Exn.result(new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(path)), UTF8.charset)) match {
      case Exn.Res(text) => Some(text)
      case Exn.Exn(_) => None
    }

  private def meminfo_mb(key: String): Option[Long] =
    read("/proc/meminfo").flatMap(_.linesIterator.collectFirst {
      case line if line.startsWith(key + ":") =>
        line.drop(key.length + 1).trim.takeWhile(_.isDigit).toLong / 1024
    })

  private def rss_mb(pid: Long): Long =
    read(s"/proc/$pid/statm").flatMap(_.trim.split("\\s+").lift(1)).flatMap(_.toLongOption)
      .map(_ / page_mb_divisor).getOrElse(0L)

  private def command_line(handle: ProcessHandle): String =
    handle.info.commandLine.toScala.orElse(handle.info.command.toScala).getOrElse("")

  private def is_poly(handle: ProcessHandle): Boolean =
    handle.info.command.toScala.exists(_.endsWith("/poly"))

  private def is_isabelle_java(handle: ProcessHandle): Boolean =
    handle.info.command.toScala.exists(_.endsWith("/java")) && command_line(handle).contains("isabelle")

  private def all_processes(): List[ProcessHandle] = {
    val me = System.getProperty("user.name")
    val it = ProcessHandle.allProcesses().iterator()
    val result = List.newBuilder[ProcessHandle]
    while (it.hasNext) {
      val handle = it.next()
      if (handle.info.user.toScala.forall(_ == me)) result += handle
    }
    result.result()
  }

  def available_mb(): Option[Long] = meminfo_mb("MemAvailable")

  def json(): JSON.Object.T = {
    val processes = all_processes()
    val java = processes.filter(is_isabelle_java)
    val poly = processes.filter(is_poly)
    val own = ProcessHandle.current() :: {
      val it = ProcessHandle.current().descendants().iterator()
      val b = List.newBuilder[ProcessHandle]
      while (it.hasNext) b += it.next()
      b.result()
    }
    JSON_Object("machine" ->
      JSON_Object.flatten(List(
        meminfo_mb("MemTotal").map("memory_total_mb" -> _),
        available_mb().map("memory_available_mb" -> _),
        Some("isabelle_processes_all_servers" -> JSON_Object(
          "java" -> java.length, "poly" -> poly.length,
          "total_rss_mb" -> (java ::: poly).map(h => rss_mb(h.pid)).sum)),
        Some("this_server" -> JSON_Object(
          "processes" -> own.length, "total_rss_mb" -> own.map(h => rss_mb(h.pid)).sum)))))
  }

  def summary(): String = {
    val all = json().get("machine").collect { case JSON.Object(o) => o }.getOrElse(Map.empty)
    val procs = all.get("isabelle_processes_all_servers").collect { case JSON.Object(o) => o }.getOrElse(Map.empty)
    s"${procs.getOrElse("java", 0)} Isabelle java and ${procs.getOrElse("poly", 0)} poly processes use " +
      s"${procs.getOrElse("total_rss_mb", 0)} MB in total"
  }

  def require_memory(ignore: Boolean, min_available_mb: Int): Unit =
    for (available <- available_mb() if !ignore && available < min_available_mb)
      error(s"Refusing to start a session: only $available MB of memory are available (limit $min_available_mb MB), " +
        s"and ${summary()} on this machine. A further session may exhaust the memory and crash the machine. " +
        "Stop sessions or servers you do not need, or pass ignore_memory_check: true if you know better.")
}
