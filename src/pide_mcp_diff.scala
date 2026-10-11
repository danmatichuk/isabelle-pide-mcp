/*  Title:      PIDE_MCP/pide_mcp_diff.scala

Line-based diff of two texts into several edit spans.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Diff {
  // upper bound on the size of the LCS table; larger inputs fall back to a single span
  val max_cells: Long = 4000000L

  // a replacement of old_text at offset (in the old text) by new_text
  sealed case class Hunk(
    offset: Text.Offset,
    old_text: String,
    new_text: String,
    old_lines: Int,
    new_lines: Int,
    new_line: Int, // first line of new_text (or where lines were removed) in the new text, 1-based
    old_line: Int  // first line of old_text (or where lines were inserted) in the old text, 1-based
  ) {
    def changed_lines: Int = old_lines max new_lines
  }

  // lines including their terminators
  private def lines(text: String): Vector[String] = {
    val builder = Vector.newBuilder[String]
    var i = 0
    while (i < text.length) {
      val j = text.indexOf('\n', i)
      val end = if (j < 0) text.length else j + 1
      builder += text.substring(i, end)
      i = end
    }
    builder.result()
  }

  def hunks(old_text: String, new_text: String): List[Hunk] =
    if (old_text == new_text) Nil
    else {
      val a = lines(old_text)
      val b = lines(new_text)
      val prefix = a.iterator.zip(b.iterator).takeWhile(_ == _).length
      val suffix =
        a.reverseIterator.take(a.length - prefix).zip(b.reverseIterator.take(b.length - prefix))
          .takeWhile(_ == _).length
      val am = a.slice(prefix, a.length - suffix)
      val bm = b.slice(prefix, b.length - suffix)
      val offsets = a.scanLeft(0)(_ + _.length)

      def hunk(old_from: Int, old_to: Int, new_from: Int, new_to: Int): Hunk = {
        val start = offsets(prefix + old_from)
        val end = offsets(prefix + old_to)
        Hunk(start, old_text.substring(start, end), bm.slice(new_from, new_to).mkString,
          old_to - old_from, new_to - new_from, prefix + new_from + 1, prefix + old_from + 1)
      }

      if (am.isEmpty || bm.isEmpty || am.length.toLong * bm.length > max_cells)
        List(hunk(0, am.length, 0, bm.length))
      else {
        val n = am.length
        val m = bm.length
        // lcs(i)(j): length of the longest common subsequence of am[i..] and bm[j..]
        val lcs = Array.ofDim[Int](n + 1, m + 1)
        for (i <- n - 1 to 0 by -1; j <- m - 1 to 0 by -1)
          lcs(i)(j) =
            if (am(i) == bm(j)) lcs(i + 1)(j + 1) + 1 else lcs(i + 1)(j) max lcs(i)(j + 1)
        val result = List.newBuilder[Hunk]
        var open: Option[(Int, Int)] = None
        def close(i: Int, j: Int): Unit = {
          for ((oi, oj) <- open) result += hunk(oi, i, oj, j)
          open = None
        }
        var i = 0
        var j = 0
        while (i < n || j < m) {
          if (i < n && j < m && am(i) == bm(j)) { close(i, j); i += 1; j += 1 }
          else {
            if (open.isEmpty) open = Some((i, j))
            if (j >= m || (i < n && lcs(i + 1)(j) >= lcs(i)(j + 1))) i += 1 else j += 1
          }
        }
        close(n, m)
        result.result()
      }
    }

  // unified diff (like `diff -u`) of two texts
  def unified(label: String, old_text: String, new_text: String, context: Int = 3): String = {
    val old = lines(old_text)
    val now = lines(new_text)
    def show(prefix: String, line: String): String =
      prefix + (if (line.endsWith("\n")) line else line + "\n\\ No newline at end of file\n")
    // hunks whose contexts overlap share one section
    val groups = hunks(old_text, new_text).foldLeft(List.empty[List[Hunk]]) {
      case (current :: done, h) if h.old_line - (current.head.old_line + current.head.old_lines) <= 2 * context =>
        (h :: current) :: done
      case (groups, h) => List(h) :: groups
    }.map(_.reverse).reverse
    val sections = groups.map { group =>
      val start = (group.head.old_line - 1 - context) max 0
      val end = (group.last.old_line - 1 + group.last.old_lines + context) min old.length
      val body = new StringBuilder
      var pos = start
      for (h <- group) {
        for (line <- old.slice(pos, h.old_line - 1)) body ++= show(" ", line)
        for (line <- old.slice(h.old_line - 1, h.old_line - 1 + h.old_lines)) body ++= show("-", line)
        for (line <- now.slice(h.new_line - 1, h.new_line - 1 + h.new_lines)) body ++= show("+", line)
        pos = h.old_line - 1 + h.old_lines
      }
      for (line <- old.slice(pos, end)) body ++= show(" ", line)
      val old_count = end - start
      val new_count = old_count - group.map(_.old_lines).sum + group.map(_.new_lines).sum
      val new_start = group.head.new_line - 1 - (group.head.old_line - 1 - start)
      def range(first: Int, count: Int) = s"${if (count == 0) first else first + 1},$count"
      s"@@ -${range(start, old_count)} +${range(new_start, new_count)} @@\n" + body.result()
    }
    if (sections.isEmpty) "" else s"--- a/$label\n+++ b/$label\n" + sections.mkString
  }

  // edits that can be applied sequentially to the old text (back to front)
  def edits(hunks: List[Hunk]): List[Text.Edit] =
    hunks.reverse.flatMap(h => Text.Edit.replace(h.offset, h.old_text, h.new_text))

  def edits(old_text: String, new_text: String): List[Text.Edit] = edits(hunks(old_text, new_text))
}
