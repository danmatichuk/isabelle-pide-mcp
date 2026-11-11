/*  Title:      PIDE_MCP/pide_mcp_util.scala
    Author:     Kevin Kappelmann

General utilities.
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_Util {
  // "3-5, 9" for the lines 3, 4, 5, 9
  def line_ranges(lines: List[Int]): String =
    lines.distinct.sorted.foldLeft(List.empty[(Int, Int)]) {
      case ((a, b) :: rest, l) if l == b + 1 => (a, l) :: rest
      case (acc, l) => (l, l) :: acc
    }.reverse.map { case (a, b) => if (a == b) a.toString else s"$a-$b" }.mkString(", ")

  def capture_failures[A](args: IterableOnce[A], run: A => Unit): Iterator[(A, Throwable)] =
    args.iterator.flatMap(arg =>
      Exn.capture(run(arg)) match {
        case Exn.Res(_) => None
        case Exn.Exn(exn) => Some((arg, exn))
      })

  // like Exn.release_first, but reports every error instead of only the first one
  def failure(exn: Throwable, more: List[Throwable]): Throwable =
    (exn :: more).filterNot(Exn.is_interrupt) match {
      case Nil => exn
      case List(exn1) => exn1
      case failures => ERROR(cat_lines(failures.map(Exn.message)))
    }

  def check_failures(exns: List[Throwable]): Unit =
    for (exn <- exns.headOption) throw failure(exn, exns.tail)

  def text_range(doc: Line.Document, start_line: Int, end_line: Int): Option[Text.Range] =
    if (start_line - 1 > end_line) None
    else doc.text_range(Line.Range(Line.Position(start_line - 1), Line.Position(end_line)))

  def text_perspective(
    doc: Line.Document,
    line_ranges: List[(Int, Int)],
    context: Int
  ): Text.Perspective =
    Text.Perspective(line_ranges.map { case (start_line, end_line) =>
      text_range(doc, (start_line - context) max 1, (end_line + context) min doc.lines.length)
        .getOrElse(error(
          s"Bad line range $start_line..$end_line (file has ${doc.lines.length} lines)")) })

  // only whitespace and comments: such spans are no commands and have no content to report
  def is_ignored_source(text: String): Boolean =
    Token.explode(Keyword.Keywords.empty, text).forall(_.is_ignored)

  def numbered_line(line: Int, text: String): String =
    s"${line}: ${text}"

  def line_range(start_line: Int, end_line: Int): String =
    if (start_line == end_line) start_line.toString else s"$start_line-$end_line"

  def numbered_lines(lines: IterableOnce[String], start_line: Int): String =
    lines.iterator.zipWithIndex.map { case (l, i) =>
      numbered_line(start_line + i, l) }.mkString("\n")

  def numbered_lines(text: String, start_line: Int): String =
    numbered_lines(Line.logical_lines(text), start_line)

  def numbered_lines_range(lines: List[String], start_line: Int, end_line: Int): String =
    numbered_lines(lines.slice(start_line - 1, end_line min lines.length), start_line)

  def numbered_lines_range(text: String, start_line: Int, end_line: Int): String =
    numbered_lines_range(Line.logical_lines(text), start_line, end_line)

  def print_process_result(process_result: Process_Result): String =
    process_result.print_rc + if_proper(process_result.err, s": ${process_result.err}")

  def path(s: String): Path = Path.explode(File.standard_path(s))

  def uses_isabelle_encoding(path: Path): Boolean =
    File.is_thy(path.file_name) || File.is_ML(path.file_name)

  def decode_file_text(path: Path, text: String): String =
    if (uses_isabelle_encoding(path)) Symbol.decode(text) else text

  def encode_file_text(path: Path, text: String): String =
    if (uses_isabelle_encoding(path)) Symbol.encode(text) else text

  def display_name(entry: Option[Name_Space.Entry], range: Text.Range, source: String): String =
    entry.map(_.name).filter(_.nonEmpty).getOrElse(range.substring(source))

  // a cleaned node remains in version.nodes but with empty source
  def is_loaded_dynamic(node: Document.Node): Boolean =
    node.get_blob match {
      case Some(blob) => blob.source.nonEmpty
      case None => !node.is_empty
    }

  def intersect_range(full: Text.Range, range: Option[Text.Range]): Text.Range =
    range.fold(full)(r => full.try_restrict(r).getOrElse(Text.Range.zero))

  def restrict_text_range(text: String, range: Option[Text.Range]): Text.Range =
    intersect_range(Text.Range.length(text), range)

  // seconds as written in options: 5.0 -> "5", 2.5 -> "2.5"
  def seconds_text(seconds: Double): String =
    if (seconds == seconds.round.toDouble) seconds.round.toString else seconds.toString

  def elem_body_plain_text(elem: XML.Elem): String =
    Pretty.string_of(Qualify_Names.body(elem.body), pure = true)

  /* Constants that hide behind a word in the MCP's output are printed with their full name (pide_mcp_qualify_names).
     Isabelle itself already qualifies a name that is ambiguous in the context (`Foo.map`, `list.map`), but a notation
     whose only token is a word (`notation my_map ("map")`, e.g. from an included bundle) prints exactly like the
     library's `map`: such a word is replaced by the constant's full name (`Foo.my_map`). Notation with symbols or
     several tokens (`@`, `∀x.`, `if … then … else`, `THE x.`) prints as before. Also qualified: a constant printed by
     its plain name that shadows a different constant of the same base name from the base sessions (a project's
     `map` while List.map exists; within its own theory Isabelle prints it as plain `map`). Modes: notation (default), outside_base
     (also every constant of a theory outside the base sessions), all (every constant), none. Only the MCP's copy of
     the messages is rewritten (its markup), not the prover's printing: the user's editor is not affected. */
  object Qualify_Names {
    @volatile private var mode = "notation"  // notation | outside_base | all | none
    private val base_theories = Synchronized(Set.empty[String])  // base names of the theories of base sessions
    // base name -> full names of the constants of the base sessions (loaded in the background after a session start)
    private val base_consts = Synchronized(Map.empty[String, Set[String]])

    def add_base_consts(names: List[String]): Unit =
      base_consts.change(map => names.foldLeft(map) { (m, name) =>
        val base = Long_Name.base_name(name)
        m + (base -> (m.getOrElse(base, Set.empty) + name))
      })

    // a constant outside the standard library (AFP, project), printed by its plain base name, while another constant
    // of the session image has that base name (`map` for chacha20.map, while List.list.map exists); collisions among
    // library constants (two `Suc`) are not shown
    private def shadows(name: String, text: String): Boolean =
      text == Long_Name.base_name(name) &&
        !library_theories.value.contains(Long_Name.explode(name).headOption.getOrElse("")) &&
        base_consts.value.get(text).exists(_.exists(_ != name))

    // base names of theories of the Isabelle distribution (source under $ISABELLE_HOME/src): the standard library
    private val library_theories = Synchronized(Set.empty[String])

    def init(options: Options, base: Iterable[String], source: String => Option[Path] = _ => None): Unit = {
      mode = options.string("pide_mcp_qualify_names")
      base_theories.change(_ ++ base.map(Long_Name.base_name))
      val src = Path.explode("$ISABELLE_HOME/src").canonical.implode + "/"
      val library = base.filter(theory => source(theory).exists(_.canonical.implode.startsWith(src)))
      library_theories.change(_ ++ library.map(Long_Name.base_name))
    }

    /* the name to print: the full name, except when it repeats its theory qualifier, like the map function of a
       `lift_bnf` (Seq.seq.Seq.map_seq): then the theory and the rest after the repetition (Seq.map_seq), which refers
       to the same constant */
    def display(name: String): String =
      Long_Name.explode(name) match {
        case theory :: rest if rest.nonEmpty && rest.init.contains(theory) =>
          Long_Name.implode(theory :: rest.drop(rest.init.lastIndexOf(theory) + 1))
        case _ => name
      }

    // a constant printed by its name (not notation)
    private def qualify_name(name: String): Boolean =
      mode match {
        case "all" => true
        case "outside_base" => !base_theories.value.contains(Long_Name.explode(name).headOption.getOrElse(""))
        case _ => false
      }

    private def is_word(s: String): Boolean =
      s.nonEmpty && Symbol.is_ascii_letter(s.head) && s.forall(c => Symbol.is_ascii_letdig(c) || c == '.')

    // the syntax tokens of each constant in a message: literal words, and whether it has symbol delimiters
    private def notations(body: XML.Body): Map[String, (Set[String], Boolean)] = {
      var result = Map.empty[String, (Set[String], Boolean)]
      def tokens(tree: XML.Tree): List[(Boolean, String)] = tree match {
        case XML.Elem(Markup(Markup.LITERAL, _), b) => List((true, XML.content(b)))
        case XML.Elem(Markup(Markup.DELIMITER, _), b) => List((false, XML.content(b)))
        case XML.Elem(_, b) => b.flatMap(tokens)
        case _ => Nil
      }
      def traverse(tree: XML.Tree): Unit = tree match {
        case XML.Elem(Markup.Entity(Markup.CONSTANT, name), b) =>
          val (words, delims) = b.flatMap(tokens).partition(_._1)
          val (w0, d0) = result.getOrElse(name, (Set.empty[String], false))
          result += (name -> (w0 ++ words.map(_._2), d0 || delims.nonEmpty))
          b.foreach(traverse)
        case XML.Elem(_, b) => b.foreach(traverse)
        case _ =>
      }
      body.foreach(traverse)
      result
    }

    def body(body: XML.Body): XML.Body =
      if (mode == "none") body
      else {
        val syntax = notations(body)
        def rewrite(body: XML.Body): XML.Body = body.map {
          case XML.Elem(markup @ Markup.Entity(Markup.CONSTANT, name), b) if name.nonEmpty =>
            val text = XML.content(b)
            val (words, delims) = syntax.getOrElse(name, (Set.empty[String], false))
            val notation_word = words == Set(text) && !delims && is_word(text) && text != name
            val plain_name = words.isEmpty && !delims && text != name && name.endsWith("." + text) &&
              (qualify_name(name) || shadows(name, text))
            if (notation_word || plain_name) XML.Elem(markup, List(XML.Text(display(name))))
            else XML.Elem(markup, rewrite(b))
          case XML.Elem(markup, b) => XML.Elem(markup, rewrite(b))
          case text => text
        }
        rewrite(body)
      }
  }
}
