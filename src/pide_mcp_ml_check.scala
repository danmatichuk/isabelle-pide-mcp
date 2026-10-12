/*  Title:      PIDE_MCP/pide_mcp_ml_check.scala

Syntax check of ML text without a session and without running it: antiquotations and formal comments are replaced (they depend on the
theory context), the rest is compiled by the stand-alone Poly/ML compiler (ml/check_syntax.sml) whose compiled code is never run.
Finds the errors that otherwise cost a whole reload of a theory and its dependents to find (e.g. a trailing comma in a record type).
*/

package isabelle.pide.mcp

import isabelle._

object PIDE_MCP_ML_Check {
  sealed case class Error(line: Int, message: String)
  // `unjudged`: lines that could hide an error the check cannot tell from an antiquotation's expansion
  sealed case class Result(errors: List[Error], unjudged: Int)

  // the text with antiquotations and formal comments replaced by `""` or blanks (newlines kept: line numbers stay the same).
  // Conditional compilation (`\<^if_unix>\<open>code\<close>`): the code of the platforms of this system (unix, linux) is checked as code, the
  // other platforms' code is dropped, as the Isabelle system itself would do here.
  // a line with control characters (e.g. DEL as Isabelle markup) is not judged: lexer and compiler disagree there
  private def has_control(line: Option[String]): Boolean =
    line.exists(_.exists(c => c == '\u007f' || (c < ' ' && c != '\t' && c != '\r')))

  def prepare(text: String): (String, List[Error], List[(Int, Int)]) = {
    val out = new StringBuilder
    var errors = List.empty[Error]
    var line = 1
    val src_lines = text.replace("\r\n", "\n").split('\n')
    var antiq_depth = 0
    var windows = List.empty[(Int, Int)]  // (line of a replaced antiquotation, line of the next token after it)
    var open_windows = List.empty[Int]
    def newlines(s: String): String = s.filter(_ == '\n')
    def cartouche_content(src: String): String =
      src.stripPrefix("\\<open>").stripPrefix("\u2039").stripSuffix("\\<close>").stripSuffix("\u203a")
    for (tok <- ML_Lex.tokenize(text.replace("\r\n", "\n"))) {  // CRLF files are accepted by Isabelle
      val src = tok.source
      if (open_windows.nonEmpty && !tok.is_space && tok.kind != ML_Lex.Kind.ANTIQ_STOP && antiq_depth == 0 && !tok.is_comment) {
        windows :::= open_windows.map(start => (start, line)); open_windows = Nil
      }
      tok.kind match {
        case ML_Lex.Kind.ANTIQ_START =>
          antiq_depth += 1; if (antiq_depth == 1) { out ++= "\"\""; open_windows ::= line }; out ++= newlines(src)
        case ML_Lex.Kind.ANTIQ_STOP => antiq_depth = (antiq_depth - 1) max 0; out ++= newlines(src)
        case k if antiq_depth > 0 => out ++= newlines(src)
        // conditional compilation: one token `\<^if_unix>\<open>code\<close>`; the code of this system's platforms is checked, the rest dropped
        case ML_Lex.Kind.CONTROL if src.startsWith("\\<^if_") =>
          val keep = src.startsWith("\\<^if_unix") || src.startsWith("\\<^if_linux")
          val content = cartouche_content(src.drop(src.indexOf('>') + 1))
          if (keep) { val (inner, inner_errors, inner_windows) = prepare(content); out ++= inner; errors :::= inner_errors.reverse
            windows :::= inner_windows.map { case (a, b) => (a + line - 1, if (b == Int.MaxValue) b else b + line - 1) } }
          else out ++= newlines(src)
        case ML_Lex.Kind.ANTIQ | ML_Lex.Kind.CONTROL => out ++= "\"\"" ++= newlines(src); open_windows ::= line
        case ML_Lex.Kind.ANTIQ_CARTOUCHE | ML_Lex.Kind.FORMAL_COMMENT => out ++= newlines(src).padTo(1, ' ')
        case ML_Lex.Kind.ERROR =>
          if (!has_control(src_lines.lift(line - 1)))
            errors ::= Error(line, "bad input: " + src.take(40))
          out ++= newlines(src)
        case ML_Lex.Kind.STRING | ML_Lex.Kind.CHAR =>
          out ++= src.replace("\\<", "\\092<")  // Isabelle symbols inside strings
        case _ => out ++= src
      }
      line += src.count(_ == '\n')
    }
    windows :::= open_windows.map(start => (start, Int.MaxValue))
    (out.toString, errors.reverse, windows)
  }

  private def poly_executable(): Option[Path] = {
    val home = Isabelle_System.getenv("POLYML_HOME")
    if (home.isEmpty) None
    else {
      val dirs = Option(new java.io.File(home).listFiles).toList.flatten.filter(_.isDirectory)
      dirs.sortBy(d => if (d.getName.startsWith("x86_64_32") || d.getName.contains("linux") || d.getName.contains("darwin")) 0 else 1)
        .map(d => new java.io.File(d, "poly")).find(f => f.canExecute && Exn.result(
          Isabelle_System.bash(Bash.string(f.getPath) + " --version").ok) == Exn.Res(true))
        .map(f => Path.explode(f.getPath))
    }
  }

  // Returns the syntax errors of `text` (empty: none found), or an error message if the check could not run.
  def check(text: String): Either[String, Result] =
    poly_executable() match {
      case None => Left("no Poly/ML executable found (POLYML_HOME)")
      case Some(poly) =>
        val (prepared, lex_errors, windows) = prepare(text)
        val text_lines = text.replace("\r\n", "\n").split('\n')
        if (lex_errors.nonEmpty) Right(Result(lex_errors, 0))
        else Isabelle_System.with_tmp_file("pide_mcp_ml_check", "ml") { file =>
          File.write(file, prepared)
          // the fixity declarations of the text, run first (see check_syntax.sml)
          val infix_lines = """(?m)^[ \t]*(infix|infixr|nonfix)\b[^\n]*""".r.findAllIn(prepared).map(_.trim.stripSuffix(";") + ";").mkString("\n")
          val infix_file = Isabelle_System.tmp_file("pide_mcp_ml_infix", "ml")
          File.write(File.path(infix_file), infix_lines)
          // (PIDE_MCP_ML_CHECK_SCRIPT: a test hook for a script outside the installed component)
          val script = Option(System.getenv("PIDE_MCP_ML_CHECK_SCRIPT")).map(Path.explode(_))
            .getOrElse(Path.explode("$PIDE_MCP_HOME/ml/check_syntax.sml"))
          val result = Isabelle_System.bash(
            "PIDE_MCP_ML_FILE=" + Bash.string(File.platform_path(file)) + " PIDE_MCP_ML_INFIX=" + Bash.string(File.platform_path(File.path(infix_file))) + " " + Bash.string(File.platform_path(poly)) +
              " -q --use " + Bash.string(File.platform_path(script)) + " < /dev/null",
            watchdog = Bash.Watchdog(Time.seconds(60)))
          infix_file.delete()
          if (result.timeout) Left("the syntax check timed out")
          else if (!result.out_lines.contains("END"))  // the script did not run to its end
            Left("the syntax check could not run: " + (result.out_lines ::: result.err_lines).takeRight(3).mkString(" | "))
          else {
            val Syntax = """SYNTAX (\d+) (.*)""".r
            // An antiquotation stands for unknown ML (an expression or declarations), so an error at or right
            // after one cannot be told from a real one: only errors elsewhere are reported.
            val n = text_lines.length
            val unjudged = (1 to n).count(l => windows.exists { case (a, b) => a <= l && l <= b } || has_control(text_lines.lift(l - 1)))
            Right(Result(
              result.out_lines.collect { case Syntax(l, m) => Error(l.toInt, m.trim) }
                .filterNot(e => windows.exists { case (a, b) => a <= e.line && e.line <= b })
                .filterNot(e => has_control(text_lines.lift(e.line - 1))),
              unjudged))
          }
        }
    }

  // readable text for a result: the errors with the offending source line
  def describe(file_name: String, text: String, result: Result): String = {
    val lines = split_lines(text)
    if (result.errors.isEmpty)
      s"ML syntax check of $file_name: no syntax errors found" +
        (if (result.unjudged > 0) s" in the lines it could judge; ${unjudged_note(result.unjudged)}" else "") +
        " (names and types are not checked)"
    else
      s"ML SYNTAX ERROR in $file_name (nothing was run):\n" +
        cat_lines(result.errors.distinctBy(_.line).take(3).map(e => s"  line ${e.line}: ${e.message}" +
          lines.lift(e.line - 1).fold("")(l => "\n    " + l.trim.take(160)))) +
        (if (result.unjudged > 0) "\n  (" + unjudged_note(result.unjudged) + ")" else "")
  }

  def unjudged_note(n: Int): String =
    s"$n line(s) at antiquotations or with control characters were NOT checked: an error there is not reported"

}
