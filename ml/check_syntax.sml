(*  Title:      PIDE_MCP/ml/check_syntax.sml

Syntax check of ML text for the PIDE MCP server: the file named in the environment variable PIDE_MCP_ML_FILE is compiled with the
stand-alone Poly/ML compiler, and the compiled code is NEVER run (no declaration, no side effect). Output: one line
"SYNTAX <line> <message>" per syntax error of the first declaration that fails to parse. Name and type errors are not reported:
earlier declarations of the text are not entered, so they would be false alarms.
*)

val file = (case OS.Process.getEnv "PIDE_MCP_ML_FILE" of SOME f => f | NONE => raise Fail "PIDE_MCP_ML_FILE");
val ch = TextIO.openIn file;
val line = ref 1;
fun get () =
  (case TextIO.input1 ch of
    NONE => NONE
  | SOME c => (if c = #"\n" then line := ! line + 1 else (); SOME c));

fun pretty_string p =
  let val buf = ref ([]: string list)
  in PolyML.prettyPrint (fn s => buf := s :: ! buf, 200) p; String.concat (rev (! buf)) end;

fun is_syntax_error msg =
  List.exists (fn p => String.isSubstring p msg)
    ["expected but", "was found", "Unterminated", "unterminated", "Syntax error", "syntax error"];

(*fixity declarations (infix, infixr, nonfix lines of the text) are run first: they only enter operator fixities, which later
  declarations of the text need to parse; nothing else is ever run*)
val _ =
  (case OS.Process.getEnv "PIDE_MCP_ML_INFIX" of
    NONE => ()
  | SOME f =>
      let
        val ich = TextIO.openIn f;
        fun iget () = TextIO.input1 ich;
        fun enter_fix (_, code) () =
          (case code of
            NONE => ()
          | SOME c =>
              let val {fixes, ...} = c ()
              in List.app (#enterFix PolyML.globalNameSpace) fixes end);
        val iparams =
         [PolyML.Compiler.CPOutStream (fn _ => ()),
          PolyML.Compiler.CPErrorMessageProc (fn _ => ()),
          PolyML.Compiler.CPCompilerResultFun enter_fix,
          PolyML.Compiler.CPPrintInAlphabeticalOrder false];
      in
        (while not (TextIO.endOfStream ich) do PolyML.compiler (iget, iparams) ()) handle _ => ()
      end);

val syntax_seen = ref false;
fun report (l, msg) =
  print ("SYNTAX " ^ Int.toString l ^ " " ^ String.map (fn #"\n" => #" " | c => c) msg ^ "\n");

fun message {message = msg, hard, location = (loc: PolyML.location), context = _} =
  let val text = pretty_string msg
  in if hard andalso is_syntax_error text then (syntax_seen := true; report (FixedInt.toInt (#startLine loc), text)) else () end;

exception STATIC of unit;
fun result_fun (_, code) () =
  (case code of NONE => raise STATIC () | SOME _ => ());   (*the compiled code is not run*)

val parameters =
 [PolyML.Compiler.CPOutStream (fn _ => ()),
  PolyML.Compiler.CPErrorMessageProc message,
  PolyML.Compiler.CPLineNo (fn () => ! line),
  PolyML.Compiler.CPCompilerResultFun result_fun,
  PolyML.Compiler.CPPrintInAlphabeticalOrder false,
  PolyML.Compiler.CPBindingSeq (let val r = ref 0 in fn () => (r := ! r + 1; ! r) end)];

val _ =
  (while not (TextIO.endOfStream ch) andalso not (! syntax_seen) do
    (PolyML.compiler (get, parameters) () handle STATIC () => ()))
  handle _ => ();

val _ = print "END\n";
