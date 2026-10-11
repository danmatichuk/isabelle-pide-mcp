(*  Title:      PIDE_MCP/ml/PIDE_MCP_Eval.thy

Helper theory, loaded on demand by the `eval_ml` tool of the PIDE MCP server.
It registers a PIDE query operation that evaluates ML code in the context at a position of a
theory (with all ML structures of the imported theories), without changing the theory.
*)

theory PIDE_MCP_Eval
  imports Pure
begin

ML \<open>
local

fun plain ss = XML.content_of (YXML.parse_body (implode ss));

in

val _ =
  Query_Operation.register {name = "pide_mcp_eval_ml", pri = Task_Queue.urgent_pri}
    (fn {state = st, args, writeln_result, ...} =>
      let
        val code = the_single args;
        val ctxt = Toplevel.context_of st;
        val out = Unsynchronized.ref ([]: string list);
        fun add s = Unsynchronized.change out (cons s);
        val flags =
          {environment = "", redirect = false, verbose = true, catch_all = false, debug = NONE,
            writeln = add, warning = fn s => add ("warning: " ^ s)};
        fun eval () = ML_Context.eval_source_in (SOME ctxt) flags (Input.source true code (Position.start, Position.none));
        (*output of explicit writeln/tracing/warning calls in the code is collected as well*)
        val result =
          Exn.capture
            (Unsynchronized.setmp Private_Output.writeln_fn (add o plain)
              (Unsynchronized.setmp Private_Output.tracing_fn (add o plain)
                (Unsynchronized.setmp Private_Output.warning_fn (fn ss => add ("warning: " ^ plain ss))
                  eval))) ();
        val _ = if null (! out) then () else writeln_result (cat_lines (map trim_line (rev (! out))));
      in Exn.release result end);

end;
\<close>

end
