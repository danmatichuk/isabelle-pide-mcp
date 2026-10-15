(*  Title:      PIDE_MCP/ml/PIDE_MCP_Apply_Inspect.thy

Helper theory of the PIDE MCP server, loaded in the background after a session start. It imports
Apply_Inspect from the apply_inspect repository (the git submodule deps/apply_inspect: the
`apply_inspect` command, a diagnostic variant of `apply`) and sets its time limits: the method runs
for at most apply_inspect_timeout; from apply_inspect_trace_start on, the goal state that it has
reached is recorded every apply_inspect_trace_interval; `apply_inspect` shows the first recorded
state and each `back` the next one. A theory that imports this one gets the same limits for its
own `apply_inspect`.

The query operation pide_mcp_interim_goal runs a method like `apply_inspect` with these limits on
the proof state at a position, without changing the theory: the first recorded goal state (for a
proof step interrupted at pide_mcp_step_time_limit).
*)

theory PIDE_MCP_Apply_Inspect
  imports "../deps/apply_inspect/Apply_Inspect"
begin

declare [[apply_inspect_timeout = 5000, apply_inspect_trace_start = 200, apply_inspect_trace_interval = 100]]

ML \<open>
structure PIDE_MCP_Apply_Inspect =
struct

(*the limits declared above, also for the query, which runs in the context of another theory*)
val limits =
 {timeout = Time.fromMilliseconds (Config.get_global \<^theory> Apply_Inspect.timeout),
  trace_start = Time.fromMilliseconds (Config.get_global \<^theory> Apply_Inspect.trace_start),
  trace_interval = Time.fromMilliseconds (Config.get_global \<^theory> Apply_Inspect.trace_interval)};

val params: Apply_Inspect.params =
  {every_use = false, on_use = fn _ => fn _ => (), on_loop = fn _ => fn _ => ()};

fun ms t = string_of_int (Time.toMilliseconds t);

fun goal_text st =
  Pretty.string_of (Goal_Display.pretty_goal (Proof.context_of st) (#goal (Proof.goal st)));

(*lines: "outcome: traced|finished|failed", "timeout_ms: N", "start_ms: N", "interval_ms: N",
  "time_ms: N" (traced), "error: ..." (failed), "goal:" and the first recorded goal state (traced)*)
fun interim_goal [method_arg] state =
      let
        val ctxt = Proof.context_of state;
        val text =
          Parse.read_embedded ctxt (Keyword.no_major_keywords (Thy_Header.get_keywords' ctxt))
            (Scan.many Token.not_eof) (Input.string method_arg)
          |> Method.read ctxt;
        val {succeeded, error, partial, ...} = Apply_Inspect.inspect_limited params limits text state;
        val limit_lines =
          ["timeout_ms: " ^ ms (#timeout limits), "start_ms: " ^ ms (#trace_start limits),
            "interval_ms: " ^ ms (#trace_interval limits)];
      in
        (case Seq.pull partial of
          SOME ({time, state = st, ...}, _) =>
            cat_lines (["outcome: traced"] @ limit_lines @ ["time_ms: " ^ ms time, "goal:", goal_text st])
        | NONE =>
            if succeeded then cat_lines ("outcome: finished" :: limit_lines)
            else cat_lines ("outcome: failed" :: limit_lines @ the_list (Option.map (prefix "error: ") error)))
      end
  | interim_goal _ _ = error "Bad arguments";

val _ =
  Query_Operation.register {name = "pide_mcp_interim_goal", pri = Task_Queue.urgent_pri}
    (fn {state = st, args, writeln_result, ...} =>
      (case try Toplevel.proof_of st of
        NONE => error "No proof state at this position"
      | SOME state => writeln_result (interim_goal args state)));

end;
\<close>

end
