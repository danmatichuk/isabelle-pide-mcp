(*  Title:      PIDE_MCP/ml/PIDE_MCP_Apply_Inspect.thy

Helper theory of the PIDE MCP server, loaded in the background after a session start. It imports
Apply_Inspect from the apply_inspect repository (the git submodule deps/apply_inspect: the
`apply_inspect` command, a diagnostic variant of `apply`) and sets its time limits: the method is
paused after apply_inspect_timeout, and each `back` continues it for apply_inspect_step_time. A
theory that imports this one gets the same limits for its own `apply_inspect`.

The query operation pide_mcp_interim_goal runs a method like `apply_inspect` with these limits on
the proof state at a position, without changing the theory: the goal state that the method has
reached when it is paused (for a proof step interrupted at pide_mcp_step_time_limit).
*)

theory PIDE_MCP_Apply_Inspect
  imports "../deps/apply_inspect/Apply_Inspect"
begin

declare [[apply_inspect_timeout = 200, apply_inspect_step_time = 100]]

ML \<open>
structure PIDE_MCP_Apply_Inspect =
struct

(*the limits declared above, also for the query, which runs in the context of another theory*)
val limits =
 {timeout = Time.fromMilliseconds (Config.get_global \<^theory> Apply_Inspect.timeout),
  step_time = Time.fromMilliseconds (Config.get_global \<^theory> Apply_Inspect.step_time)};

val params: Apply_Inspect.params =
  {every_use = false, on_use = fn _ => fn _ => (), on_loop = fn _ => fn _ => ()};

fun ms t = string_of_int (Time.toMilliseconds t);

fun goal_text st =
  Pretty.string_of (Goal_Display.pretty_goal (Proof.context_of st) (#goal (Proof.goal st)));

(*lines: "outcome: paused|finished|failed", "timeout_ms: N", "step_ms: N", "time_ms: N" (paused),
  "error: ..." (failed), "goal:" and the goal state reached (paused)*)
fun interim_goal [method_arg] state =
      let
        val ctxt = Proof.context_of state;
        val text =
          Parse.read_embedded ctxt (Keyword.no_major_keywords (Thy_Header.get_keywords' ctxt))
            (Scan.many Token.not_eof) (Input.string method_arg)
          |> Method.read ctxt;
        val {succeeded, error, partial, ...} = Apply_Inspect.inspect_limited params limits text state;
        val limit_lines = ["timeout_ms: " ^ ms (#timeout limits), "step_ms: " ^ ms (#step_time limits)];
      in
        (case Seq.pull partial of
          SOME ({time, state = st, ...}, _) =>
            cat_lines (["outcome: paused"] @ limit_lines @ ["time_ms: " ^ ms time, "goal:", goal_text st])
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
