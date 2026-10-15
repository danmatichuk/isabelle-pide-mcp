(*  Title:      PIDE_MCP/ml/PIDE_MCP_Apply_Inspect.thy

Helper theory of the PIDE MCP server, loaded in the background after a session start. It imports
Apply_Inspect from the apply_inspect repository (the git submodule deps/apply_inspect: the
`apply_inspect` command, a diagnostic variant of `apply`) and sets its time limits: the method runs
for at most apply_inspect_timeout; from apply_inspect_trace_start on, the goal state that it has
reached is recorded every apply_inspect_trace_interval; `apply_inspect` shows the first recorded
state and each `back` the next one. A theory that imports this one gets the same limits for its
own `apply_inspect`.

The query operation pide_mcp_apply_inspect runs a method like `apply_inspect` on the proof state
at a position, without changing the theory, and stops it after a given time (default:
apply_inspect_trace_start): the goal state that it has reached then. It serves the interim goal
of a proof step interrupted at pide_mcp_step_time_limit and the `apply_inspect` tool.
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

val helper_ctxt = Proof_Context.init_global \<^theory>;

fun ms t = string_of_int (Time.toMilliseconds t);

fun plain prt =
  XML.content_of (YXML.parse_body (Pretty.unformatted_string_of prt))
  |> String.tokens Char.isSpace |> space_implode " "
  |> (fn s => if size s > 300 then String.substring (s, 0, 300) ^ " ..." else s);

fun goal_text st =
  Pretty.string_of (Goal_Display.pretty_goal (Proof.context_of st) (#goal (Proof.goal st)));

(*args: method, time in ms (empty: apply_inspect_trace_start). The method runs like apply_inspect on the
  proof state, with that time as both its time limit and the start of the trace: the result is the one
  goal state that it has reached then (within a simp call, the term it has rewritten to).
  Lines: "outcome: traced|finished|failed", "time_ms: N", "error: ...", "loop: ..." (loop warnings),
  "uses:" and "  theorem" lines (theorems used until then, without infrastructure), "goal:" and the goal*)
fun inspect_at [method_arg, time_arg] state =
      let
        val ctxt = Proof.context_of state;
        val text =
          Parse.read_embedded ctxt (Keyword.no_major_keywords (Thy_Header.get_keywords' ctxt))
            (Scan.many Token.not_eof) (Input.string method_arg)
          |> Method.read ctxt;
        val time =
          (case Int.fromString time_arg of
            SOME n => Time.fromMilliseconds n
          | NONE => #trace_start limits);
        val pretty_use = Apply_Inspect.pretty_use ctxt;
        val loops = Synchronized.var "pide_mcp_apply_inspect_loops" ([]: string list);
        fun on_loop source rules =
          Synchronized.change loops (cons (source ^ " may be looping, repeatedly using: " ^
            (if null rules then "(no named theorems)" else commas (map (plain o pretty_use) rules))));
        val params = {every_use = false, on_use = fn _ => fn _ => (), on_loop = on_loop};
        val {succeeded, error, uses, partial} =
          Apply_Inspect.inspect_limited params
            {timeout = time, trace_start = time, trace_interval = time} text state;
        (*the query runs in the context of another theory, which does not see the apply_inspect_hide
          declarations of Apply_Inspect: they come from this theory*)
        fun infrastructure u =
          Apply_Inspect.infrastructure ctxt u orelse Apply_Inspect.infrastructure helper_ctxt u;
        fun use_lines us =
          "uses:" :: map (prefix "  ")
            (distinct (op =) (map (plain o pretty_use) (filter_out infrastructure us)));
        val loop_lines = map (prefix "loop: ") (distinct (op =) (rev (Synchronized.value loops)));
      in
        (case Seq.pull partial of
          SOME ({time, state = st, uses}, _) =>
            cat_lines (["outcome: traced", "time_ms: " ^ ms time] @ loop_lines @ use_lines uses @
              ["goal:", goal_text st])
        | NONE =>
            if succeeded then cat_lines (["outcome: finished"] @ loop_lines @ use_lines uses)
            else
              cat_lines (["outcome: failed"] @ the_list (Option.map (prefix "error: ") error) @ loop_lines @
                use_lines uses))
      end
  | inspect_at _ _ = error "Bad arguments";

val _ =
  Query_Operation.register {name = "pide_mcp_apply_inspect", pri = Task_Queue.urgent_pri}
    (fn {state = st, args, writeln_result, ...} =>
      (case try Toplevel.proof_of st of
        NONE => error "No proof state at this position"
      | SOME state => writeln_result (inspect_at args state)));

end;
\<close>

end
