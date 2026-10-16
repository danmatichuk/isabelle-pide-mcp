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

(*args: method, timeout in ms (empty: apply_inspect_trace_start). The method runs like apply_inspect on the
  proof state, with that time as its timeout and as the start of the trace: as the trace takes samples only
  before the timeout, there are none, and the result is the goal state at the end of the run itself (frozen at
  the timeout; within a simp call, the term it has rewritten to), without a replay.
  Lines: "outcome: traced|finished|failed", "time_ms: N", "error: ...", "loop: ..." (loop warnings),
  "uses:" and "  theorem \<times>N" lines (theorems used until then and how often, most frequent first, without
  infrastructure), "goal:" and the goal: the
  state at the timeout, the result of a method that finishes, or the state after the last step of one that
  fails (none if it got nowhere)*)
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
        val {succeeded, error, use_counts, partial, final, ...} =
          Apply_Inspect.inspect_limited params
            {timeout = time, trace_start = time, trace_interval = time} text state;
        (*the query runs in the context of another theory, which does not see the apply_inspect_hide
          declarations of Apply_Inspect: they come from this theory*)
        fun infrastructure u =
          Apply_Inspect.infrastructure ctxt u orelse Apply_Inspect.infrastructure helper_ctxt u;
        (*uses that print the same are merged; the order of the first one is kept*)
        fun use_lines counts =
          let
            val merged =
              fold (fn (u, n) => fn acc =>
                if infrastructure u then acc
                else
                  let val name = plain (pretty_use u) in
                    (case AList.lookup (op =) acc name of
                      SOME m => AList.update (op =) (name, m + n) acc
                    | NONE => acc @ [(name, n)])
                  end) counts [];
          in "uses:" :: map (fn (name, n) => "  " ^ name ^ " \<times>" ^ string_of_int n) merged end;
        val loop_lines = map (prefix "loop: ") (distinct (op =) (rev (Synchronized.value loops)));
        val final_goal = (case final of SOME st => ["goal:", goal_text st] | NONE => []);
      in
        (case Seq.pull partial of
          SOME ({time, state = st, use_counts, ...}, _) =>
            cat_lines (["outcome: traced", "time_ms: " ^ ms time] @ loop_lines @ use_lines use_counts @
              ["goal:", goal_text st])
        | NONE =>
            if succeeded then cat_lines (["outcome: finished"] @ loop_lines @ use_lines use_counts @ final_goal)
            else
              cat_lines (["outcome: failed"] @ the_list (Option.map (prefix "error: ") error) @ loop_lines @
                use_lines use_counts @ final_goal))
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
