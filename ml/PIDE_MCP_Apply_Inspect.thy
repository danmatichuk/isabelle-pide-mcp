(*  Title:      PIDE_MCP/ml/PIDE_MCP_Apply_Inspect.thy

Helper theory, loaded on demand by the `apply_inspect` tool of the PIDE MCP server.
It imports Apply_Inspect from the apply_inspect repository (the git submodule deps/apply_inspect:
the `apply_inspect` command, a diagnostic variant of `apply` that reports the theorems a failed
proof method used) and registers a PIDE query operation that runs a method on the proof state at a
position, without changing the theory.
*)

theory PIDE_MCP_Apply_Inspect
  imports "../deps/apply_inspect/Apply_Inspect" PIDE_MCP_Suggest
begin

ML \<open>
structure PIDE_MCP_Apply_Inspect =
struct

fun plain_limit limit prt =
  XML.content_of (YXML.parse_body (Pretty.unformatted_string_of prt))
  |> String.tokens Char.isSpace |> space_implode " "
  |> (fn s => if size s > limit then String.substring (s, 0, limit) ^ " ..." else s);

val plain = plain_limit 300;

(*statement of a used theorem or fact (assumptions and other rules print their statement anyway)*)
fun statement ctxt (u as Apply_Inspect.Theorem _) = Option.map Thm.prop_of (Apply_Inspect.use_thm ctxt u)
  | statement ctxt (u as Apply_Inspect.Fact _) = Option.map Thm.prop_of (Apply_Inspect.use_thm ctxt u)
  | statement _ _ = NONE;

(*args: method, find_theorems criteria (may be empty), maximum number of uses, timeout in seconds,
  whether to add the statement of each theorem*)
fun report [method_arg, criteria_arg, max_uses_arg, timeout_arg, statements_arg] state =
      let
        val ctxt = Proof.context_of state;
        val pretty_use = Apply_Inspect.pretty_use ctxt;  (*builds the declaration tables once*)
        (*not Method.read_closure_input, which runs the method once on a dummy goal outside of
          inspect: inspect closes the method itself and reports its errors*)
        val text =
          Parse.read_embedded ctxt (Keyword.no_major_keywords (Thy_Header.get_keywords' ctxt))
            (Scan.many Token.not_eof) (Input.string method_arg)
          |> Method.read ctxt;
        val printed =
          if criteria_arg = "" then K true
          else Apply_Inspect.use_filter state (Find_Theorems.read_query Position.none criteria_arg);
        val max_uses = the_default 100 (Int.fromString max_uses_arg);
        val timeout = Time.fromSeconds (the_default 10 (Int.fromString timeout_arg));

        (*uses and loops are collected as they are reported: a method that runs into the timeout
          or out of memory does not return its result*)
        val uses = Synchronized.var "pide_mcp_apply_inspect_uses" ([]: string list);
        fun on_use _ u =
          if printed u then
            let
              val s = plain (pretty_use u) ^
                (if statements_arg = "true" then
                  (case statement ctxt u of
                    SOME prop => ": " ^ plain_limit 2000 (Syntax.pretty_term ctxt prop)
                  | NONE => "")
                 else "");
            in Synchronized.change uses (fn us => if member (op =) us s then us else s :: us) end
          else ();
        val loops = Synchronized.var "pide_mcp_apply_inspect_loops" ([]: string list);
        fun on_loop source rules =
          let
            val what =
              (case source of
                "simp" => "the simplifier"
              | "classical" => "the classical reasoner"
              | _ => "the proof method " ^ quote source);
            val names =
              if null rules then "(no named theorems)"
              else commas (map (plain o pretty_use) rules);
          in
            Synchronized.change loops
              (cons ("*** may be looping: " ^ what ^ " repeatedly uses " ^ names ^ " ***"))
          end;
        val params = {every_use = false, on_use = on_use, on_loop = on_loop};

        val outcome =
          (case Exn.capture (fn () =>
              Timeout.apply timeout (fn () => Apply_Inspect.inspect params text state) ()) () of
            Exn.Res {succeeded, error, uses = all} =>
              (List.app (on_use "") all;
               (case error of
                 NONE =>
                  if succeeded then "The method succeeds (nothing was applied)."
                  else "The method fails."
               | SOME msg => "The method raised an error: " ^ plain_limit 1000 (Pretty.str msg)))
          | Exn.Exn (Timeout.TIMEOUT _) =>
              "The instrumented method did not finish within " ^ timeout_arg ^ " s (the instrumentation " ^
                "makes it slower than the real method, whose timing get_state shows)."
          | Exn.Exn exn =>
              if Exn.is_interrupt_breakdown exn then
                "The method ran out of stack or memory (it may loop)."
              else Exn.reraise exn);

        val shown = sort_distinct string_ord (Synchronized.value uses);
        val n = length shown;
      in
        cat_lines
          (outcome :: rev (Synchronized.value loops) @
            (string_of_int n ^ " theorem" ^ (if n = 1 then "" else "s") ^ " used" ^
              (if criteria_arg = "" then "" else " (matching the criteria)") ^ ":") ::
            map (fn s => "  " ^ s) (take max_uses shown) @
            (if n > max_uses then ["  ... (" ^ string_of_int (n - max_uses) ^ " more)"] else []))
      end
  | report _ _ = error "Bad arguments";

(*trace for the automatic inspection of a slow or failed step: every use is counted (every_use), in order.
  args: method, timeout in seconds, maximum number of trace lines, and for a terminal step (by, done, qed, ...) the
  maximum number of suggested facts.
  Output: the outcome, loop warnings, for a terminal step whose method succeeds "subgoals left: N" (after the
  finishing step of a terminal proof), "counts:" with one "  N name<TAB>full name" line per theorem (most used first,
  with the declarations of the first use; "<TAB>(infrastructure)" appended for sort membership rules), "trace:" with
  one "  source: name" line per use in order, and when subgoals are left "suggested:" with the facts for the first
  of them (lines as PIDE_MCP_Suggest.suggest).*)
fun use_key (Apply_Inspect.Theorem (name, _)) = Thm_Name.print name
  | use_key (Apply_Inspect.Fact name) = Thm_Name.print name
  | use_key (Apply_Inspect.Assumption (name, _)) =
      if name = Thm_Name.empty then "(assumption)" else Thm_Name.print name
  | use_key (Apply_Inspect.Rule _) = "(other rule)";

(*sort membership rules: a statement OFCLASS(...), the rules Axclass records for a class (X_class_def,
  X_class.intro, X_class.axioms) and the intro rules and axioms of its class locale (class.X.intro,
  class.X_axioms.intro); not the class assumptions or class parameter definitions*)
fun sort_rule ctxt =
  let
    val thy = Proof_Context.theory_of ctxt;
    val axclass_rules =
      fold (fn c => fn tab =>
        (case try (Axclass.get_info thy) c of
          SOME {def, intro, axioms, ...} =>
            let
              val (locale_intro, locale_axioms_intro) =
                if Class.is_class thy c then Locale.intros_of thy c else (NONE, NONE);
              val locale_axioms = if Class.is_class thy c then Locale.axioms_of thy c else [];
            in
              fold (fn th => Symtab.update (Thm_Name.print (Thm.derivation_name th), ()))
                (def :: intro :: axioms @ the_list locale_intro @ the_list locale_axioms_intro @ locale_axioms) tab
            end
        | NONE => tab)) (Sorts.all_classes (Sign.classes_of thy)) Symtab.empty;
  in
    fn (key, u) => Symtab.defined axclass_rules key orelse
      (case Apply_Inspect.use_thm ctxt u of
        SOME th => can Logic.dest_of_class (Logic.strip_imp_concl (Thm.prop_of th))
      | NONE => false)
  end;

fun trace (method_arg :: timeout_arg :: trace_max_arg :: terminal_args) state =
      let
        val ctxt = Proof.context_of state;
        val pretty_use = Apply_Inspect.pretty_use ctxt;
        val text =
          Parse.read_embedded ctxt (Keyword.no_major_keywords (Thy_Header.get_keywords' ctxt))
            (Scan.many Token.not_eof) (Input.string method_arg)
          |> Method.read ctxt;
        val timeout = Time.fromSeconds (the_default 5 (Int.fromString timeout_arg));
        val trace_max = the_default 5000 (Int.fromString trace_max_arg);

        (*key -> (count, first use); the trace in reverse order, capped*)
        val counts = Synchronized.var "pide_mcp_apply_trace_counts" (Symtab.empty: (int * Apply_Inspect.use) Symtab.table);
        val trace = Synchronized.var "pide_mcp_apply_trace" (0, []: string list);
        fun on_use source u =
          let val key = use_key u in
            Synchronized.change counts (Symtab.map_default (key, (0, u)) (fn (n, u0) => (n + 1, u0)));
            Synchronized.change trace (fn (n, lines) =>
              (n + 1, if n < trace_max then (source ^ ": " ^ key) :: lines else lines))
          end;
        val loops = Synchronized.var "pide_mcp_apply_trace_loops" ([]: string list);
        fun on_loop source rules =
          let
            val what =
              (case source of
                "simp" => "the simplifier"
              | "classical" => "the classical reasoner"
              | _ => "the proof method " ^ quote source);
            val names = if null rules then "(no named theorems)" else commas (map use_key rules);
          in Synchronized.change loops (cons ("*** may be looping: " ^ what ^ " repeatedly uses " ^ names ^ " ***")) end;
        val params = {every_use = true, on_use = on_use, on_loop = on_loop};

        val outcome =
          (case Exn.capture (fn () =>
              Timeout.apply timeout (fn () => Apply_Inspect.inspect params text state) ()) () of
            Exn.Res {succeeded, error, uses = all} =>
              (*a failed method reports what it tried in its result only: count each at least once*)
              (List.app (fn u => Synchronized.change counts
                  (Symtab.map_default (use_key u, (0, u)) (fn (n, u0) => (Int.max (n, 1), u0)))) all;
               case error of
                NONE => if succeeded then "succeeds" else "fails"
              | SOME msg => "raises an error: " ^ plain_limit 300 (Pretty.str msg))
          | Exn.Exn (Timeout.TIMEOUT _) => "did not finish within " ^ timeout_arg ^ " s (it may loop)"
          | Exn.Exn exn =>
              if Exn.is_interrupt_breakdown exn then "ran out of stack or memory (it may loop)"
              else Exn.reraise exn);

        (*a terminal step whose method succeeds: the goal left after it and the finishing step, by the real method*)
        val suggest_max = (case terminal_args of [n] => the_default 0 (Int.fromString n) | _ => 0);
        val left =
          if suggest_max <= 0 orelse outcome <> "succeeds" then NONE
          else
            (case Exn.capture (fn () => Timeout.apply timeout (fn () =>
                Seq.pull (Seq.filter_results
                  (Proof.apply (Method.finish_text (SOME text, true), Position.no_range) state))) ()) () of
              Exn.Res (SOME (state', _)) =>
                let val n = Thm.nprems_of (#goal (Proof.goal state')) in
                  if n = 0 then SOME (0, [])
                  else SOME (n, the_default [] (try (PIDE_MCP_Suggest.suggest suggest_max true) state'))
                end
            | Exn.Res NONE => NONE
            | Exn.Exn exn => if Exn.is_interrupt exn then Exn.reraise exn else NONE);

        val sorted =
          Symtab.fold (fn (key, (n, u)) => cons (n, key, u)) (Synchronized.value counts) []
          |> sort (fn ((n1, k1, _), (n2, k2, _)) => prod_ord (rev_order o int_ord) string_ord ((n1, k1), (n2, k2)));
        val (n_trace, trace_lines) = Synchronized.value trace;
        val is_sort_rule = sort_rule ctxt;
      in
        cat_lines
          (outcome :: distinct (op =) (rev (Synchronized.value loops)) @
            (case left of SOME (n, _) => ["subgoals left: " ^ string_of_int n] | NONE => []) @
            ["counts:"] @
            map (fn (n, key, u) =>
              "  " ^ string_of_int n ^ " " ^ (if key = "(other rule)" then key else plain (pretty_use u)) ^
                "\t" ^ key ^ (if is_sort_rule (key, u) then "\t(infrastructure)" else "")) sorted @
            ["trace:"] @ map (fn s => "  " ^ s) (rev trace_lines) @
            (if n_trace > trace_max then ["  ... (" ^ string_of_int (n_trace - trace_max) ^ " more uses)"] else []) @
            (case left of SOME (_, facts as _ :: _) => "suggested:" :: facts | _ => []))
      end
  | trace _ _ = error "Bad arguments";

val _ =
  Query_Operation.register {name = "pide_mcp_apply_trace", pri = Task_Queue.urgent_pri}
    (fn {state = st, args, writeln_result, ...} =>
      (case try Toplevel.proof_of st of
        NONE => error "No proof state at this position"
      | SOME state => writeln_result (trace args state)));

val _ =
  Query_Operation.register {name = "pide_mcp_apply_inspect", pri = Task_Queue.urgent_pri}
    (fn {state = st, args, writeln_result, ...} =>
      (case try Toplevel.proof_of st of
        NONE => error "No proof state at this position: use a line inside a proof with an open goal"
      | SOME state => writeln_result (report args state)));

end;
\<close>

end
