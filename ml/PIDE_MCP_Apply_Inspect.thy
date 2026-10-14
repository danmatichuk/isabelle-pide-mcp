(*  Title:      PIDE_MCP/ml/PIDE_MCP_Apply_Inspect.thy

Helper theory, loaded on demand by the `apply_inspect` tool of the PIDE MCP server.
It imports Apply_Inspect from the apply_inspect repository (the git submodule deps/apply_inspect:
the `apply_inspect` command, a diagnostic variant of `apply` that reports the theorems a failed
proof method used) and registers a PIDE query operation that runs a method on the proof state at a
position, without changing the theory.
*)

theory PIDE_MCP_Apply_Inspect
  imports "../deps/apply_inspect/Apply_Inspect"
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
              "The method did not finish within " ^ timeout_arg ^ " s (it may loop)."
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

val _ =
  Query_Operation.register {name = "pide_mcp_apply_inspect", pri = Task_Queue.urgent_pri}
    (fn {state = st, args, writeln_result, ...} =>
      (case try Toplevel.proof_of st of
        NONE => error "No proof state at this position: use a line inside a proof with an open goal"
      | SOME state => writeln_result (report args state)));

end;
\<close>

end
