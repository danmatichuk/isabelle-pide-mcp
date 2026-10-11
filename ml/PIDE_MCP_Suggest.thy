(*  Title:      PIDE_MCP/ml/PIDE_MCP_Suggest.thy

Helper theory, loaded on demand by the `suggest_facts` tool of the PIDE MCP server.
It registers a PIDE query operation that returns the facts that Sledgehammer's relevance filter
(MePo/MaSh) selects for the goal at a position, without running any prover.
*)

theory PIDE_MCP_Suggest
  imports Main
begin

ML \<open>
local

fun fact_line ctxt with_statements (((name, _), thm): Sledgehammer_Fact.fact) =
  if with_statements then
    let
      val stmt =
        Print_Mode.setmp [] (fn () =>
          Pretty.unformatted_string_of (Syntax.pretty_term ctxt (Thm.prop_of thm))) ()
        |> String.tokens Char.isSpace |> space_implode " "
      val stmt = if size stmt > 300 then String.substring (stmt, 0, 300) ^ " ..." else stmt
    in "  " ^ name ^ ": " ^ stmt end
  else "  " ^ name;

in

val _ =
  Query_Operation.register {name = "pide_mcp_suggest_facts", pri = Task_Queue.urgent_pri}
    (fn {state = st, args, writeln_result, ...} =>
      (case try Toplevel.proof_of st of
        NONE => error "No proof state at this position: use a line inside a proof with an open goal"
      | SOME state =>
          let
            val [max_facts_arg, statements_arg] = args;
            val max_facts = the_default 20 (Int.fromString max_facts_arg);
            val with_statements = statements_arg = "true";
            val ctxt = Proof.context_of state;
            val params = Sledgehammer_Commands.default_params (Proof.theory_of state) [];
            val fact_override = Sledgehammer_Fact.no_fact_override;
            val {facts = chained_thms, goal, ...} = Proof.goal state;
            val (_, hyp_ts, concl_t) = ATP_Util.strip_subgoal goal 1 ctxt;
            val prover = hd (#provers params);
            val all_facts =
              Sledgehammer_Fact.nearly_all_facts_of_context ctxt false fact_override chained_thms
                hyp_ts concl_t;
            val factss =
              Sledgehammer_MaSh.relevant_facts ctxt params prover max_facts fact_override
                hyp_ts concl_t all_facts;
            fun section (filter, facts) =
              ("Selected " ^ string_of_int (length facts) ^ " " ^
                (if filter = "" then "" else filter ^ " ") ^ "fact" ^
                (if length facts = 1 then "" else "s") ^ " (most relevant first):") ::
              map (fact_line ctxt with_statements) facts;
          in
            writeln_result
              (cat_lines
                (if forall (null o snd) factss then ["Found no relevant facts"]
                 else maps section factss))
          end));

end;
\<close>

end
