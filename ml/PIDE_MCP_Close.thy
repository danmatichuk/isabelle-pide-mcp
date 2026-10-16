(*  Title:      PIDE_MCP/ml/PIDE_MCP_Close.thy

Helper theory of the PIDE MCP server, loaded in the background after a session start (Pure only, so in every
session). It defines the protocol command PIDE_MCP.close_exec for a proof step that the server interrupted at its
time limit: sometimes its evaluation stops, but the closing status of the command (`failed`, `finished`, `canceled`,
as sent by Command.eval_state for an interrupted command) never arrives, and PIDE keeps showing it as running.
The command sends that status for the exec, but only if the exec has stopped: all its groups are cancelled and none
of them has tasks left.
*)

theory PIDE_MCP_Close
  imports Pure
begin

ML \<open>
(*args: exec id, number of `running` marks still open (runs), number of open forks (forks), as the server's
  document state counts them for the exec*)
val _ =
  Protocol_Command.define "PIDE_MCP.close_exec"
    (fn [exec_arg, runs_arg, forks_arg] =>
      let
        val groups = Execution.peek (Document_ID.parse exec_arg);
        val stopped =
          not (null groups) andalso forall Task_Queue.is_canceled groups andalso null (Future.snapshot groups);
        fun count arg = Int.max (0, the_default 0 (Int.fromString arg));
        val marks =
          [Markup.failed] @ replicate (count runs_arg) Markup.finished @
          replicate (count forks_arg) Markup.joined @ [Markup.canceled];
      in
        if stopped then
          Position.setmp_thread_data (Position.id_only exec_arg)
            (fn () => Output.status (map YXML.output_markup_only marks)) ()
        else ()
      end
    | _ => ());
\<close>

end
