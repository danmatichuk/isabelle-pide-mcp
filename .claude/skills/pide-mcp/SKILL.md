---
name: pide-mcp
description: Using the Isabelle proof assistant with the PIDE MCP server. Use this skill whenever working with Isabelle theory files (*.thy) or ML files (*.ML) or when you are dealing with Isabelle in the general sense. Load it also when the PIDE MCP is expected but seems unavailable: it says what to do then (report to the user, no silent fallback to batch builds).
metadata:
  version: "1.1"
---

Workflow for Isabelle theories and ML files through the PIDE MCP server. Parameters are explained in the tool descriptions.
Read these only when you need them:
- [sessions.md](sessions.md): starting a session for a project with a ROOT file, libraries and session-qualified imports, when a batch build is better.
- [ml_files.md](ml_files.md): working on `.ML` files.

## Before You Start: Check That the MCP Works

**At the start of every Isabelle task, check that the PIDE MCP works, and stop and tell the user if it does not.** Do this before reading, editing, or building anything.

1. **The tools must exist** (`get_state`, `start_session`, ...). If they are deferred, load them with ToolSearch, **together with the proof tools** you will need: `select:mcp__isabelle__sledgehammer,mcp__isabelle__suggest_facts,mcp__isabelle__find_theorems,mcp__isabelle__find_consts,mcp__isabelle__create_scratch,mcp__isabelle__cancel,mcp__isabelle__get_session_state`. Tools you never loaded are tools you will not think of.
2. **The server must answer:** call `list_sessions`.
3. **A session must be usable:** if none runs, `start_session` (may take minutes). Some sessions may already run; each has its own state, and most tools take its id (`session`, optional if only one runs).

**If one of these fails: stop, do not work around it.** Tell the user what failed with the **exact error text** and what you checked, and ask how to proceed. Do not silently fall back to `isabelle build`, blind edits or `grep`; a batch fallback needs the user's explicit agreement. The same holds mid-task when the server or session dies. Errors in the Isabelle content (failed proofs, type errors) are normal, not MCP failures. After a server restart, tool schemas you loaded may be outdated: try a documented parameter anyway, or reload the schema.

## Edit, Then `get_state`

- **Add a few definitions or lemmas at a time,** not hundreds of lines: errors and loops are then easy to locate.
- **Edit with `edit`** (it returns as soon as the text is written) or with scripts (`sed`, Python) for changes in several places; `get_state` picks up disk edits. Edit one file one call after the other.
- **After every edit, call `get_state` right away, without `wait_seconds`,** on a narrow `start_line`/`end_line` around what you changed; check the file-wide counts too, since a local change can break later commands.
- **A clean theory has 0 failed, 0 errors and 0 `bad`. `sorry` is reported as `bad`, not as an error.**
- **`still_running_possibly_nonterminating` is a signal, not something to wait for.** Methods usually finish in under 5 s. If one is still running after a few seconds (a big `simp`/`auto`, a VCG), replace it by a cheaper or more targeted one (fewer rules, `simp only:`, an explicit rule, a split goal). `cancel` (origin + line) interrupts it without unloading the file; `edit` with `cancel_after_seconds: N` interrupts edited commands that run longer than N s. After a cancel, edit that command: commands after it are processed again only then. Past ~30 s, assume a loop.
- **Fence off the rest of the theory while you work on a proof:** put `end` lines right after it, one per open `context`/`locale … begin` block plus one for the theory (spare ones do no harm). The commands after the fence then fail at once with `missing theory context` or `Bad context` errors, which are expected and cost no time: look only at the problems before the fence, and remove it when you move on.
- **After a refactor, read `first_problem` first:** later failures are often its consequences. Use `sorry` to set aside a part that blocks the rest.
- **Before a call that may take long** (`start_session`, `get_state` with `wait_seconds` ≥ 10), say in one line what you check and the longest wait, and afterwards what came back: the user only sees a spinner.
- If the prover seems stuck, `get_session_state` shows the running commands of all theories. Only as a last resort `unload` the theory or restart the session.

## Finding Facts and Experimenting

- **Use `find_theorems` / `find_consts` before guessing lemma names** (by name, `name: rev`, or by pattern, `"rev (rev ?xs) = ?xs"`); they do not modify the file. `suggest_facts` lists the lemmas Sledgehammer considers relevant for a goal. In a proof, `print_facts` shows the local facts.
- **A goal shown as `A &&& B` is several subgoals in one: apply any method, even the no-op `apply -`, to split it,** then solve each or use `simp_all`/`auto`. A method splits first and then acts on subgoal 1, so `rule conjI`/`conjunctionI` fail there, and a `Failed to apply proof method` error shows the unsplit goal from before the method. Goals of `instance` (`OFCLASS(...)`) or `interpretation` are one predicate: split them with `standard` / `unfold_locales`.
- **Experiment in a scratch theory (`create_scratch`)** with the same imports as your development: use the returned path and `theory` name as they are, never import a scratch theory from the development, and copy what works back. A quick `sledgehammer` or `find_theorems` on the current goal can stay in the file.
- **Test ML against the live theory state with `eval_ml`,** not with `tracing` calls in an ML file (each probe reloads it and its dependents).
