---
name: pide-mcp
description: Using the Isabelle proof assistant with the PIDE MCP server. Use this skill whenever working with Isabelle theory files (*.thy) or ML files (*.ML) or when you are dealing with Isabelle in the general sense and have access to the PIDE MCP.
metadata:
  version: "1.0"
---

## What I Do

Guidance for Isabelle formalizations and proof development using the PIDE MCP server.

## When To Use Me

Use this skill when working with Isabelle theory files (*.thy) or ML files (*.ML) via the PIDE MCP.

## Quick Reference

PIDE MCP comes with a customizable set of tools.
This SKILL file describes some basic tools that might be available.
Also load other SKILL files that describe tools offered by PIDE MCP.
Important rules (further elaborated in the sections below):

- PIDE MCP can manage several Isabelle PIDE sessions. **Some sessions might have already been started.**
- **Most tools take the id of a running session as input. Use `list_sessions` to get the ids and status of all sessions.**
- **After every edit, call `get_state`** to see status, errors, warnings.
- **Restrict by `start_line` / `end_line`** when querying to avoid analysing data for hundreds of finished commands in large theories.
- **`sorry` is reported as `bad`, not as an `error`.** A clean theory has 0 bad commands AND 0 errors AND 0 failures.
- **If `> 0` commands are running for more than ~30s, suspect a loop** - restructure rather than wait.
- **Use `get_session_state` whenever you think the process is stuck** - it shows progress of theories and lists currently running commands and resource statistics.
- **Use `get_session_state` for a global overview and `get_state` for theory-local information.**
- **The PIDE perspective** is set by some tools and marks the commands for which print functions are run, which return extra output e.g. automatic `quickcheck` and `solve_direct` information.
- **`create_scratch`**: use for exploration and alternatives. (Typically) use the same imports on scratch theories as the ones you use for the development that you are creating a scratch theory for.

## Interaction with Isabelle via PIDE MCP

### Session Management

- **Each (PIDE) session has a unique id and its own base session, directories, options, and independent state.** A file loaded in one is invisible to the others.
- Most tools take a running session's id as input, which may be omitted when exactly one session runs.
- **`start_session` is synchronous and may take minutes** as it builds the base session if missing.
    **Use it only when required**, e.g. to restart an unresponsive session, after a `ROOT` file change, or to move to a different base session.
- **Only use `stop_session` when required**, e.g. when you have to restart a session or urgently need to free resources.
- `stop_session` keeps files on disk, unless some tool decides to cleanup (e.g. `create_scratch` deletes the session's scratch files).
- **`unload` unloads a file and its dependents from the session. Only use it when required**, e.g. when you are 100% certain a theory got unresponsive or you urgently need to free resources.

### State Synchronization
- **All changes are immediately checked** by PIDE after edits.
- **You may edit with whatever is convenient:** the `edit` tool, your own file tools, or scripts (`sed`, Python,...) for mechanical or multi-location rewrites. Write each file in one go, not in several partial steps.
- **`get_state`, `get_session_state`, and `find_entities` automatically pick up changes made on disk** to all loaded files (also by a human who saves their editor), and send only the changed lines to PIDE. After an edit with other tools, just call `get_state`; no `read` is needed. The result then lists the file under `external_edits_synced`, with the changed lines in the new text (at most 10 spans). These lines only show where the text changed: a local edit can break commands anywhere after it (and in dependent theories), so check the whole file's state, not just those lines.
- After syncing, `get_state` waits up to 5 s for the changed lines to be processed. After a big rewrite, many commands may still be reprocessing when it answers: call it again before trusting the counts.
- **Use `read` to re-synchronise** only for files that are not loaded yet, or if the state still does not match the disk (changes are detected by modification time and size, so a same-size edit within the same timestamp tick can be missed). Base session theories are static and never synchronised.
- Reading unloaded theories with other tools (e.g. grepping the AFP) is fine.
- **Avoid absolute file path imports for theories. Use session-qualified theory names for sessions available to you.** See `get_session_state` on how to find session qualifiers.
- **`edit` reads its file from disk before matching `old_text`.** If `old_text` is rejected as not found, the text really differs from what you expect (e.g. a human edited it): `read` the range again. Symbols match in either form (`\<rightarrow>` or `→`); lines outside the edited range keep their bytes on disk.
- `get_state` on a file that is not loaded yet (e.g. one you just wrote with your own tools) loads it and waits up to 10 s for it.

### After Every Edit
1. **Check for errors/unfinished/successful commands after edits (also with other tools) with `get_state`**, which synchronises disk edits first.
2. **`sorry`s are included as `bad` commands. `errors` counts actual failures. A "clean" state requires both to be 0.**

### Time Management
- **Avoid adding large amounts of new material at once, as it makes it hard to identify the source of errors and nontermination.**
- **Proof methods typically terminate in less than 5 seconds.**
- **A proof step (`apply`, `by`, `proof`, `qed`, `done`, `.`, `..`) is interrupted when its real run reaches 5 s:** `get_state` shows it as `canceled` with `interrupted_at_limit_seconds`, and a step that does not run in parallel (not the `by` of a lemma) holds up the commands after it until you edit it. Once a step has run for 5 s (by default; also while it still runs, without interrupting it), its entry shows `interim_goal` shortly after: the goal state that the step's method had reached after about 200 ms, when it is run again with apply_inspect (within a `simp` call the term it had rewritten to, so a loop shows as a growing term), the theorems it used with how often (`interim_theorems_used`) and loop warnings (`interim_loops`); if the method finishes or fails within that time, its resulting goal state or the state after its last step.
- **`apply_inspect` (tool) shows what a slow or looping method does, without changing the file:** with `origin` and the `start_line` of a proof step, it runs the step's method (or `method`) on the goal before that step, stops it at `timeout_ms` (default 200) and returns the goal state reached then, the theorems used until then (with how often, most frequent first) and loop warnings; a method that finishes in time gives its resulting goal state, one that fails the state after its last step. Call it with a few `timeout_ms` values (e.g. 200, 1000, 3000) to see how the goal develops. In a theory you can do the same by hand: import `"$PIDE_MCP_HOME/ml/PIDE_MCP_Apply_Inspect"`, write `apply_inspect <method>` and `back` lines (the goal state at 200 ms, then 100 ms later per `back`, up to 5 s; it always continues with the state reached, as a skipped proof); that changes the imports and reprocesses the file, so prefer the tool. Replace the method by a faster one (fewer rules, `simp only:`, an explicit rule, a split goal). Only for a step that really needs longer, run it once without the limit with `retry_without_limit` (origin and the step's line); an edit of the step stops it and brings the limit back. If many steps of a proof legitimately need longer, raise the limit for all steps instead of splitting them into many small ones: `set_step_time_limit` with `milliseconds` (e.g. 20000; 0: no limit) changes it for the running session, and steps interrupted at the lower limit run again. Raising the limit does not delay the interim goal: a step still gets it after 5 s (`interim_goal_after_milliseconds` of the same tool; 0: none). Some ML code does not react to interrupts. `try0`, `sledgehammer` and other diagnostic commands have no limit.
- If commands take longer, be suspicious! Only if you are very confident that a proof legitimately needs more time wait a bit longer. Short waits typically let you move faster!
- **If `get_state` shows `> 0` running commands for more than ~30 seconds, suspect non-termination** (a loop or overly expensive computation). Restructure rather than wait.
- **Use `get_session_state` to show the progress and currently running commands across all theories.**

### Scratch Theories
- **Use `create_scratch` to test large changes in a scratch theory before radically changing an existing theory.**
- **Scratch theories persist for the session**. They are deleted on session stop. You may further change and explore them once created in the course of the session.
- Use the scratch theories to try proofs and search tools without polluting your main development. Exception: if it is just a brief exploration (e.g. calling `sledgehammer` or `find_theorems` on the currently open goals), just do it in the same theory.
- For scratch theories, pass the imports matching your needs. Typically, it is the same imports as the ones you use for the development that you are creating a scratch theory for.

**Incremental workflow** (for developing complex proofs):
```
1. create_scratch → scratch theory_path

2. edit → extend the scratch theory (insert new content)

3. get_state → check the new status in the scratch theory

4. happy with the result → write it back to the original file
```

This allows you to test proof strategies and alternative developments without changing the original file.
Final results can be written back to the original file.

### Querying (Proof) Context
- **`get_state` returns structured information at any command position, including status (check it after edits), subgoals, prover messages (errors, warnings, writelns, .etc), type information for terms,...**
- **`get_state` accepts flags to opt into some of the data**, like `include_types` (for Isabelle and ML), `include_facts` (theorems used in range), etc.
- **Set the `get_state` commands limit** when you only care about summary information or a limited number of commands - a large number of commands may flood your context.
- **Restrict by `start_line` / `end_line`** when you only care about a specific lemma or error - it is less verbose than scanning the whole file. But don't forget that local changes may have global effects.
- **`find_entities` lets you explore defined entities (theorems, definitions, methods, ML terms, etc.)**.
Moreover, you can use Isar commands if necessary, for example:
- **`print_facts` - print all facts of the current context (named assumptions, case facts, intermediate results, locale assumptions, etc.)**
- `find_theorems`
- `find_consts`

### Importing existing library, facts, and definitions
- **Use `get_session_state` to get all available session directories, often containing useful library material.**
- Import theories from these sessions using the right **session-qualified import**.
- **To get session qualifiers, grep the `ROOT` files in the session directories.**

### Common Pitfalls

#### Large Changes
- ❌ **Don't:** Add hundreds of lines of new definitions and proofs in one edit
- ✅ **Do:** Add incrementally - a few definitions at a time, check after each
    - Easier to identify source of errors
    - Faster feedback on timeouts
    - Better isolation of problematic proofs

#### Status Checking
- ❌ **Don't:** Make multiple edits without checking status
    ```
    edit  (* Add lemma 1 *)
    edit  (* Add lemma 2 *)
    edit  (* Add lemma 3 - which one has the error? *)
    ```
- ✅ **Do:** Check `get_state` after each significant edit
    ```
    edit
    get_state  (* Verify status: ok *)
    edit  (* Continue only after verification *)
    ```

#### Scratch Theory Usage
- ❌ **Don't:** Try to import scratch theories from your main development
- ✅ **Do:** Use scratch theories for experimentation, then copy successful results back to main theory
    ```
    1. create_scratch to test approach
    2. Verify it works in scratch theory
    3. Copy successful proof back to main theory with edit
    ```

### Error Recovery

If you encounter errors or non-termination:
1. Check the exact error.
2. Isolate the problem: use `sorry` to skip problematic parts temporarily.
3. Make incremental fixes: make small changes and check after each.
4. Use `create_scratch` for experimentation and alternatives.
5. Increase wait for termination if needed, but prefer refactoring over long waits!
6. If you think the process is unresponsive, use `get_session_state` to find information. If all other means fail, unload the theory and, if that also fails, restart the session.

