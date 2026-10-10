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
- **You may edit with whatever is convenient:** the `edit` tool, or scripts (`sed`, Python,...) for mechanical or multi-location rewrites. Write each file in one go, not in several partial steps.
- **`get_state`, `get_session_state`, and `find_entities` automatically pick up changes made on disk** to all loaded files (also by a human who saves their editor), and send only the changed lines to PIDE. After a scripted edit, just call `get_state`; no `read` is needed. The result then contains `external edits synced: <file> (N of M lines in K spans; lines 12, 30-31, 62 (removed))` giving the changed lines in the new text, capped at 10 spans (`external_edits_synced` in `format: json`). The note also names a `full diff` file: it is for the human reviewer, so do not read it (it would only use up your context) unless asked. When you tell the user about a scripted edit, give them that file as a markdown link, e.g. `[sync diff](/abs/path/sync_x.diff)`, because tool output is not clickable for them. (If the note instead says `full diff shown to the user`, the project's PostToolUse hook already showed them the path: nothing to do.) The ranges only show where the text changed: a local edit can break commands anywhere after it (and in dependent theories), so check the whole file's state, not just those lines.
- After a big rewrite, many commands are still reprocessing when `get_state` answers: use `wait_seconds` or retry before trusting the counts.
- **Use `read` to re-synchronise** only for files that are not loaded yet, or if the state still does not match the disk (changes are detected by modification time and size, so a same-size edit within the same timestamp tick can be missed). Base session theories are static and never synchronised.
- Reading unloaded theories with other tools (e.g. grepping the AFP) is fine.
- **Avoid absolute file path imports for theories. Use session-qualified theory names for sessions available to you.** See `get_session_state` on how to find session qualifiers.
- **`edit` synchronises its file from disk before matching `old_text`.** If `old_text` is rejected as not found, the text really differs from what you expect (e.g. a human edited it): `read` the range again.

### After Every Edit
1. **Check for errors/unfinished/successful commands after edits (also scripted ones) with `get_state`**, which synchronises disk edits first.
2. **`sorry`s are included as `bad` commands. `errors` counts actual failures. A "clean" state requires both to be 0.**

### Time Management
- **Avoid adding large amounts of new material at once, as it makes it hard to identify the source of errors and nontermination.**
- **Proof methods typically terminate in less than 5 seconds.**
- If commands take longer, be suspicious! Only if you are very confident that a proof legitimately needs more time wait a bit longer. Short waits typically let you move faster!
- **If `get_state` shows `> 0` running commands for more than ~30 seconds, suspect non-termination** (a loop or overly expensive computation). Restructure rather than wait.
- **Use `get_session_state` to show the progress and currently running commands across all theories.**

### Scratch Theories
- **Use `create_scratch` to test large changes in a scratch theory before radically changing an existing theory.**
- **Scratch theories persist for the session**. They are deleted on session stop. You may further change and explore them once created in the course of the session.
- Use the scratch theories to try proofs and search tools without polluting your main development. Exception: if it is just a brief exploration (e.g. calling `sledgehammer` or `find_theorems` on the currently open goals), just do it in the same theory.
- **`create_scratch` returns the `path` and the `theory` name.** Use the returned path as `origin` as it is: the file name is fixed (`tmp_pide_mcp_scratch_<suffix>.thy`), so do not invent your own file name. The theory header must use the returned `theory` name. The extension defaults to `.thy` (pass `.ML` for ML files, which only load through a theory).
- For scratch theories, pass the imports matching your needs with the `imports` argument (e.g. `["Box.Seq"]`), which writes the `theory … imports … begin … end` skeleton for you; then add content with `edit`. Typically, use the same imports as the development that you are creating a scratch theory for. Without `imports`, the file is empty and you write the header yourself.

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
- **Choose the `get_state` `detail` level.** `summary` returns counts and the first failing command (a few hundred tokens). `locations` (the default unless `commands_limit` is positive) adds one compact entry per problematic command: line range, problem kinds, first message line, no source. `full` adds sources and all messages. A positive `commands_limit` implies `full`. `commands_limit` only limits the plain commands list: `commands_limit: 0` is fine for "what failed?", it still lists the problems (use `detail: summary` for counts only).
- **Each problematic command is listed once** in `problems` with its `kinds` (`failed`, `bad`, `errors`, `warnings`, ...). `problems_limit` (default 10) caps the list but the counts stay complete. `source_max_lines` (default 5) cuts long sources.
- **After a refactor, many commands fail as consequences of one early error.** Look at `first_problem` first: it is the first failing command in file order and usually the cause. `get_session_state` with `include_failures` gives the same per theory plus the lines of failing commands, so you can locate errors across theories without a large `get_state`.
- `include_infos` and `include_types` / `include_facts` can enlarge `full` output considerably; use them on narrow line ranges only.
- All tools return compact readable text. `get_state`, `get_session_state`, and `find_entities` accept `format: json` for structured JSON instead.
- **If a result exceeds the client's output limit,** Claude Code saves it to a file and tells you so. For the default text format, `grep` that file or `Read` it in line chunks. A `jq` suggestion only applies to `format: json`. Prefer a narrower line range, `detail: locations` or `summary`, or a smaller `problems_limit` over reading a huge result.
- Use `wait_seconds` on `get_state` for a file that is not loaded yet, or after editing an imported file, instead of retrying. If a file shows all commands `unprocessed` and a `waiting for imported theories still being processed: …` line (`waiting_for` in JSON), it is waiting for those upstream theories; `wait_seconds` blocks until the file itself is being processed, and `get_session_state` shows the upstream progress.
- A message in the `summary`/`locations` views ending in `(+N more lines, see detail: full)` is cut off: use `detail: full` on that line range to read all of it (the goal state and context are usually in the later lines).
- With `include_infos`, pass `include_ml_echo: false` to drop the echo of top-level ML declarations (`val x = …`, `structure …`) and keep explicit `writeln` output.
- `include_infos` output is capped per message (`info_max_lines`, default 30): ML top-level bindings echo their values. Print only the fields you need, or wrap the code in `let` so that nothing is echoed.
- **Restrict by `start_line` / `end_line`** when you only care about a specific lemma or error - it is less verbose than scanning the whole file. But don't forget that local changes may have global effects.
- **`find_entities` lets you explore defined entities (theorems, definitions, methods, ML terms, etc.)**.
Moreover, you can use Isar commands if necessary, for example:
- **`print_facts` - print all facts of the current context (named assumptions, case facts, intermediate results, locale assumptions, etc.)**
- `find_theorems`
- `find_consts`

### ML Files
- **Work on `*.ML` files through the MCP like on theories**: `read`, `edit`, `get_state`, and `find_entities` take the file path as `origin`. External edits (scripts, `sed`) are synchronised as for theories.
- **An ML file is only processed as part of the theory that loads it** (`ML_file "Foo.ML"`). Load that theory first (`read` it); until then `get_state` on the ML file only queues it. "Not loaded by any theory" means no loaded theory loads the file.
- **The whole ML file is one command** (the `ML_file` command). `get_state` therefore shows one entry for it, whose first message names the failing line (`ML error (line N of …)`). Use `detail: full` to see the complete message. Dependents in the loading theory are rechecked automatically after every change.
- **Large ML files are reprocessed completely after each edit.** For exploration, test the code in a scratch theory with the same imports using `ML \<open>…\<close>` blocks: each block is its own command, so you get per-block status, types (`include_types`), and output in seconds. Move the finished code into the `.ML` file afterwards.
- **Do not fall back to `isabelle build` just because the file is ML.** The MCP rechecks incrementally, shows ML errors with their lines, and keeps the session loaded; a batch build restarts from the session image and shows less.

### When a batch build is the better tool
- For a mechanical change across many lines where few errors are expected, or for sessions the PIDE session is not based on (e.g. a test session built on the session you are editing), `isabelle build` and grepping its `***` lines is simpler and cheap.
- For proof debugging, goal states, `find_theorems`, and checking downstream theories, the MCP is better: it rechecks incrementally in seconds.
- To see whether later theories are healthy while an early theory is broken, put `sorry` in (or comment out) the broken part of the early theory.

### Starting a session for a project with a ROOT file
`start_session` without arguments gives a plain `HOL` session. For a project, set `logic` to the session your edited theories build **on** (the parent session, not the session they belong to) and `dirs` to the directories containing the project's `ROOT` files, e.g. `logic: "MyParent"`, `dirs: ["/path/to/project"]`.

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

