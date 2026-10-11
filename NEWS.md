PIDE MCP NEWS -- history of user-relevant changes
=================================================

New in this PIDE MCP version
----------------------------

* The `pide-mcp` skill has a "Before You Start" section: check that the MCP tools exist and answer, and if not, stop and report the exact failure to the user instead of silently falling back to batch builds. The other skills and the server instructions point to it.
* The server instructions (always in the agent's context) now name the `pide-mcp` and `isabelle-proof-development` skills, ask for a `get_state` check after every edit, and recommend `try0`, then `sledgehammer [timeout = 10]` (always with a short timeout) for goals that do not close. The `isabelle-proof-development` skill and the sledgehammer tool extension say the same.
* `create_scratch` with `content` says in its result when the `theory` name in the content does not match the file name (or no header is there).
* `create_scratch`: the extension defaults to `.thy` and a missing leading dot is added (before, `create_scratch` without extension created an unloadable file). The result includes the `theory` name for the header. New argument `imports` initialises a new `.thy` file with a theory skeleton. Files without any extension now give a clear error instead of `Illegal character "/" in path element`.
* `get_state`: `commands_limit` 0 no longer implies `detail: summary`. The default `detail` is `full` for a positive `commands_limit` and `locations` otherwise, so that `commands_limit: 0` still lists the problems. New argument `include_ml_echo` (default true; with `include_infos`) to drop the echo of top-level ML declarations. INCOMPATIBILITY.
* `get_state` no longer lists whitespace and comment-only spans as commands (they appeared as blank entries with misleading line numbers and inflated the command counts).
* `get_state` with `wait_seconds` also waits while the file only waits for imported theories that are still being processed (e.g. after editing an imported file), instead of returning all commands as unprocessed. The result lists such theories as `waiting_for` (with progress), also without `wait_seconds`.
* `get_state` marks first messages that leave out further lines (`(+N more lines, see detail: full)`). New argument `info_max_lines` (default 30) caps each writeln/information/tracing message shown with `include_infos`, e.g. the echo of long ML bindings.
* `get_state` on a line range now reports the status, messages, `bad` markers and source of every command overlapping the range in full, instead of cutting them to the range: a failed multi-line command no longer shows up without its error message. Types, facts, and markup still respect the range. The first message in `summary` and `locations` skips header-only lines such as `ML error:`. For `.ML` files (a single command), errors outside the requested lines are reported as problem kind `errors_outside_range` with a hint instead of showing no problem.
* Synchronised external edits are also written as a unified diff to a file in directory `pide_mcp_sync_diff_dir` (default `.pide_mcp/sync_diffs`, relative to the server's working directory, i.e. the project; the directory ignores itself in git; set empty to disable); only the file name is reported to the agent, so that the diff can be reviewed without using up its context. The newest 100 diffs are kept.
* All tools now return readable text (real line breaks, no JSON quoting) as content only, without `structuredContent`. `find_entities` gets the `format` argument of `get_state` (`format: json` for the previous output). INCOMPATIBILITY.
* `get_state`, `get_session_state`, and `find_entities` now synchronise loaded files that were edited on disk outside of the server (e.g. by scripts or `sed`) before answering, so an explicit `read` is no longer needed. Only the changed line spans are sent to PIDE. The result reports them as `external_edits_synced`.
* `get_state` and `get_session_state` now return readable text (real line breaks, no JSON quoting) as content only, without `structuredContent`. Pass `format: json` for the previous JSON output including `structuredContent`. INCOMPATIBILITY.
* `get_session_state` has a new argument `include_failures`: first failing command and failing lines per theory.
* `get_state` has a new argument `wait_seconds` to wait for files that are not loaded or processed yet.
* `get_state` output is now compact. New arguments `detail` (`summary`, `locations`, `full`), `problems_limit`, and `source_max_lines`. Problematic commands are listed once in `problems` (with their `kinds`) instead of once per kind, and `first_problem` reports the first failing command. Command sources are truncated. The `commands_*` detail lists and `errors`/`warnings` detail lists are replaced by counts. INCOMPATIBILITY.
* Reading or editing a theory loads its unloaded files.
* `unload` now also unloads files of unloaded theories unless a remaining theory still loads them.
* Add [tool\_extensions](./tool_extensions) with experimental sledgehammer tool as an example user-level tool extension.
* Add overlay and query operations support.
* Remove `show_results` default value. Set options manually if required. INCOMPATIBILITY.
* Enable `show_states` only for live sessions, not for base session builds. Disable `editor_output_state` to avoid duplicate states. INCOMPATIBILITY.
* More robust session start and stop. Introduce new phase `building`. `stop_session` can now also stop sessions that are not running.
* Failing tool calls now return a result with `isError` instead of a JSON-RPC error. INCOMPATIBILITY.
* Tool results are now also returned as `structuredContent`.
* New options `pide_mcp_await_option_sessions` and `pide_mcp_exit_on_failed_option_sessions` control whether sessions given as command-line options are prepared before the MCP server starts
  and whether the MCP server stops if one such sessions fails to start.
* Command-line option `-L` now logs to the file in addition to the console. INCOMPATIBILITY.
* New command-line option `-w` logs MCP requests and responses while `-v` no longer does. INCOMPATIBILITY.
* Support of `notifications/cancelled` for long-running tool calls.
* Long-running operations now report progress, configurable with options `pide_mcp_session_progress_delay` 
  and `pide_mcp_progress_threshold`.
* New option `pide_mcp_tools` to enable/disable tools at startup time.
* New tool `get_session_state` shows session information, status, progress, and resource statistics, superseding tools `list_loaded_theories` and `list_session_directories`. INCOMPATIBILITY.
  Runtime statistics are reported as averages and maxima over a sampling window,
  configurable via option `pide_mcp_session_statistics_limit`.
* New command-line option `-S` starts further sessions on startup.
  Isabelle system options given outside `-S` are inherited by the sessions given via `-S`.
  An invocation with only Isabelle system options does not start a session.
  Use `-S ""` to force a session start in such cases. INCOMPATIBILITY.
* Support for multi session handling and new tools `start_session`, `stop_session`, and `list_sessions`.
  Existing tools now take an optional session id, which may be omitted when exactly one session runs.
  `PIDE_MCP_Tool`s now receive several sessions. INCOMPATIBILITY.
* Update `get_state`: include tracing, remove unprocessed command details, and fix wrong message line number attribution for blobs.
* Set visible perspective on last read or edited origin only. Dependencies are hidden. Use option `pide_mcp_range_context` to adjust number of lines around range that stay visible.
* Await stable snapshot after editing operations (without blocking other tools)
* Reworked loading and editing: closes several issues reported by Diego Marmsoler and Yong Kiam Tan (stuck process due to invalid imports and de-synchronisation of session and resources, edit race conditions, etc.)
* New tool `unload`: Remove ("clean") a theory and its dependents.
* Add tool plugin system: users can freely add and remove (new) tools.
* The `edit` tool now matches substrings within a given range and can edit multiple occurrences at once.
  This is in line with how most coding agents implement their edit tools.

New in PIDE MCP 2025-2
----------------------------

* Port of PIDE MCP c13a4bd3c018 compatible with Isabelle2025-2
* Includes all post PIDE MCP c13a4bd3c018 updates until the release of PIDE MCP 2026

New in PIDE MCP c13a4bd3c018
----------------------------

* Initial release compatible with Isabelle/c13a4bd3c018
