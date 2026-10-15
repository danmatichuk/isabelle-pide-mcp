PIDE MCP NEWS -- history of user-relevant changes
=================================================

New in this PIDE MCP version
----------------------------

* `edit` keeps the lines outside the edited range byte for byte (before, the whole file was symbol-encoded on write, so literal
    Unicode symbols elsewhere turned into `\<Longrightarrow>` etc.); `old_text` is matched with symbols in Unicode form.
* `edit` refuses unknown arguments (nothing is done); other tools note them in the result (before, they were ignored silently).
* Time limit for proof steps: a step (`apply`, `by`, `proof`, `qed`, `done`, `.`, `..`) whose real run (also of a forked proof) reaches
    the new option `pide_mcp_step_time_limit` (default 5 s; 0: no limit) is interrupted. `get_state` shows it as `canceled`, with
    `interrupted_at_limit_seconds` and a hint. The new tool `retry_without_limit` runs such a step again, once, without the limit
    (`no_time_limit`); an edit of the step brings the limit back. The `timing_ms` of a running forked proof is its running time
    (before: 0).
* `get_state`, `get_session_state`, and `find_entities` pick up edits of loaded files made on disk outside of the server (scripts, other editors, the agent's
    own file tools) before they answer, so no `read` is needed after such edits. Only the changed line spans are sent to PIDE. The result lists the
    synced files under `external_edits_synced`, with the changed lines; after syncing, `get_state` waits up to 5 s for the changed lines to be processed.
* Tool results are readable YAML-like text by default (no JSON quoting, real line breaks), returned as content only, without
    `structuredContent`. Every tool takes the argument `format` (`yaml` or `json`; `json` returns the previous structured result);
    the new option `pide_mcp_output_format` sets the default. INCOMPATIBILITY.
* Add Isabelle/ML skill file.
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
