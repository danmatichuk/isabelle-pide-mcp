# Sessions, Libraries, Batch Builds

## Session management
- Each PIDE session has its own base session, directories, options and state: a file loaded in one is invisible to the others.
- **`start_session` is synchronous and may take minutes** (it builds the base session if missing). Use it only when required: to restart an unresponsive session, after a `ROOT` file change, or to move to another base session. Use `stop_session` only to restart or to free resources; it keeps files on disk (scratch files are deleted).
- `unload` removes a file and its dependents from the session: only when a theory is certainly unresponsive or you must free resources.

## Starting a session for a project with a ROOT file
`start_session` without arguments gives a plain `HOL` session. For a project, pass `dirs` (the directories containing the project's `ROOT` files) and one of:

- **`logic` = the session your edited theories build on** (their parent session, not the session they belong to). The base image is built from that session; everything else is loaded dynamically. Example: `logic: "MyParent"`, `dirs: ["/path/to/project"]`.
- **`logic` = the session your edited theories belong to, with `session_requirements: true`.** The base image is built from the theories that session imports from other sessions, on top of an ancestor (default: the parent of `logic`; `session_ancestor` overrides it, and must be an ancestor of `logic`). The session's own theories are loaded dynamically, so you can edit them. Example: `logic: "MyProject"`, `session_requirements: true`, `dirs: ["/path/to/project"]`.

Use `session_requirements` when you do not want to work out the parent session yourself, or when the session imports further sessions beside its parent. `no_build: true` checks whether the image exists without building it.

## Libraries and imports
- `get_session_state` with `include_session_dirs` lists the session directories; the session names are in their `ROOT` files (grep them).
- **Import theories by session-qualified name** (`Box.Seq`, not `Seq`, and no absolute paths). A theory file in a session directory can be imported this way even if the `ROOT` file does not list it: `ROOT` only decides what a batch build processes.
- Reading theories that are not loaded with other tools (e.g. grepping the AFP) is fine; for loaded ones prefer `find_theorems` / `find_consts` / `find_entities`.

## When a batch build is the better tool
- **A batch build is never a silent fallback for a broken MCP:** if the MCP does not work, tell the user.
- For a mechanical change across many lines where few errors are expected, or for sessions the PIDE session is not based on (e.g. a test session built on the session you edit), `isabelle build` and grepping its `***` lines is simple and cheap.
- For proof debugging, goal states and checking downstream theories, the MCP is better: it rechecks incrementally in seconds.
- To see whether later theories are healthy while an early one is broken, put `sorry` in (or comment out) the broken part.
