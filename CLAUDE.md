# Notes for agents working in this repository

## Skills: sync the global copies after every skill commit

Agents started outside this repository load the global skills in `~/.claude/skills`, which are plain copies (no symbolic links) of `.claude/skills`.
After **every commit that changes the skills** (`.claude/skills`, `.agents/skills`, `.opencode/skills`), run `dev/sync_skills.sh` yourself right after
the commit (it backs up the old global copies under `~/.claude/skills/.backup-<time>/`; `--dry-run` shows what would change), and say in the last
line of your reply that the sync was run. The user authorised exactly this script for this purpose; do not use it for anything else and do not touch the
global skills in any other way.

## Hooks

`.claude/hooks/*.py` are registered in the user's global settings as copies in `~/.claude/hooks/`. When you change one of these scripts, say in your reply that
the global copy is stale; do not refresh it without the user's word. Install changes of a hook script atomically (temporary file, then rename) and keep it
from ever failing on bad input: a registered hook runs for every session of the user.

## Restarts

Changes under `src/`, `ml/` or `etc/` restart every MCP server on its next call (the dev proxy), which reloads big sessions and loses running ones: coordinate with the
other running sessions before merging such a change.
