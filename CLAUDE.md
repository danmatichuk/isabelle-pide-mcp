# Notes for agents working in this repository

## Skills: do not sync the global copies

Agents started outside this repository load the global skills in `~/.claude/skills`, which are plain copies (no symbolic links) of `.claude/skills`.
Do not run `dev/sync_skills.sh` and do not touch the global skills in any other way, also not after a commit that changes the skills: experiment
harnesses take the skills from each commit, and the global copies stay as the user set them. Run the script only when the user asks for it.

## Hooks

`.claude/hooks/*.py` are registered in the user's global settings as copies in `~/.claude/hooks/`. When you change one of these scripts, say in your reply that
the global copy is stale; do not refresh it without the user's word. Install changes of a hook script atomically (temporary file, then rename) and keep it
from ever failing on bad input: a registered hook runs for every session of the user.

## Restarts

Changes under `src/`, `ml/` or `etc/` restart every MCP server on its next call (the dev proxy), which reloads big sessions and loses running ones: coordinate with the
other running sessions before merging such a change.
