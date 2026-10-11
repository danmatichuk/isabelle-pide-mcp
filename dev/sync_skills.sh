#!/usr/bin/env bash
# Install this repository's agent skills into the global skill folders of the coding agents (what README.md describes as
# "copy the folders"): agents started OUTSIDE this repository (e.g. a tester in another directory) load the global copies,
# not the ones under .claude/skills of this repository, so edits here do not reach them until this is run.
#
# Usage: dev/sync_skills.sh [--dry-run]
# Existing global copies are saved to <target>/.backup-<timestamp>/ first.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
stamp="$(date +%Y%m%d-%H%M%S)"
dry=""; [ "${1:-}" = "--dry-run" ] && dry=1
install() {  # source skills dir, target skills dir
  local src="$1" dst="$2"
  [ -d "$src" ] || return 0
  for skill in "$src"/*/; do
    name="$(basename "$skill")"
    if [ -d "$dst/$name" ] && ! diff -rq "$skill" "$dst/$name" >/dev/null 2>&1; then
      echo "update: $dst/$name"
      if [ -z "$dry" ]; then mkdir -p "$dst/.backup-$stamp" && cp -a "$dst/$name" "$dst/.backup-$stamp/"; fi
    elif [ ! -d "$dst/$name" ]; then
      echo "new:    $dst/$name"
    else
      echo "same:   $dst/$name"; continue
    fi
    if [ -z "$dry" ]; then mkdir -p "$dst/$name" && cp -a "$skill"/. "$dst/$name"/; fi
  done
}
install "$here/.claude/skills" "$HOME/.claude/skills"
# other agents (uncomment what you use): 
# install "$here/.agents/skills" "$HOME/.agents/skills"
# install "$here/.opencode/skills" "$HOME/.config/opencode/skills"
