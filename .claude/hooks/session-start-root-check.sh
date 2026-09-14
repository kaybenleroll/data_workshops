#!/usr/bin/env bash
# SessionStart hook: nudges toward launching Claude Code from the repo root.
#
# This repo is a monorepo of ~90 self-contained talk/workshop subprojects.
# Historical practice (and stated preference) is to launch Claude Code from
# the repo root so session transcripts/history stay unified across talks;
# CLAUDE.md's directory-scoping rule already prevents cross-talk
# contamination, so per-subdirectory launches aren't needed for isolation.
# This is advisory only — it never blocks the session from starting.
set -euo pipefail

ROOT="${CLAUDE_PROJECT_DIR:-}"
CWD="$(pwd)"

[[ -z "$ROOT" || "$CWD" == "$ROOT" ]] && exit 0

REL="${CWD#"$ROOT"/}"

case "$REL" in
  talk_*|ws_*|oldertalks/*)
    echo "Session launched from '$REL', not the repo root. Convention here is to launch from the repo root (unified transcript history; CLAUDE.md's directory-scoping rule already keeps subprojects isolated during work). Consider relaunching with: cd $ROOT && claude"
    ;;
esac
