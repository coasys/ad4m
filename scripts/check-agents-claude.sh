#!/usr/bin/env bash
# Every AGENTS.md needs a sibling CLAUDE.md whose content is exactly `@AGENTS.md`
# (one trailing newline allowed). Claude Code loads a folder's AGENTS.md only through it.
# Run from anywhere: bash scripts/check-agents-claude.sh

set -uo pipefail
cd "$(dirname "$0")/.." || exit 1

FAILED=0
while IFS= read -r agents; do
  claude="$(dirname "$agents")/CLAUDE.md"
  if [[ ! -f "$claude" ]]; then
    echo "missing: $claude (next to $agents)"
    FAILED=1
  elif [[ "$(cat "$claude")" != "@AGENTS.md" ]]; then
    echo "wrong content: $claude (must be exactly @AGENTS.md)"
    FAILED=1
  fi
done < <(find . \( -name node_modules -o -name target -o -name .git -o -name dist -o -name lib -o -name .hc-toolchain -o -name tst-tmp \) -prune -o -name AGENTS.md -type f -print | sort)

[[ $FAILED -eq 0 ]] && echo "ok: every AGENTS.md has a CLAUDE.md with @AGENTS.md"
exit $FAILED
