#!/usr/bin/env bash
# Tests for .circleci/scripts/paths-changed.sh. Scratch git repos (a bare one is
# `origin`), no network. About a second.
# Run: bash .circleci/scripts/paths-changed.test.sh

set -uo pipefail

SCRIPT="$(cd "$(dirname "$0")" && pwd)/paths-changed.sh"
FAILED=0
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

G=(git -c user.name=t -c user.email=t@example.org -c commit.gpgsign=false -c init.defaultBranch=dev)
PATHS=(rust-executor/src/sfu/ 'tests/js/tests/sfu*' .circleci/)

# origin: a bare repository with dev. repo: a clone, on a PR branch.
"${G[@]}" init -q --bare "$TMP/origin.git"
"${G[@]}" clone -q "$TMP/origin.git" "$TMP/repo" 2>/dev/null
cd "$TMP/repo" || exit 1
mkdir -p rust-executor/src/sfu rust-executor/src/db tests/js/tests docs .circleci
echo 'mod room;' > rust-executor/src/sfu/mod.rs
echo 'fn db() {}' > rust-executor/src/db/mod.rs
echo 'it()' > tests/js/tests/sfu-e2e.test.ts
echo 'it()' > tests/js/tests/agent.test.ts
echo 'jobs' > .circleci/config.yml
echo 'page' > docs/index.md
"${G[@]}" add -A
"${G[@]}" commit -qm "feat: first"
"${G[@]}" push -q origin dev 2>/dev/null

# change <path> <text> <message>: a new commit that changes one file.
change() {
  mkdir -p "$(dirname "$1")"
  echo "$2" >> "$1"
  "${G[@]}" add -A
  "${G[@]}" commit -qm "$3"
}

# check <name> <expected exit> <expected output pattern> [VAR=value ...]
# Runs the script with the job's paths in a clean environment.
check() {
  local name="$1" want_rc="$2" want_out="$3"
  shift 3
  local out rc=0
  out="$(env -i PATH="$PATH" HOME="$TMP" PATHS_GATE_COMMIT_MSG="feat: something" \
    "$@" bash "$SCRIPT" "${PATHS[@]}" 2>&1)" || rc=$?
  if [ "$rc" != "$want_rc" ] || ! grep -qE "$want_out" <<<"$out"; then
    echo "  FAIL: $name: exit $rc (want $want_rc), output: $out"
    FAILED=1
  else
    echo "  ok: $name -> $(head -1 <<<"$out")"
  fi
}

echo "paths-changed.sh"

# Branches and overrides, before any diff is read.
check "push to dev skips: the 2-hourly tier runs it" 10 '^skip: push to dev' CIRCLE_BRANCH=dev
check "push to staging runs" 0 '^run: push to staging' CIRCLE_BRANCH=staging
check "push to main runs" 0 '^run: push to main' CIRCLE_BRANCH=main
check "tag runs" 0 '^run: tag v1' CIRCLE_TAG=v1
check "no branch runs" 0 '^run: no CIRCLE_BRANCH'
check "PATHS_GATE_FORCE runs" 0 '^run: PATHS_GATE_FORCE' CIRCLE_BRANCH=feat/x PATHS_GATE_FORCE=true
check "[full-ci] in the commit message runs" 0 '^run: \[full-ci\]' \
  CIRCLE_BRANCH=feat/x PATHS_GATE_COMMIT_MSG=$'fix: y\n\n[full-ci]'
out="$(env -i PATH="$PATH" HOME="$TMP" CIRCLE_BRANCH=feat/x bash "$SCRIPT" 2>&1)"; rc=$?
if [ "$rc" = 0 ] && grep -q '^run: no paths given' <<<"$out"; then echo "  ok: no paths -> $out"
else echo "  FAIL: no paths: exit $rc, output: $out"; FAILED=1; fi

# A PR branch off dev.
"${G[@]}" checkout -q -b feat/x
change docs/index.md "more" "docs: more"
change rust-executor/src/db/mod.rs "fn more() {}" "feat: db"
check "PR that touches no job path skips" 10 '^skip: no file under' CIRCLE_BRANCH=feat/x
check "skip names the base it compared with" 10 'origin/dev' CIRCLE_BRANCH=feat/x
change tests/js/tests/agent.test.ts "it(2)" "test: agent"
check "a test file outside the glob still skips" 10 '^skip:' CIRCLE_BRANCH=feat/x

change rust-executor/src/sfu/mod.rs "mod relay;" "feat: sfu relay"
check "PR that touches SFU code runs" 0 '^run: 1 changed file.*rust-executor/src/sfu/mod.rs' CIRCLE_BRANCH=feat/x

# Only the PR's own changes count: dev moves on (SFU change on dev), the PR merges
# dev in, and the PR itself touches no job path.
"${G[@]}" checkout -q dev
"${G[@]}" checkout -q -b feat/y
change docs/index.md "y" "docs: y"
"${G[@]}" checkout -q dev
change rust-executor/src/sfu/mod.rs "mod cascade;" "feat: sfu cascade on dev"
"${G[@]}" push -q origin dev 2>/dev/null
"${G[@]}" checkout -q feat/y
"${G[@]}" merge -q --no-edit dev 2>/dev/null
check "merge of dev that brings SFU changes, PR touches none: skips" 10 '^skip:' CIRCLE_BRANCH=feat/y

change tests/js/tests/sfu-session.test.ts "it()" "test: sfu session"
check "glob path (tests/js/tests/sfu*) runs" 0 '^run: 1 changed file.*sfu-session' CIRCLE_BRANCH=feat/y
"${G[@]}" reset -q --hard HEAD~1
change .circleci/config.yml "more jobs" "ci: config"
check "CI config change runs" 0 '^run: .*\.circleci/config.yml' CIRCLE_BRANCH=feat/y

# The script fetches dev itself: a stale local origin/dev must not count dev's
# own changes as the PR's.
"${G[@]}" checkout -q dev
change docs/index.md "z" "docs: z"
"${G[@]}" checkout -q -b feat/z
change docs/index.md "z2" "docs: z2"
"${G[@]}" checkout -q dev
change rust-executor/src/sfu/mod.rs "mod pipe;" "feat: sfu pipe on dev"
"${G[@]}" push -q origin dev 2>/dev/null
"${G[@]}" update-ref refs/remotes/origin/dev HEAD~2
"${G[@]}" checkout -q feat/z
"${G[@]}" merge -q --no-edit dev 2>/dev/null
check "fetches dev before the diff" 10 '^skip:' CIRCLE_BRANCH=feat/z

# Fails open.
check "fetch failure runs" 0 '^run: could not fetch' CIRCLE_BRANCH=feat/z PATHS_GATE_REMOTE=nowhere
check "unknown base runs" 0 '^run: could not fetch' CIRCLE_BRANCH=feat/z PATHS_GATE_BASE=no-such-branch
"${G[@]}" checkout -q --orphan unrelated
"${G[@]}" rm -rqf . >/dev/null
echo x > x
"${G[@]}" add x
"${G[@]}" commit -qm "unrelated"
check "no merge base runs" 0 '^run: no merge base' CIRCLE_BRANCH=unrelated
out="$(cd "$TMP" && env -i PATH="$PATH" HOME="$TMP" CIRCLE_BRANCH=feat/x PATHS_GATE_COMMIT_MSG=x bash "$SCRIPT" docs/ 2>&1)"; rc=$?
if [ "$rc" = 0 ] && grep -q '^run: ' <<<"$out"; then echo "  ok: outside a git repository runs -> $out"
else echo "  FAIL: outside a git repository: exit $rc, output: $out"; FAILED=1; fi

if [ "$FAILED" != 0 ]; then
  echo "FAILED"
  exit 1
fi
echo "all passed"
