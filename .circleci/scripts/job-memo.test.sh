#!/usr/bin/env bash
# Tests for .circleci/scripts/job-memo.sh. Scratch git repos, no network. About a second.
# Run: bash .circleci/scripts/job-memo.test.sh

set -uo pipefail

SCRIPT="$(cd "$(dirname "$0")" && pwd)/job-memo.sh"
FAILED=0
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

REPO="$TMP/repo"
MEMO="$TMP/memo"
G=(git -c user.name=t -c user.email=t@example.org -c commit.gpgsign=false)

mkdir -p "$REPO"
cd "$REPO" || exit 1
git init -q .
mkdir -p rust-executor/src docs tests/js .circleci docs-src/identity
echo 'fn main() {}' > rust-executor/src/lib.rs
echo 'page' > docs/index.html
echo 'it()' > tests/js/a.test.ts
echo 'jobs' > .circleci/config.yml
echo 'spec' > docs-src/identity/spec.md
"${G[@]}" add -A
"${G[@]}" commit -qm "feat: first"

# memo <mode> <job> [VAR=value ...]: runs the script in a clean environment.
memo() {
  local mode="$1" job="$2"
  shift 2
  env -i PATH="$PATH" HOME="$TMP" JOB_MEMO_DIR="$MEMO" CIRCLE_BUILD_URL="https://ci.example/job/1" \
    "$@" bash "$SCRIPT" "$mode" "$job"
}

# check <name> <expected exit> <expected output pattern> <mode> <job> [VAR=value ...]
check() {
  local name="$1" want_rc="$2" want_out="$3"
  shift 3
  local out rc=0
  out="$(memo "$@" 2>&1)" || rc=$?
  if [ "$rc" != "$want_rc" ] || ! grep -qE "$want_out" <<<"$out"; then
    echo "  FAIL: $name: exit $rc (want $want_rc), output: $out"
    FAILED=1
  else
    echo "  ok: $name -> $(head -1 <<<"$out")"
  fi
}

# change <path> <text> <message>: a new commit that changes one file.
change() {
  mkdir -p "$(dirname "$1")"
  echo "$2" > "$1"
  "${G[@]}" add -A
  "${G[@]}" commit -qm "$3"
}

echo "job-memo.sh"

check "no record yet runs" 0 '^run: no pass on record' check rust-tests
check "record stores the pass" 0 '^recorded: rust-tests' record rust-tests
check "same inputs skip, with the job that passed" 10 '^skip: rust-tests passed on the same inputs: https://ci.example/job/1 commit ' check rust-tests
check "another job has its own record" 0 '^run: no pass on record' check clippy

change docs/index.html 'page 2' "docs: page"
check "a change under docs/ still skips" 10 '^skip: ' check rust-tests
change tests/js/b.test.ts 'it()' "test: js"
check "a new file under tests/js/ still skips" 10 '^skip: ' check rust-tests

check "[full-ci] in the commit message runs" 0 '^run: \[full-ci\]' check rust-tests JOB_MEMO_COMMIT_MSG="fix: x [full-ci]"
check "JOB_MEMO_FORCE=true runs" 0 '^run: JOB_MEMO_FORCE' check rust-tests JOB_MEMO_FORCE=true
check "JOB_MEMO_FORCE=false does not force" 10 '^skip: ' check rust-tests JOB_MEMO_FORCE=false

change docs-src/identity/spec.md 'spec 2' "docs: spec the identity tests embed"
check "a change under docs-src/ runs (not on the list)" 0 '^run: no pass on record' check rust-tests
memo record rust-tests >/dev/null
change rust-executor/src/lib.rs 'fn main() { }' "fix: rust"
check "a Rust change runs" 0 '^run: no pass on record' check rust-tests
memo record rust-tests >/dev/null
change .circleci/config.yml 'jobs 2' "ci: config"
check "a CI config change runs" 0 '^run: no pass on record' check rust-tests
memo record rust-tests >/dev/null
change brand-new-dir/file 'x' "feat: new directory"
check "a path on no list runs" 0 '^run: no pass on record' check rust-tests
memo record rust-tests >/dev/null
change docsX/file 'x' "feat: a directory whose name starts like an ignored one"
check "docsX/ is not docs/" 0 '^run: no pass on record' check rust-tests

# A file mode change is a change (ls-tree prints the mode).
memo record rust-tests >/dev/null
chmod +x rust-executor/src/lib.rs
"${G[@]}" add -A
"${G[@]}" commit -qm "chore: mode"
check "a mode change runs" 0 '^run: no pass on record' check rust-tests

# A revert brings the old tree back: the pass recorded for it counts again.
memo record rust-tests >/dev/null
change rust-executor/src/lib.rs 'fn other() {}' "fix: again"
check "the commit to revert runs" 0 '^run: no pass on record' check rust-tests
"${G[@]}" revert --no-edit HEAD >/dev/null
check "its revert skips" 10 '^skip: ' check rust-tests

check "an unknown job runs" 0 '^run: no key for job nope' check nope
check "an unknown job records nothing" 0 '^not recorded: no key' record nope
check "a missing mode is a usage error" 2 'usage' "" rust-tests

# Fails open: a memo directory that cannot be made, and a directory that is not a repo.
check "an unwritable memo dir records nothing, exit 0" 0 '^not recorded: ' record rust-tests JOB_MEMO_DIR=/proc/nope/memo
check "an unreadable memo dir runs" 0 '^run: no pass on record' check rust-tests JOB_MEMO_DIR=/proc/nope/memo
mkdir -p "$TMP/norepo"
out="$(cd "$TMP/norepo" && env -i PATH="$PATH" HOME="$TMP" JOB_MEMO_DIR="$MEMO" GIT_CEILING_DIRECTORIES="$TMP" bash "$SCRIPT" check rust-tests 2>/dev/null)"
rc=$?
if [ "$rc" = 0 ] && grep -q '^run: no key' <<<"$out"; then echo "  ok: outside a git repo runs"; else echo "  FAIL: outside a repo: exit $rc, $out"; FAILED=1; fi

# An empty entry (a write cut short) is no record.
k="$(memo key clippy)"
: > "$MEMO/clippy--$k"
check "an empty entry runs" 0 '^run: no pass on record' check clippy

# Entries past the age limit go when the next pass is recorded.
touch -d '20 days ago' "$MEMO/rust-tests--old"
memo record clippy >/dev/null
if [ -e "$MEMO/rust-tests--old" ]; then echo "  FAIL: old entry kept"; FAILED=1; else echo "  ok: an entry older than 14 days is deleted"; fi

if [ "$FAILED" = 0 ]; then echo "all passed"; else echo "FAILED"; exit 1; fi
