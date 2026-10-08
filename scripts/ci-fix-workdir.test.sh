#!/usr/bin/env bash
# Tests for the `fix_workdir` command in .circleci/config.yml (#1325). The step
# runs before `checkout`, so it cannot live in a script from the repository;
# this test extracts the exact `command: |` block from the config and runs it
# against scratch repositories: healthy ones that must be left alone, and ones
# broken the way the runners' workdirs break. No network, about two seconds.
# Run: bash scripts/ci-fix-workdir.test.sh

set -uo pipefail

CONFIG="$(cd "$(dirname "$0")/.." && pwd)/.circleci/config.yml"
FAILED=0

fail() {
    echo "  FAIL: $*"
    FAILED=1
}

# The command body: lines after `command: |` under `fix_workdir:`, up to the
# first non-blank line indented less than the body (the next step/command).
STEP="$(awk '
    /^  fix_workdir:/ { in_cmd = 1; next }
    in_cmd && !in_body && /^ *command: \|$/ { in_body = 1; next }
    in_body {
        if ($0 ~ /^[[:space:]]*$/) { print ""; next }
        match($0, /^ */)
        if (!indent) indent = RLENGTH
        if (RLENGTH < indent) exit
        print substr($0, indent + 1)
    }
' "$CONFIG")"
if [ -z "$STEP" ] || ! grep -q 'git fsck --connectivity-only' <<<"$STEP"; then
    echo "FAIL: could not extract the fix_workdir command from $CONFIG"
    exit 1
fi

# Runs the step the way CircleCI does (bash -eo pipefail) in $1, capturing output.
run_step() {
    (cd "$1" && CIRCLE_REPOSITORY_URL="https://example.invalid/coasys/ad4m.git" \
        bash -eo pipefail -c "$STEP" >"$1.log" 2>&1)
    STEP_RC=$?
    STEP_LOG="$(cat "$1.log")"
}

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT
git() { command git -c user.name=t -c user.email=t@t -c init.defaultBranch=main "$@"; }

# Stand-in for GitHub: four commits that each change f, so a blob:none clone
# of it really lacks three blobs.
git init -q "$TMP/origin"
for i in 1 2 3 4; do
    echo "v$i" > "$TMP/origin/f"
    git -C "$TMP/origin" add f
    git -C "$TMP/origin" commit -qm "c$i"
done
git -C "$TMP/origin" config uploadpack.allowFilter true
ORIGIN_HEAD="$(git -C "$TMP/origin" rev-parse HEAD)"

# A clone the way a previous job leaves it: on a branch, with stale branches
# from earlier jobs, an untracked cache dir and a modified tracked file.
make_workdir() {
    rm -rf "$TMP/w"
    git clone -q "$@" "file://$TMP/origin" "$TMP/w"
    git -C "$TMP/w" checkout -q -b "fix/some-pr" HEAD
    git -C "$TMP/w" branch -q "stale/one" HEAD~1
    git -C "$TMP/w" branch -q "stale/two" HEAD~2
    git -C "$TMP/w" update-ref refs/remotes/origin/other "$ORIGIN_HEAD"
    mkdir -p "$TMP/w/target/release"
    echo cached > "$TMP/w/target/release/ad4m"
    echo dirty >> "$TMP/w/f"
}
objects_of() { git -C "$1" cat-file --batch-all-objects --batch-check='%(objectname)' | sort; }

echo "healthy clone: objects and files kept, branches dropped, HEAD detached"
make_workdir
before="$(objects_of "$TMP/w")"
run_step "$TMP/w"
[ "$STEP_RC" = 0 ] || fail "step exited $STEP_RC: $STEP_LOG"
[ -d "$TMP/w/.git" ] || fail ".git was removed from a healthy clone"
[ "$(objects_of "$TMP/w")" = "$before" ] || fail "objects changed in a healthy clone"
[ "$(git -C "$TMP/w" rev-parse HEAD)" = "$ORIGIN_HEAD" ] || fail "HEAD moved"
git -C "$TMP/w" symbolic-ref -q HEAD >/dev/null && fail "HEAD is still symbolic (not detached)"
[ -z "$(git -C "$TMP/w" for-each-ref refs/heads)" ] || fail "local branches left: $(git -C "$TMP/w" for-each-ref refs/heads)"
[ "$(git -C "$TMP/w" for-each-ref refs/remotes | wc -l)" = 3 ] || fail "remote-tracking refs touched"
[ "$(cat "$TMP/w/target/release/ad4m")" = cached ] || fail "untracked cache touched"
[ "$(tail -1 "$TMP/w/f")" = dirty ] || fail "working tree touched"
grep -q "removed 4 stale local branch ref(s)" <<<"$STEP_LOG" || fail "no branch-removal log line: $STEP_LOG"
grep -q "removing .git" <<<"$STEP_LOG" && fail "healthy clone logged a heal: $STEP_LOG"

echo "healthy clone run twice: second run is a no-op"
run_step "$TMP/w"
[ "$STEP_RC" = 0 ] || fail "second run exited $STEP_RC"
[ -z "$STEP_LOG" ] || fail "second run (no branches, detached) printed: $STEP_LOG"
# The deleted branch name can be created again, as checkout will do.
git -C "$TMP/w" checkout -q -b "fix/some-pr" HEAD || fail "cannot recreate the branch after the step"

echo "healthy blob:none partial clone: left alone, missing blobs accepted"
make_workdir --filter=blob:none
mv "$TMP/origin" "$TMP/origin.away"   # no lazy fetch can paper over the missing blobs
[ "$(git -C "$TMP/w" rev-list --objects --all --missing=print | grep -c '^?')" = 3 ] || fail "fixture is not a partial clone"
run_step "$TMP/w"
mv "$TMP/origin.away" "$TMP/origin"
[ "$STEP_RC" = 0 ] || fail "step exited $STEP_RC: $STEP_LOG"
[ -d "$TMP/w/.git" ] || fail ".git was removed from a healthy partial clone"
grep -q "removing .git" <<<"$STEP_LOG" && fail "healthy partial clone logged a heal: $STEP_LOG"

echo "partial clone without its .promisor markers: healed, state logged"
make_workdir --filter=blob:none
rm "$TMP/w"/.git/objects/pack/*.promisor
mv "$TMP/origin" "$TMP/origin.away"
run_step "$TMP/w"
mv "$TMP/origin.away" "$TMP/origin"
[ "$STEP_RC" = 0 ] || fail "step exited $STEP_RC: $STEP_LOG"
grep -q "fsck --connectivity-only failed" <<<"$STEP_LOG" || fail "fsck failure not logged: $STEP_LOG"
grep -q "broken link from" <<<"$STEP_LOG" || fail "fsck output not logged: $STEP_LOG"
grep -q "remote.origin.promisor=true" <<<"$STEP_LOG" || fail "partial-clone config not logged: $STEP_LOG"
grep -q "promisor packs=0" <<<"$STEP_LOG" || fail "promisor pack count not logged: $STEP_LOG"
grep -q "missing objects reachable from refs -- removing .git" <<<"$STEP_LOG" || fail "heal not logged: $STEP_LOG"

echo "clone missing a loose object: healed, empty repo initialised over the files"
make_workdir
echo local > "$TMP/w/g"; git -C "$TMP/w" add g; git -C "$TMP/w" commit -qm loose   # loose objects to delete
blob="$(git -C "$TMP/w" rev-parse HEAD:g)"
rm "$TMP/w/.git/objects/${blob:0:2}/${blob:2}"
run_step "$TMP/w"
[ "$STEP_RC" = 0 ] || fail "step exited $STEP_RC: $STEP_LOG"
grep -q "missing blob $blob" <<<"$STEP_LOG" || fail "fsck output not logged: $STEP_LOG"
grep -q "promisor packs=0" <<<"$STEP_LOG" || fail "partial-clone state not logged: $STEP_LOG"
grep -q "missing objects reachable from refs -- removing .git" <<<"$STEP_LOG" || fail "heal not logged: $STEP_LOG"
grep -q "initialising an empty repository" <<<"$STEP_LOG" || fail "no init log line: $STEP_LOG"
[ -d "$TMP/w/.git" ] || fail "no repository initialised"
[ -z "$(git -C "$TMP/w" for-each-ref)" ] || fail "initialised repo is not empty"
[ "$(git -C "$TMP/w" remote get-url origin)" = "https://example.invalid/coasys/ad4m.git" ] || fail "origin not set from CIRCLE_REPOSITORY_URL"
[ "$(cat "$TMP/w/target/release/ad4m")" = cached ] || fail "untracked cache lost"
[ -f "$TMP/w/f" ] || fail "checked-out files lost"

echo "clone whose pack is gone (refs point nowhere): healed"
make_workdir
rm "$TMP/w"/.git/objects/pack/*.pack
run_step "$TMP/w"
[ "$STEP_RC" = 0 ] || fail "step exited $STEP_RC: $STEP_LOG"
grep -q "invalid sha1 pointer" <<<"$STEP_LOG" || fail "fsck output not logged: $STEP_LOG"
grep -q "missing objects reachable from refs -- removing .git" <<<"$STEP_LOG" || fail "heal not logged: $STEP_LOG"
grep -q "initialising an empty repository" <<<"$STEP_LOG" || fail "no init log line: $STEP_LOG"

echo "corrupted .git (git cannot open it): healed by the first check"
make_workdir
echo garbage > "$TMP/w/.git/HEAD"
run_step "$TMP/w"
[ "$STEP_RC" = 0 ] || fail "step exited $STEP_RC: $STEP_LOG"
grep -q "corrupted .git (git rev-parse fails) -- removing .git" <<<"$STEP_LOG" || fail "heal not logged: $STEP_LOG"
grep -q "fsck" <<<"$STEP_LOG" && fail "fsck ran on a .git that does not open: $STEP_LOG"
grep -q "initialising an empty repository" <<<"$STEP_LOG" || fail "no init log line: $STEP_LOG"

echo "files but no .git (after an earlier heal): empty repo initialised"
rm -rf "$TMP/w"; mkdir -p "$TMP/w/target"; echo cached > "$TMP/w/target/x"
run_step "$TMP/w"
[ "$STEP_RC" = 0 ] || fail "step exited $STEP_RC: $STEP_LOG"
[ -d "$TMP/w/.git" ] || fail "no repository initialised"
grep -q "initialising an empty repository" <<<"$STEP_LOG" || fail "no init log line: $STEP_LOG"

echo "empty directory (fresh runner): nothing happens"
rm -rf "$TMP/w"; mkdir "$TMP/w"
run_step "$TMP/w"
[ "$STEP_RC" = 0 ] || fail "step exited $STEP_RC: $STEP_LOG"
[ ! -e "$TMP/w/.git" ] || fail "a repository was initialised in an empty directory"
[ -z "$STEP_LOG" ] || fail "empty directory printed: $STEP_LOG"

echo "unborn HEAD (empty repo from a previous heal, checkout never ran): no-op"
rm -rf "$TMP/w"; mkdir "$TMP/w"; git -C "$TMP/w" init -q; echo x > "$TMP/w/x"
run_step "$TMP/w"
[ "$STEP_RC" = 0 ] || fail "step exited $STEP_RC: $STEP_LOG"
[ -d "$TMP/w/.git" ] || fail ".git removed from an empty repository"
[ -z "$STEP_LOG" ] || fail "unborn HEAD printed: $STEP_LOG"

if [ "$FAILED" = 0 ]; then
    echo "All fix_workdir tests passed"
else
    echo "Some fix_workdir tests FAILED"
    exit 1
fi
