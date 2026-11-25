#!/usr/bin/env bash
# Tests for .circleci/scripts/should-run-heavy.sh. A fake `curl` on PATH serves
# fixture JSON (or fails), so they need no network. About a second.
# Run: bash .circleci/scripts/should-run-heavy.test.sh

set -uo pipefail

SCRIPT="$(cd "$(dirname "$0")" && pwd)/should-run-heavy.sh"
FAILED=0
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Fake curl: records each URL it is asked for, then prints $FAKE_BODY or fails
# like `curl -f` on an HTTP error when FAKE_FAIL is set.
mkdir -p "$TMP/bin"
cat > "$TMP/bin/curl" <<'EOF'
#!/usr/bin/env bash
for a in "$@"; do case "$a" in http*) echo "$a" >> "$FAKE_LOG" ;; esac; done
[ -n "${FAKE_FAIL:-}" ] && exit 22
printf '%s' "$FAKE_BODY"
EOF
chmod +x "$TMP/bin/curl"

pr() { # number draft base [label]
  local labels='[]'
  [ -n "${4:-}" ] && labels="[{\"name\":\"$4\"}]"
  printf '{"number":%s,"state":"open","draft":%s,"base":{"ref":"%s"},"labels":%s}' "$1" "$2" "$3" "$labels"
}

# check <name> <expected exit> <expected output pattern> [VAR=value ...]
# Runs the script in a clean environment with the given variables.
check() {
  local name="$1" want_rc="$2" want_out="$3"
  shift 3
  : > "$TMP/curl.log"
  local out rc=0
  out="$(env -i PATH="$TMP/bin:$PATH" HOME="$TMP" FAKE_LOG="$TMP/curl.log" \
    HEAVY_GATE_COMMIT_MSG="feat: something" HEAVY_GATE_CACHE_DIR="$TMP/cache" \
    "$@" bash "$SCRIPT" ${ARGS:-})" || rc=$?
  if [ "$rc" != "$want_rc" ] || ! grep -qE "$want_out" <<<"$out"; then
    echo "  FAIL: $name: exit $rc (want $want_rc), output: $out"
    FAILED=1
  else
    echo "  ok: $name -> $(head -1 <<<"$out")"
  fi
}

echo "should-run-heavy.sh"
light() { ARGS=light check "$@"; } # the question every other job asks: does CI run at all?

check "push to dev runs, no API call" 0 '^run: push to dev' CIRCLE_BRANCH=dev FAKE_FAIL=1
[ -s "$TMP/curl.log" ] && { echo "  FAIL: push to dev called the API"; FAILED=1; }
check "push to staging runs" 0 '^run: push to staging' CIRCLE_BRANCH=staging
check "push to main runs" 0 '^run: push to main' CIRCLE_BRANCH=main
check "tag runs" 0 '^run: tag v1' CIRCLE_TAG=v1 CIRCLE_BRANCH=
check "nightly (forced) runs on any branch" 0 '^run: HEAVY_GATE_FORCE' \
  CIRCLE_BRANCH=feat/x HEAVY_GATE_FORCE=true FAKE_BODY='[]'
check "no branch runs" 0 '^run: no CIRCLE_BRANCH'

check "PR to dev runs" 0 '^run: PR #12 targets dev' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 12 false dev)]"
grep -q 'repos/coasys/ad4m/pulls?state=open&per_page=100&head=coasys%3Afeat%2Fx$' "$TMP/curl.log" \
  || { echo "  FAIL: wrong API URL: $(cat "$TMP/curl.log")"; FAILED=1; }
check "draft PR to dev skips" 10 '^skip: PR #13 is a draft' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 13 true dev)]"
check "PR to a stack branch skips" 10 '^skip: PR #14 targets feat/base, not dev' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 14 false feat/base)]"
check "branch with no PR skips" 10 '^skip: no open PR' CIRCLE_BRANCH=spike/x FAKE_BODY='[]'
check "[full-ci] in the commit message runs, no API call" 0 '^run: \[full-ci\]' \
  CIRCLE_BRANCH=feat/x HEAVY_GATE_COMMIT_MSG=$'fix: y\n\n[full-ci]' FAKE_FAIL=1
[ -s "$TMP/curl.log" ] && { echo "  FAIL: [full-ci] called the API"; FAILED=1; }
check "full-ci label on a stacked draft runs" 0 '^run: PR #15 has the full-ci label' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 15 true feat/base full-ci)]"
check "two PRs from one branch: any to dev runs" 0 '^run: PR #17 targets dev' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 16 false feat/base),$(pr 17 false dev)]"
check "fork PR branch pull/N asks for that PR" 10 '^skip: PR #18 is a draft' \
  CIRCLE_BRANCH=pull/18 FAKE_BODY="$(pr 18 true dev)"
grep -q 'repos/coasys/ad4m/pulls/18$' "$TMP/curl.log" || { echo "  FAIL: fork URL: $(cat "$TMP/curl.log")"; FAILED=1; }
check "repo from CIRCLE_REPOSITORY_URL" 0 '^run:' CIRCLE_BRANCH=feat/x \
  CIRCLE_REPOSITORY_URL=git@github.com:someone/fork.git FAKE_BODY="[$(pr 19 false dev)]"
grep -q 'repos/someone/fork/pulls?.*head=someone%3Afeat%2Fx$' "$TMP/curl.log" \
  || { echo "  FAIL: repo URL: $(cat "$TMP/curl.log")"; FAILED=1; }

# Fails open: runs, with exit 11 so a caller can tell "run because the PR asks for it"
# from "run because the lookup failed".
check "API failure runs (exit 11: unsure)" 11 '^run: GitHub API request failed' CIRCLE_BRANCH=feat/x FAKE_FAIL=1
check "API rate-limit body (an object, no number) runs" 11 '^run: unexpected GitHub API answer' \
  CIRCLE_BRANCH=feat/x FAKE_BODY='{"message":"API rate limit exceeded"}'
check "API garbage runs" 11 '^run: could not read' CIRCLE_BRANCH=feat/x FAKE_BODY='<html>'
mkdir -p "$TMP/nojq"; for t in bash env sed cat tr mkdir find git; do ln -sf "$(command -v $t)" "$TMP/nojq/$t" 2>/dev/null; done
ln -sf "$TMP/bin/curl" "$TMP/nojq/curl"
out_nojq="$(env -i PATH="$TMP/nojq" HOME="$TMP" FAKE_LOG="$TMP/curl.log" HEAVY_GATE_COMMIT_MSG=x \
  CIRCLE_BRANCH=feat/x "$TMP/nojq/bash" "$SCRIPT")"; rc_nojq=$?
if [ "$rc_nojq" = 11 ] && grep -q '^run: jq not installed' <<<"$out_nojq"; then
  echo "  ok: jq missing runs, exit 11 -> $out_nojq"
else
  echo "  FAIL: jq missing: exit $rc_nojq (want 11), output: $out_nojq"; FAILED=1
fi

# One request per workflow: the second job reads the first one's answer; a failed
# lookup is not kept, and another workflow (a rerun) asks again.
rm -rf "$TMP/cache"
check "workflow cache: first job asks" 10 '^skip: PR #20 is a draft$' \
  CIRCLE_BRANCH=feat/x CIRCLE_WORKFLOW_ID=wf1 FAKE_BODY="[$(pr 20 true dev)]"
check "workflow cache: second job reuses, no API call" 10 'cached for this workflow' \
  CIRCLE_BRANCH=feat/x CIRCLE_WORKFLOW_ID=wf1 FAKE_FAIL=1
[ -s "$TMP/curl.log" ] && { echo "  FAIL: cached answer still called the API"; FAILED=1; }
check "workflow cache: a rerun asks again" 0 '^run: PR #20 targets dev$' \
  CIRCLE_BRANCH=feat/x CIRCLE_WORKFLOW_ID=wf2 FAKE_BODY="[$(pr 20 false dev)]"
check "workflow cache: a failure is not kept" 11 '^run: GitHub API request failed' \
  CIRCLE_BRANCH=feat/y CIRCLE_WORKFLOW_ID=wf3 FAKE_FAIL=1
check "workflow cache: next job after a failure asks again" 10 '^skip: no open PR for this branch$' \
  CIRCLE_BRANCH=feat/y CIRCLE_WORKFLOW_ID=wf3 FAKE_BODY='[]'

# `light`: the question of the jobs that run on every PR (build, fmt, clippy, ...).
# Only a draft halts them; a stacked PR or a branch with no PR still runs them.
echo "should-run-heavy.sh light"
light "light: draft PR to dev skips" 10 '^skip: PR #30 is a draft' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 30 true dev)]"
light "light: draft PR to a stack branch skips" 10 '^skip: PR #31 is a draft' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 31 true feat/base)]"
light "light: ready PR to dev runs" 0 '^run: PR #32 is ready' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 32 false dev)]"
light "light: ready PR to a stack branch runs" 0 '^run: PR #33 is ready' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 33 false feat/base)]"
light "light: branch with no PR runs" 0 '^run: no open PR' CIRCLE_BRANCH=spike/x FAKE_BODY='[]'
light "light: full-ci label on a draft runs" 0 '^run: PR #34 has the full-ci label' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 34 true dev full-ci)]"
light "light: [full-ci] in the commit message on a draft runs, no API call" 0 '^run: \[full-ci\]' \
  CIRCLE_BRANCH=feat/x HEAVY_GATE_COMMIT_MSG=$'fix: y\n\n[full-ci]' FAKE_FAIL=1
light "light: two PRs from one branch, one ready: runs" 0 '^run: PR #36 is ready' \
  CIRCLE_BRANCH=feat/x FAKE_BODY="[$(pr 35 true dev),$(pr 36 false feat/base)]"
light "light: dev, nightly and tags run, no API call" 0 '^run: push to dev' CIRCLE_BRANCH=dev FAKE_FAIL=1
light "light: nightly (forced) runs" 0 '^run: HEAVY_GATE_FORCE' CIRCLE_BRANCH=feat/x HEAVY_GATE_FORCE=true FAKE_FAIL=1
light "light: API failure runs (exit 11: unsure)" 11 '^run: GitHub API request failed' CIRCLE_BRANCH=feat/x FAKE_FAIL=1
light "light: API garbage runs" 11 '^run: could not read' CIRCLE_BRANCH=feat/x FAKE_BODY='<html>'

# One lookup answers both questions: the heavy jobs and the light jobs of a workflow
# share the cache, whichever asks first.
rm -rf "$TMP/cache"
light "light cache: first job asks" 10 '^skip: PR #40 is a draft$' \
  CIRCLE_BRANCH=feat/x CIRCLE_WORKFLOW_ID=wfl1 FAKE_BODY="[$(pr 40 true dev)]"
check "light cache: a heavy job reuses it, no API call" 10 'cached for this workflow' \
  CIRCLE_BRANCH=feat/x CIRCLE_WORKFLOW_ID=wfl1 FAKE_FAIL=1
[ -s "$TMP/curl.log" ] && { echo "  FAIL: heavy job after a light one called the API"; FAILED=1; }
light "light cache: a stacked ready PR: heavy skips, light runs, one request" 0 '^run: PR #41 is ready' \
  CIRCLE_BRANCH=feat/z CIRCLE_WORKFLOW_ID=wfl2 FAKE_BODY="[$(pr 41 false feat/base)]"
check "light cache: the heavy job of that workflow skips from the cache" 10 '^skip: PR #41 targets feat/base, not dev.*cached' \
  CIRCLE_BRANCH=feat/z CIRCLE_WORKFLOW_ID=wfl2 FAKE_FAIL=1
[ -s "$TMP/curl.log" ] && { echo "  FAIL: cached heavy answer called the API"; FAILED=1; }

if [ "$FAILED" != 0 ]; then
  echo "FAILED"
  exit 1
fi
echo "all passed"
