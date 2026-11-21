#!/usr/bin/env bash
# Tests for .github/scripts/heavy-suites-pr.sh. A fake `curl` on PATH answers
# the GitHub and CircleCI APIs from fixtures (or fails) and logs every request,
# and a fake `sleep` returns at once, so they need no network and no waiting.
# The real should-run-heavy.sh makes the decision. About 15 seconds.
# Run: bash .github/scripts/heavy-suites-pr.test.sh

set -uo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$DIR/heavy-suites-pr.sh"
FAILED=0
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Fake curl: logs "METHOD URL BODY" per request, then answers by URL. A *_FAIL
# variable makes that request fail like `curl -f` on an HTTP error. CircleCI
# reads come from files in $FAKE_DIR named after the request (see cci_key);
# for the n-th read of a key, <key>.<n> wins, then <key>.after once a pipeline
# was started, then <key>.
mkdir -p "$TMP/bin"
cat > "$TMP/bin/curl" <<'EOF'
#!/usr/bin/env bash
method=GET url="" body=""
while [ $# -gt 0 ]; do
  case "$1" in
    -X) method="$2"; shift ;;
    -d|--data|--data-raw) body="$2"; [ "$method" = GET ] && method=POST; shift ;;
    -H|-o|-w|--max-time|--retry) shift ;;
    http*) url="$1" ;;
  esac
  shift
done
echo "$method $url $body" >> "$FAKE_LOG"
echo $(( $(cat "$FAKE_CLOCK") + ${FAKE_CURL_SECS:-0} )) > "$FAKE_CLOCK"
answer() { [ -n "${2:-}" ] && exit 22; printf '%s' "$1"; exit 0; }
fixture() { # <key> <fail>
  [ -n "${2:-}" ] && exit 22
  local n f
  n=$(( $(cat "$FAKE_DIR/$1.count" 2>/dev/null || echo 0) + 1 ))
  echo "$n" > "$FAKE_DIR/$1.count"
  for f in "$FAKE_DIR/$1.$n" "$FAKE_DIR/$1.after" "$FAKE_DIR/$1"; do
    case "$f" in *.after) grep -q '^POST .*/pipeline/run ' "$FAKE_LOG" || continue ;; esac
    [ -f "$f" ] && { cat "$f"; exit 0; }
  done
  exit 22 # 404
}
run_answer='{"id":"p-new","number":4242,"state":"created"}'
case "$url" in
  */repos/*/pulls/[0-9]*) answer "{\"head\":{\"sha\":\"${FAKE_PR_HEAD:-$FAKE_SHA}\"}}" "${FAKE_PR_FAIL:-}" ;;
  */repos/*/pulls*) answer "${FAKE_PULLS:-[]}" "${FAKE_PULLS_FAIL:-}" ;;
  */repos/*/statuses/*) answer '{}' "${FAKE_STATUS_FAIL:-}" ;;
  */pipeline/run) answer "${FAKE_CCI_RUN:-$run_answer}" "${FAKE_CCI_RUN_FAIL:-}" ;;
  */pipeline-definitions) answer "${FAKE_CCI_DEFS:-}" "${FAKE_CCI_FAIL:-}" ;;
  */api/v2/project/*/pipeline\?*) fixture pipelines "${FAKE_CCI_FAIL:-}" ;;
  */api/v2/pipeline/*/workflow) u="${url%/workflow}"; fixture "wf_${u##*/}" "${FAKE_CCI_FAIL:-}" ;;
  */api/v2/pipeline/*) fixture "pipe_${url##*/}" "${FAKE_CCI_FAIL:-}" ;;
  */api/v2/workflow/*/job) u="${url%/job}"; fixture "jobs_${u##*/}" "${FAKE_CCI_FAIL:-}" ;;
  */api/v1.1/project/*) fixture "job_${url##*/}" "${FAKE_CCI_FAIL:-}" ;;
  */api/v2/project/*) answer "${FAKE_CCI_PROJECT:-}" "${FAKE_CCI_FAIL:-}" ;;
esac
echo "fake curl: no answer for $url" >&2
exit 22
EOF
# Fake sleep: advances the fake clock. FAKE_SLEEP_TERM=1 sends TERM to the
# script instead, as a cancelled or timed-out Actions job does.
cat > "$TMP/bin/sleep" <<'EOF'
#!/usr/bin/env bash
echo "sleep $*" >> "$FAKE_LOG"
[ -n "${FAKE_SLEEP_TERM:-}" ] && kill -TERM "$PPID"
echo $(( $(cat "$FAKE_CLOCK") + $1 )) > "$FAKE_CLOCK"
EOF
# Fake date: `date +%s` reads the fake clock. Each curl adds FAKE_CURL_SECS.
cat > "$TMP/bin/date" <<'EOF'
#!/usr/bin/env bash
[ "$*" = +%s ] && exec cat "$FAKE_CLOCK"
exec /usr/bin/date "$@"
EOF
chmod +x "$TMP/bin/curl" "$TMP/bin/sleep" "$TMP/bin/date"

SHA=0123456789abcdef0123456789abcdef01234567
OTHER=89abcdef0123456789abcdef0123456789abcdef

pr() { # number draft base [label]
  local labels='[]'
  [ -n "${4:-}" ] && labels="[{\"name\":\"$4\"}]"
  printf '{"number":%s,"state":"open","draft":%s,"base":{"ref":"%s"},"labels":%s}' "$1" "$2" "$3" "$labels"
}

# event <action> <draft> <base> [extra jq assignments]: writes $TMP/event.json,
# a pull_request event for PR #30 from coasys/ad4m:feat/x.
event() {
  jq -n --arg a "$1" --argjson d "$2" --arg b "$3" --arg sha "$SHA" '{
    action: $a,
    pull_request: {number: 30, draft: $d, base: {ref: $b, repo: {full_name: "coasys/ad4m"}},
      head: {ref: "feat/x", sha: $sha, repo: {full_name: "coasys/ad4m"}}, labels: []}
  }'"${4:-}" > "$TMP/event.json"
}

# CircleCI fixtures. fx <key> <json>: the answer for that request (see the fake).
fx() { printf '%s' "$2" > "$TMP/fx/$1"; }
fx_reset() { rm -rf "$TMP/fx"; mkdir -p "$TMP/fx"; }
# pipes <id>:<number>:<sha> ...: the branch's pipeline list, as the GitHub App
# project answers it (no `vcs`; the commit is in trigger_parameters).
pipes() {
  local p items=()
  for p in "$@"; do
    IFS=: read -r id n s <<<"$p"
    items+=("{\"id\":\"$id\",\"number\":$n,\"state\":\"created\",\"trigger_parameters\":{\"git\":{\"branch\":\"feat/x\",\"checkout_sha\":\"$s\"}}}")
  done
  local IFS=,
  printf '{"items":[%s]}' "${items[*]}"
}
wf() { printf '{"items":[{"id":"wf-n","name":"nightly-llm-e2e","status":"success"},{"id":"%s","name":"build-and-test","status":"%s"}]}' "$1" "$2"; }
# jobs <name>:<number|null>:<status> ...
jobs() {
  local j items=()
  for j in "$@"; do
    IFS=: read -r name n st <<<"$j"
    items+=("{\"name\":\"$name\",\"job_number\":$n,\"status\":\"$st\"}")
  done
  local IFS=,
  printf '{"items":[%s]}' "${items[*]}"
}
GATE_STEP='{"name":"Stack gate (heavy suites only for non-draft PRs to dev)"}'
# v1.1 job details: steps of a heavy job that ran, halted, is at its gate, or a light job.
step_ran() { printf '{"lifecycle":"%s","steps":[{"name":"Checkout code"},%s,{"name":"rust-executor tests"}]}' "${1:-running}" "$GATE_STEP"; }
step_halted() { printf '{"lifecycle":"finished","steps":[{"name":"Checkout code"},%s]}' "$GATE_STEP"; }
step_light() { printf '{"lifecycle":"%s","steps":[{"name":"Checkout code"},{"name":"cargo build"}]}' "${1:-finished}"; }

# A pipeline p1 for $SHA whose build finished; its rust-tests job (11) ran,
# halted or is still in build.
push_ran() {
  fx pipelines "$(pipes p-old:7:ffff p1:8:$SHA)"; fx wf_p1 "$(wf wf1 running)"
  fx jobs_wf1 "$(jobs build:10:success rust-tests:11:running integration-tests-js:12:running clippy:13:running)"
  fx job_10 "$(step_light)"; fx job_11 "$(step_ran)"; fx job_12 "$(step_ran)"; fx job_13 "$(step_light running)"
}
push_halted() {
  fx pipelines "$(pipes p1:8:$SHA)"; fx wf_p1 "$(wf wf1 success)"
  fx jobs_wf1 "$(jobs build:10:success rust-tests:11:success integration-tests-js:12:success)"
  fx job_10 "$(step_light)"; fx job_11 "$(step_halted)"; fx job_12 "$(step_halted)"
}
push_building() {
  fx pipelines "$(pipes p1:8:$SHA)"; fx wf_p1 "$(wf wf1 running)"
  fx jobs_wf1 "$(jobs build:10:running rust-tests:null:blocked)"
  fx job_10 "$(step_light running)"
}
# After a pipeline/run, the new pipeline p-new (4242) for $SHA with jobs in workflow wf2.
new_pipeline() { # <jobs json> <wf status> [step for job 21]
  fx pipelines.after "$(pipes p-new:4242:$SHA p1:8:$SHA)"; fx wf_p-new "$(wf wf2 "$2")"
  fx jobs_wf2 "$1"; fx job_20 "$(step_light)"; fx job_21 "${3:-$(step_ran)}"
  fx pipe_p-new '{"id":"p-new","number":4242,"state":"created"}'
}

# check <name> <expected exit> <expected output pattern> [VAR=value ...]
check() {
  local name="$1" want_rc="$2" want_out="$3"
  shift 3
  : > "$TMP/curl.log"
  echo 1000000 > "$TMP/clock"
  rm -f "$TMP"/fx/*.count
  local out rc=0
  out="$(env -i PATH="$TMP/bin:$PATH" HOME="$TMP" FAKE_LOG="$TMP/curl.log" FAKE_DIR="$TMP/fx" FAKE_SHA="$SHA" \
    FAKE_CLOCK="$TMP/clock" \
    GITHUB_EVENT_PATH="$TMP/event.json" GITHUB_REPOSITORY=coasys/ad4m GITHUB_TOKEN=ghs_test \
    GITHUB_SERVER_URL=https://github.com GITHUB_RUN_ID=77 \
    HEAVY_GATE_COMMIT_MSG="feat: something" HEAVY_SUITES_WAIT_SECS=600 HEAVY_SUITES_POLL_SECS=60 \
    HEAVY_SUITES_GRACE_SECS=120 \
    "$@" bash "$SCRIPT" 2>&1)" || rc=$?
  if [ "$rc" != "$want_rc" ] || ! grep -qE "$want_out" <<<"$out"; then
    echo "  FAIL: $name: exit $rc (want $want_rc), output:"
    while IFS= read -r l; do echo "      $l"; done <<<"$out"
    FAILED=1
  else
    echo "  ok: $name -> $(tail -1 <<<"$out")"
  fi
}
# logged <pattern> <message>: the fake curl saw a request matching the pattern.
logged() { grep -qE "$1" "$TMP/curl.log" || { echo "  FAIL: $2"; sed 's/^/      /' "$TMP/curl.log"; FAILED=1; }; }
not_logged() { ! grep -qE "$1" "$TMP/curl.log" || { echo "  FAIL: $2"; sed 's/^/      /' "$TMP/curl.log"; FAILED=1; }; }
# statuses <state> ...: the ci/heavy-suites statuses posted for $SHA, in order.
# Each has the context, the run's target_url and at most 140 characters.
statuses() {
  local want="$*" got="" line b
  while IFS= read -r line; do
    [ -z "$line" ] && continue
    b="${line#* * }"
    if [ "$(jq -r .context <<<"$b")" != ci/heavy-suites ] \
      || [ "$(jq -r '.description | length <= 140' <<<"$b")" != true ] \
      || [ "$(jq -r .target_url <<<"$b")" != https://github.com/coasys/ad4m/actions/runs/77 ]; then
      echo "  FAIL: malformed status: $b"; FAILED=1
    fi
    # A run can end after any status (a cancel, the job's timeout), so each
    # pending names what clears it: the PR getting ready, or a re-run.
    if [ "$(jq -r .state <<<"$b")" = pending ] \
      && ! jq -r .description <<<"$b" | grep -qE '[Rr]e-run|Runs when the PR is ready'; then
      echo "  FAIL: pending without a recovery: $(jq -r .description <<<"$b")"; FAILED=1
    fi
    got="${got:+$got }$(jq -r .state <<<"$b")"
  done <<<"$(grep -E "^POST .*/repos/coasys/ad4m/statuses/$SHA " "$TMP/curl.log")"
  [ "$got" = "$want" ] || { echo "  FAIL: want statuses '$want', got '$got'"; FAILED=1; }
}
# Every pending description names a recovery that clears it.
recovery() {
  local d
  d="$(grep -E "^POST .*/statuses/" "$TMP/curl.log" | tail -1)"
  d="$(jq -r .description <<<"${d#* * }")"
  grep -qE "$1" <<<"$d" || { echo "  FAIL: description '$d' does not match '$1'"; FAILED=1; }
  ! grep -q "Rerun the CircleCI workflow before merging" <<<"$d" || { echo "  FAIL: dead-end recovery: $d"; FAILED=1; }
  ! grep -qE "(, |then |or )Re-run" <<<"$d" || { echo "  FAIL: capital letter mid-sentence: $d"; FAILED=1; }
}

CCI=(CIRCLECI_API_TOKEN=cci_test CIRCLECI_PIPELINE_DEFINITION_ID=def-1)
RUN_URL='^POST https://circleci.com/api/v2/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4/pipeline/run '
RERUN='[Rr]e-run the Heavy suites workflow'
NEW_RAN="$(jobs build:20:success rust-tests:21:running)"

echo "heavy-suites-pr.sh"

# The gate skips: pending at once, no CircleCI call.
fx_reset
event synchronize true dev
check "push to a draft PR: pending" 0 'pending: Not run: PR #30 is a draft' \
  FAKE_PULLS="[$(pr 30 true dev)]" "${CCI[@]}"
statuses pending
not_logged circleci.com "a skipped PR called CircleCI"
logged 'GET https://api.github.com/repos/coasys/ad4m/pulls\?state=open&per_page=100&head=coasys%3Afeat%2Fx ' \
  "asked GitHub for the branch's PRs"
event synchronize false feat/base
check "push to a stacked PR: pending" 0 'pending: Not run: PR #30 targets feat/base, not dev' \
  FAKE_PULLS="[$(pr 30 false feat/base)]" "${CCI[@]}"
statuses pending

# The gate runs: success only once a pipeline of this commit runs the heavy jobs.
event synchronize false dev
push_ran
check "push to a ready PR to dev, heavy jobs running: success, no new pipeline" 0 "success: Heavy suites running in CircleCI pipeline 8" \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
statuses pending success
not_logged "$RUN_URL" "started a pipeline while one ran the suites"
logged 'GET https://circleci.com/api/v2/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4/pipeline\?branch=feat%2Fx ' \
  "did not list the branch's pipelines"
logged 'GET https://circleci.com/api/v2/pipeline/p1/workflow ' "did not pick the pipeline of the head commit"
not_logged 'pipeline/p-old/' "read the pipeline of another commit"
logged 'GET https://circleci.com/api/v1.1/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4/11 ' \
  "did not read the heavy job's steps"
check "push without CIRCLECI_API_TOKEN, heavy jobs running: success (reads need no token)" 0 '^success:' \
  FAKE_PULLS="[$(pr 30 false dev)]"
check "[full-ci] in the head commit on a draft: success" 0 '^success:' \
  FAKE_PULLS="[$(pr 30 true dev)]" HEAVY_GATE_COMMIT_MSG=$'fix: y\n\n[full-ci]'
statuses pending success

fx_reset
push_building
fx jobs_wf1.3 "$(jobs build:10:success rust-tests:11:running)"; fx job_11 "$(step_ran)"
check "push, build still running: pending, then success" 0 '^success: .*pipeline 8' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
statuses pending pending success
not_logged "$RUN_URL" "started a pipeline while the push's build ran"
logged '^sleep 60' "did not wait between polls"

fx_reset
fx pipelines '{"items":[]}'
fx pipelines.2 "$(pipes p1:8:$SHA)"; fx wf_p1 "$(wf wf1 running)"
fx jobs_wf1 "$(jobs build:10:success rust-tests:11:running)"; fx job_10 "$(step_light)"; fx job_11 "$(step_ran)"
check "push, CircleCI has no pipeline yet: waits, no new pipeline" 0 '^success: .*pipeline 8' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
not_logged "$RUN_URL" "started a second pipeline before the push's one appeared"

fx_reset
fx pipelines "$(pipes p1:8:$SHA)"; fx wf_p1 "$(wf wf1 failed)"
fx jobs_wf1 "$(jobs build:10:failed rust-tests:null:blocked)"; fx job_10 "$(step_light)"
check "push, build failed: pending, no new pipeline" 0 'pending: CircleCI pipeline 8 stopped before the heavy suites' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
statuses pending pending
recovery "Push a fix.*$RERUN"
not_logged "$RUN_URL" "rebuilt a commit whose build failed"

fx_reset
push_halted
new_pipeline "$NEW_RAN" running
check "push, the push's pipeline halted them: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
statuses pending pending success

# A state change with no push: the push's pipeline halted the heavy jobs.
fx_reset
event ready_for_review false dev
push_halted
new_pipeline "$NEW_RAN" running
check "ready for review, to dev: new pipeline, pending until its heavy jobs run" 0 'success: Heavy suites running in CircleCI pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
logged "$RUN_URL" "no pipeline/run request"
line="$(grep -E "$RUN_URL" "$TMP/curl.log" | head -1)"
[ "$(jq -c . <<<"${line#* * }")" = '{"definition_id":"def-1","config":{"branch":"feat/x"},"checkout":{"branch":"feat/x"}}' ] \
  || { echo "  FAIL: pipeline/run body: ${line#* * }"; FAILED=1; }
statuses pending pending success
d="$(grep -E "^POST .*/statuses/" "$TMP/curl.log" | sed -n 2p)"
[ "$(jq -r .description <<<"${d#* * }")" = "Heavy suites started in CircleCI pipeline 4242; waiting for them. If this stays, re-run the Heavy suites workflow (Details)." ] \
  || { echo "  FAIL: first status after the trigger: ${d#* * }"; FAILED=1; }
grep -qE "^GET .*/pulls/30 " "$TMP/curl.log" || { echo "  FAIL: did not check the PR's head before the trigger"; FAILED=1; }

fx_reset
push_halted
new_pipeline "$(jobs build:20:running rust-tests:null:blocked)" running
fx jobs_wf2.3 "$NEW_RAN"
check "ready, the new pipeline still builds: pending, then success" 0 '^success: .*4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
statuses pending pending success
[ "$(grep -cE "$RUN_URL" "$TMP/curl.log")" = 1 ] || { echo "  FAIL: more than one pipeline/run"; FAILED=1; }

fx_reset
push_halted
new_pipeline "$(jobs build:20:success rust-tests:21:success)" success "$(step_halted)"
check "ready, the new pipeline halts them too: pending" 0 'pending: CircleCI pipeline 4242 halted the heavy suites' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
statuses pending pending pending
recovery "$RERUN"
[ "$(grep -cE "$RUN_URL" "$TMP/curl.log")" = 1 ] || { echo "  FAIL: more than one pipeline/run"; FAILED=1; }

fx_reset
push_halted
fx pipe_p-new '{"id":"p-new","number":4242,"state":"errored"}'
check "ready, the new pipeline errors (config): pending" 0 'pending: CircleCI pipeline 4242 did not start' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
statuses pending pending pending
recovery "push.*$RERUN"

fx_reset
push_halted
new_pipeline "$(jobs build:20:running rust-tests:null:blocked)" running "$(step_light running)"
check "ready, the heavy jobs never start: pending after the wait" 0 'pending: CircleCI pipeline 4242 did not reach the heavy suites in 10 min' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
statuses pending pending pending
recovery "$RERUN"
[ "$(grep -c '^sleep' "$TMP/curl.log")" -le 10 ] || { echo "  FAIL: more polls than the wait allows"; FAILED=1; }

fx_reset
push_halted
check "ready, the branch moved past the commit: no pipeline, only the first pending" 0 "head of PR #30 is now $OTHER" \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}" FAKE_PR_HEAD="$OTHER"
not_logged "$RUN_URL" "started a pipeline for a newer commit than the event's"
statuses pending

fx_reset
push_halted
fx pipelines.after "$(pipes p-new:4242:$OTHER p1:8:$SHA)"
fx pipe_p-new "{\"id\":\"p-new\",\"number\":4242,\"state\":\"created\",\"trigger_parameters\":{\"git\":{\"checkout_sha\":\"$OTHER\"}}}"
check "ready, the new pipeline checked out a newer commit: no success" 0 "pipeline 4242 checked out $OTHER" \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
statuses pending pending
not_logged '/statuses/.*"success"' "claimed success for a commit the pipeline did not build"

fx_reset
push_ran
check "ready, a pipeline of this commit already runs them (a re-run): success, no new pipeline" 0 '^success: .*pipeline 8' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
not_logged "$RUN_URL" "started a second pipeline"

# Lal's review r2, 1: the newest pipeline of the commit is the one its
# ci/circleci checks show. An older one that ran (and failed) must not vouch
# for a newer one that halted.
fx_reset
fx pipelines "$(pipes p9:9:$SHA p8:8:$SHA)"
fx wf_p9 "$(wf wf9 success)"; fx jobs_wf9 "$(jobs build:90:success rust-tests:91:success)"
fx job_90 "$(step_light)"; fx job_91 "$(step_halted)"
fx wf_p8 "$(wf wf8 failed)"; fx jobs_wf8 "$(jobs build:80:success rust-tests:81:failed)"
fx job_80 "$(step_light)"; fx job_81 "$(step_ran finished)"
new_pipeline "$NEW_RAN" running
fx pipelines.after "$(pipes p-new:4242:$SHA p9:9:$SHA p8:8:$SHA)"
check "ready, an older pipeline ran and failed, the newest halted: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
logged "$RUN_URL" "took the older pipeline's run for the newer halted one"
not_logged '/statuses/.*pipeline 8' "named the older pipeline"
# The same with a re-run of the workflow in CircleCI: the newest
# build-and-test workflow of the pipeline counts.
fx_reset
fx pipelines "$(pipes p8:8:$SHA)"
fx wf_p8 '{"items":[{"id":"wf-b","name":"build-and-test","status":"success","created_at":"2026-11-21T10:00:00Z"},{"id":"wf-a","name":"build-and-test","status":"failed","created_at":"2026-11-21T09:00:00Z"}]}'
fx jobs_wf-a "$(jobs build:80:success rust-tests:81:failed)"; fx job_81 "$(step_ran finished)"
fx jobs_wf-b "$(jobs rust-tests:91:success)"; fx job_91 "$(step_halted)"
fx job_80 "$(step_light)"
new_pipeline "$NEW_RAN" running
fx pipelines.after "$(pipes p-new:4242:$SHA p8:8:$SHA)"
check "ready, the first workflow ran, its re-run halted: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
logged "$RUN_URL" "took the first workflow's run for its halted re-run"

# Lal's review r2, 2: a job whose details cannot be read may have halted, so
# the workflow is not "ran" while one is unread.
fx_reset
push_ran
rm "$TMP/fx/job_11"
check "a heavy job's details never read, another ran: pending, no success" 0 'pending: CircleCI pipeline 8 did not reach the heavy suites in 10 min' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
not_logged '/statuses/.*"success"' "success while a heavy job was unread"
fx_reset
push_ran
fx jobs_wf1 "$(jobs build:10:success rust-tests:11:success integration-tests-js:12:running)"
fx job_11.1 '<html>502</html>'; fx job_11.2 '{}'; fx job_11 "$(step_halted)"
new_pipeline "$NEW_RAN" running
check "a heavy job unreadable, then read as halted: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
logged "$RUN_URL" "no new pipeline after the unread job turned out halted"

# Lal's review r2, 3: a cancel or the job's timeout can end the run after any
# status. The first status comes before the CircleCI reads, and the one left
# behind names the re-run.
fx_reset
push_building
check "cancelled during the first wait: exit 143, the pending names the re-run" 143 '^pending: Waiting' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}" FAKE_SLEEP_TERM=1
recovery "$RERUN"
first="$(grep -nE '/statuses/' "$TMP/curl.log" | head -1 | cut -d: -f1)"
cci1="$(grep -nE 'circleci.com' "$TMP/curl.log" | head -1 | cut -d: -f1)"
[ -n "$first" ] && [ "$first" -lt "${cci1:-0}" ] || { echo "  FAIL: no status before the first CircleCI read"; FAILED=1; }
# Slow API answers count against the wait: the loop is bounded by the clock,
# not by the number of sleeps, so the final status is posted in time.
fx_reset
push_halted
new_pipeline "$(jobs build:20:running rust-tests:null:blocked)" running "$(step_light running)"
check "slow CircleCI (30 s per request): ends by the wait, with the final pending" 0 'pending: CircleCI pipeline 4242 did not reach the heavy suites in 10 min' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}" FAKE_CURL_SECS=30
recovery "$RERUN"
spent=$(( $(cat "$TMP/clock") - 1000000 ))
# 600 s of wait, the last poll's 60 s, one request past that and the final status.
[ "$spent" -le 720 ] || { echo "  FAIL: took $spent s of a 600 s wait"; FAILED=1; }
# The default wait and poll leave 5 min of the job's timeout-minutes for
# checkout, the gate, the last request and the final status.
wait_default="$(grep -oE 'HEAVY_SUITES_WAIT_SECS:-[0-9]+' "$SCRIPT" | grep -oE '[0-9]+$')"
poll_default="$(grep -oE 'HEAVY_SUITES_POLL_SECS:-[0-9]+' "$SCRIPT" | grep -oE '[0-9]+$')"
timeout_min="$(grep -oE 'timeout-minutes: [0-9]+' "$DIR/../workflows/heavy-suites.yml" | grep -oE '[0-9]+$')"
[ $(( timeout_min * 60 - wait_default - poll_default )) -ge 300 ] \
  || { echo "  FAIL: timeout-minutes $timeout_min leaves under 5 min past the ${wait_default} s wait"; FAILED=1; }
# A poll at the deadline starts nothing: that would run past it.
fx_reset
fx pipelines '{"items":[]}'
check "no pipeline until the wait is over: no new pipeline at the deadline" 0 'pending: No CircleCI pipeline of this commit reached the heavy suites in 10 min' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}" HEAVY_SUITES_GRACE_SECS=600
not_logged "$RUN_URL" "started a pipeline after the wait"
recovery "$RERUN"

fx_reset
event ready_for_review false feat/base
check "ready for review, stacked: pending, no pipeline" 0 'pending: Not run: PR #30 targets feat/base' \
  FAKE_PULLS="[$(pr 30 false feat/base)]" "${CCI[@]}"
not_logged circleci.com "a stacked PR called CircleCI"
event edited false dev '| .changes = {base: {ref: {from: "feat/base"}}}'
push_halted; new_pipeline "$NEW_RAN" running
check "base changed to dev: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
logged "$RUN_URL" "retarget to dev started no pipeline"
event edited false feat/other '| .changes = {base: {ref: {from: "feat/base"}}}'
check "base changed to another stack branch: pending" 0 '^pending:' \
  FAKE_PULLS="[$(pr 30 false feat/other)]" "${CCI[@]}"
not_logged circleci.com "a stack retarget called CircleCI"
event edited false dev '| .changes = {title: {from: "old"}}'
check "title edit: nothing" 0 'not a base change' FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
[ -s "$TMP/curl.log" ] && { echo "  FAIL: a title edit made requests"; FAILED=1; }
event reopened false dev
check "reopened, to dev: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
logged "$RUN_URL" "reopen started no pipeline"
event labeled true feat/base '| .label = {name: "full-ci"}'
check "full-ci label on a stacked draft: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 true feat/base full-ci)]" "${CCI[@]}"
event labeled false dev '| .label = {name: "bug"}'
check "another label: nothing" 0 'not the full-ci label' "${CCI[@]}"
[ -s "$TMP/curl.log" ] && { echo "  FAIL: another label made requests"; FAILED=1; }
event unlabeled true feat/base '| .label = {name: "full-ci"}'
check "full-ci label removed from a stacked draft: pending, no pipeline" 0 '^pending:' \
  FAKE_PULLS="[$(pr 30 true feat/base)]" "${CCI[@]}"
not_logged circleci.com "an unlabel called CircleCI"
event converted_to_draft true dev
check "converted to draft: pending" 0 'pending: Not run: PR #30 is a draft' \
  FAKE_PULLS="[$(pr 30 true dev)]" "${CCI[@]}"

# Without the CircleCI secret: no trigger, a log line, never green without a run.
fx_reset
event ready_for_review false dev
push_halted
check "no CIRCLECI_API_TOKEN: pending, log line, exit 0" 0 'pending: .*no CIRCLECI_API_TOKEN' \
  FAKE_PULLS="[$(pr 30 false dev)]"
not_logged "$RUN_URL" "started a pipeline without a token"
statuses pending pending
recovery "Push a commit, or set the secret and re-run"
check "no CIRCLECI_API_TOKEN: the log names the secret" 0 'CIRCLECI_API_TOKEN is not set' \
  FAKE_PULLS="[$(pr 30 false dev)]"

# Opened: the push's pipeline may have passed its gate before the PR existed.
# (r1 set success here without a token; Lal's review: nothing backs that.)
event opened false dev
check "opened, no CIRCLECI_API_TOKEN, the push's pipeline halted them: pending" 0 '^pending: .*no CIRCLECI_API_TOKEN' \
  FAKE_PULLS="[$(pr 30 false dev)]"
statuses pending pending
not_logged "$RUN_URL" "started a pipeline without a token"
fx_reset
push_ran
check "opened, no CIRCLECI_API_TOKEN, the push's gate saw the PR: success" 0 '^success:' \
  FAKE_PULLS="[$(pr 30 false dev)]"
fx_reset
push_building
fx jobs_wf1.2 "$(jobs build:10:success rust-tests:11:running)"; fx job_11 "$(step_ran)"
check "opened while the push's build still runs: no new pipeline, success" 0 "success: .*pipeline 8" \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
not_logged "$RUN_URL" "started a second pipeline while the gate had not run"
fx_reset
push_halted; new_pipeline "$NEW_RAN" running
check "opened after the push's pipeline halted them: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
logged "$RUN_URL" "no new pipeline after the gate had halted"
fx_reset
fx pipelines '{"items":[]}'; new_pipeline "$NEW_RAN" running
check "opened, no pipeline for the head commit after the grace time: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
[ "$(grep -c '^sleep' "$TMP/curl.log")" -ge 2 ] || { echo "  FAIL: did not wait for CircleCI's own pipeline first"; FAILED=1; }
event opened true dev
check "opened as a draft: pending, no CircleCI call" 0 'pending: Not run: PR #30 is a draft' \
  FAKE_PULLS="[$(pr 30 true dev)]" "${CCI[@]}"
not_logged circleci.com "a draft called CircleCI"

# Fails visible: an API failure means pending with a recovery, never green.
fx_reset
event ready_for_review false dev
push_halted
check "CircleCI trigger fails: pending, exit 0" 0 'pending: Could not start a CircleCI pipeline \(CircleCI trigger failed\)' \
  FAKE_PULLS="[$(pr 30 false dev)]" FAKE_CCI_RUN_FAIL=1 "${CCI[@]}"
statuses pending pending
recovery "$RERUN"
check "CircleCI answers without an id: pending" 0 '^pending: Could not start' \
  FAKE_PULLS="[$(pr 30 false dev)]" FAKE_CCI_RUN='{"message":"Permission denied"}' "${CCI[@]}"
check "CircleCI reads fail: pending after the wait, no pipeline" 0 'pending: No CircleCI pipeline of this commit reached the heavy suites in 10 min' \
  FAKE_PULLS="[$(pr 30 false dev)]" FAKE_CCI_FAIL=1 "${CCI[@]}"
not_logged "$RUN_URL" "started a pipeline blind"
recovery "$RERUN"
# (r1 started a pipeline anyway here. Now an unknown PR state starts nothing;
# the pending names the re-run, which asks again and starts it.)
check "PR lookup fails on ready, nothing ran: pending, no pipeline" 0 'pending: PR state unknown \(GitHub API request failed\)\. Re-run the Heavy suites workflow' \
  FAKE_PULLS_FAIL=1 "${CCI[@]}"
not_logged "$RUN_URL" "started a pipeline on an unknown PR state"
statuses pending pending
fx_reset
push_ran
check "PR lookup fails, a pipeline of this commit runs them: success" 0 '^success: .*pipeline 8' \
  FAKE_PULLS_FAIL=1 "${CCI[@]}"
event synchronize false dev
check "status request fails: exit 1" 1 'could not set' \
  FAKE_PULLS="[$(pr 30 false dev)]" FAKE_STATUS_FAIL=1

# The pipeline definition, when the variable is not set.
fx_reset
event ready_for_review false dev
push_halted; new_pipeline "$NEW_RAN" running
check "definition looked up when exactly one" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" CIRCLECI_API_TOKEN=cci_test \
  FAKE_CCI_PROJECT='{"id":"proj-uuid","slug":"circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4"}' \
  FAKE_CCI_DEFS='{"items":[{"id":"def-only","name":"build-and-test"}]}'
logged 'GET https://circleci.com/api/v2/projects/proj-uuid/pipeline-definitions ' "no definitions lookup"
logged "${RUN_URL}.*\"definition_id\":\"def-only\"" "the looked-up definition was not used"
check "two definitions and no variable: pending, no pipeline" 0 'pending: .*CIRCLECI_PIPELINE_DEFINITION_ID' \
  FAKE_PULLS="[$(pr 30 false dev)]" CIRCLECI_API_TOKEN=cci_test \
  FAKE_CCI_PROJECT='{"id":"proj-uuid"}' FAKE_CCI_DEFS='{"items":[{"id":"a"},{"id":"b"}]}'
not_logged "$RUN_URL" "started a pipeline with a guessed definition"
recovery "$RERUN"

# Fork PRs: no secrets and a read-only token; do nothing.
event ready_for_review false dev '| .pull_request.head.repo.full_name = "someone/ad4m"'
check "fork PR: nothing, exit 0" 0 'fork' FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
[ -s "$TMP/curl.log" ] && { echo "  FAIL: a fork PR made requests"; FAILED=1; }

if [ "$FAILED" != 0 ]; then
  echo "FAILED"
  exit 1
fi
echo "all passed"
