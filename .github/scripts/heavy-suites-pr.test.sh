#!/usr/bin/env bash
# Tests for .github/scripts/heavy-suites-pr.sh. A fake `curl` on PATH answers
# the GitHub and CircleCI APIs from fixture JSON (or fails) and logs every
# request, so they need no network. The real should-run-heavy.sh makes the
# decision. About two seconds.
# Run: bash .github/scripts/heavy-suites-pr.test.sh

set -uo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
SCRIPT="$DIR/heavy-suites-pr.sh"
FAILED=0
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

# Fake curl: logs "METHOD URL BODY" per request, then answers by URL. A *_FAIL
# variable makes that request fail like `curl -f` on an HTTP error.
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
answer() { [ -n "${2:-}" ] && exit 22; printf '%s' "$1"; exit 0; }
run_answer='{"id":"p-new","number":4242}'
case "$url" in
  */repos/*/pulls*) answer "${FAKE_PULLS:-[]}" "${FAKE_PULLS_FAIL:-}" ;;
  */repos/*/statuses/*) answer '{}' "${FAKE_STATUS_FAIL:-}" ;;
  */pipeline/run) answer "${FAKE_CCI_RUN:-$run_answer}" "${FAKE_CCI_RUN_FAIL:-}" ;;
  */pipeline-definitions) answer "${FAKE_CCI_DEFS:-}" "${FAKE_CCI_FAIL:-}" ;;
  */project/*/pipeline\?*) answer "${FAKE_CCI_PIPELINES:-}" "${FAKE_CCI_FAIL:-}" ;;
  */pipeline/*/workflow) answer "${FAKE_CCI_WORKFLOWS:-}" "${FAKE_CCI_FAIL:-}" ;;
  */workflow/*/job) answer "${FAKE_CCI_JOBS:-}" "${FAKE_CCI_FAIL:-}" ;;
  */project/*) answer "${FAKE_CCI_PROJECT:-}" "${FAKE_CCI_FAIL:-}" ;;
esac
echo "fake curl: no answer for $url" >&2
exit 22
EOF
chmod +x "$TMP/bin/curl"

SHA=0123456789abcdef0123456789abcdef01234567

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

# check <name> <expected exit> <expected output pattern> [VAR=value ...]
check() {
  local name="$1" want_rc="$2" want_out="$3"
  shift 3
  : > "$TMP/curl.log"
  local out rc=0
  out="$(env -i PATH="$TMP/bin:$PATH" HOME="$TMP" FAKE_LOG="$TMP/curl.log" \
    GITHUB_EVENT_PATH="$TMP/event.json" GITHUB_REPOSITORY=coasys/ad4m GITHUB_TOKEN=ghs_test \
    GITHUB_SERVER_URL=https://github.com GITHUB_RUN_ID=77 \
    HEAVY_GATE_COMMIT_MSG="feat: something" \
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
# status_is <state>: the one status posted for $SHA has this state and context.
status_is() {
  local line
  line="$(grep -E "^POST .*/repos/coasys/ad4m/statuses/$SHA " "$TMP/curl.log")"
  if [ "$(grep -c . <<<"$line")" != 1 ] \
    || [ "$(jq -r .state <<<"${line#* * }")" != "$1" ] \
    || [ "$(jq -r .context <<<"${line#* * }")" != ci/heavy-suites ] \
    || [ "$(jq -r '.description | length <= 140' <<<"${line#* * }")" != true ] \
    || [ "$(jq -r .target_url <<<"${line#* * }")" != https://github.com/coasys/ad4m/actions/runs/77 ]; then
    echo "  FAIL: want one ci/heavy-suites status '$1', got: $line"
    FAILED=1
  fi
}

CCI=(CIRCLECI_API_TOKEN=cci_test CIRCLECI_PIPELINE_DEFINITION_ID=def-1)
RUN_URL='^POST https://circleci.com/api/v2/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4/pipeline/run '

echo "heavy-suites-pr.sh"

# A push to a PR: CircleCI already runs a pipeline; only the status.
event synchronize true dev
check "push to a draft PR: pending" 0 'pending: Not run: PR #30 is a draft' \
  FAKE_PULLS="[$(pr 30 true dev)]" "${CCI[@]}"
status_is pending
not_logged circleci.com "a push called CircleCI"
logged 'GET https://api.github.com/repos/coasys/ad4m/pulls\?state=open&per_page=100&head=coasys%3Afeat%2Fx ' \
  "asked GitHub for the branch's PRs"
event synchronize false feat/base
check "push to a stacked PR: pending" 0 'pending: Not run: PR #30 targets feat/base, not dev' \
  FAKE_PULLS="[$(pr 30 false feat/base)]" "${CCI[@]}"
status_is pending
event synchronize false dev
check "push to a ready PR to dev: success, no new pipeline" 0 "success: .*this commit's CircleCI pipeline" \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
status_is success
not_logged circleci.com "a push called CircleCI"
event synchronize true dev
check "[full-ci] in the head commit on a draft: success" 0 '^success:' \
  FAKE_PULLS="[$(pr 30 true dev)]" HEAVY_GATE_COMMIT_MSG=$'fix: y\n\n[full-ci]'
status_is success

# A state change with no push: a new pipeline, and success only once it started.
event ready_for_review false dev
check "ready for review, to dev: new pipeline, success" 0 'success: .*CircleCI pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
logged "$RUN_URL" "no pipeline/run request"
line="$(grep -E "$RUN_URL" "$TMP/curl.log" | head -1)"
[ "$(jq -c . <<<"${line#* * }")" = '{"definition_id":"def-1","config":{"branch":"feat/x"},"checkout":{"branch":"feat/x"}}' ] \
  || { echo "  FAIL: pipeline/run body: ${line#* * }"; FAILED=1; }
status_is success
event ready_for_review false feat/base
check "ready for review, stacked: pending, no pipeline" 0 'pending: Not run: PR #30 targets feat/base' \
  FAKE_PULLS="[$(pr 30 false feat/base)]" "${CCI[@]}"
not_logged circleci.com "a stacked PR started a pipeline"
event edited false dev '| .changes = {base: {ref: {from: "feat/base"}}}'
check "base changed to dev: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
logged "$RUN_URL" "retarget to dev started no pipeline"
event edited false feat/other '| .changes = {base: {ref: {from: "feat/base"}}}'
check "base changed to another stack branch: pending" 0 '^pending:' \
  FAKE_PULLS="[$(pr 30 false feat/other)]" "${CCI[@]}"
not_logged circleci.com "a stack retarget started a pipeline"
event edited false dev '| .changes = {title: {from: "old"}}'
check "title edit: nothing" 0 'not a base change' FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
[ -s "$TMP/curl.log" ] && { echo "  FAIL: a title edit made requests"; FAILED=1; }
event reopened false dev
check "reopened, to dev: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
event labeled true feat/base '| .label = {name: "full-ci"}'
check "full-ci label on a stacked draft: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 true feat/base full-ci)]" "${CCI[@]}"
event labeled false dev '| .label = {name: "bug"}'
check "another label: nothing" 0 'not the full-ci label' "${CCI[@]}"
[ -s "$TMP/curl.log" ] && { echo "  FAIL: another label made requests"; FAILED=1; }
event unlabeled true feat/base '| .label = {name: "full-ci"}'
check "full-ci label removed from a stacked draft: pending, no pipeline" 0 '^pending:' \
  FAKE_PULLS="[$(pr 30 true feat/base)]" "${CCI[@]}"
not_logged circleci.com "an unlabel started a pipeline"
event converted_to_draft true dev
check "converted to draft: pending" 0 'pending: Not run: PR #30 is a draft' \
  FAKE_PULLS="[$(pr 30 true dev)]" "${CCI[@]}"

# Without the CircleCI secret: skip the trigger with a log line, never fail, never green.
event ready_for_review false dev
check "no CIRCLECI_API_TOKEN: pending, log line, exit 0" 0 'pending: Not run yet' \
  FAKE_PULLS="[$(pr 30 false dev)]"
not_logged circleci.com "called CircleCI without a token"
status_is pending
check "no CIRCLECI_API_TOKEN: the log names the secret" 0 'CIRCLECI_API_TOKEN is not set' \
  FAKE_PULLS="[$(pr 30 false dev)]"

# Fails visible: an API failure costs a full run or a pending status.
check "CircleCI trigger fails: pending, exit 0" 0 'pending: Not run yet: .*CircleCI' \
  FAKE_PULLS="[$(pr 30 false dev)]" FAKE_CCI_RUN_FAIL=1 "${CCI[@]}"
status_is pending
check "CircleCI answers without an id: pending" 0 '^pending:' \
  FAKE_PULLS="[$(pr 30 false dev)]" FAKE_CCI_RUN='{"message":"Permission denied"}' "${CCI[@]}"
check "PR lookup fails on ready: new pipeline anyway, pending" 0 'pending: PR state unknown \(GitHub API request failed\); re-run this check\.$' \
  FAKE_PULLS_FAIL=1 "${CCI[@]}"
logged "$RUN_URL" "an unknown PR state on a state change started no pipeline"
status_is pending
event synchronize false dev
check "PR lookup fails on a push: pending, no pipeline" 0 'pending: PR state unknown \(GitHub API request failed\); re-run this check\.$' \
  FAKE_PULLS_FAIL=1 "${CCI[@]}"
not_logged circleci.com "a push called CircleCI"
check "status request fails: exit 1" 1 'could not set' \
  FAKE_PULLS="[$(pr 30 false dev)]" FAKE_STATUS_FAIL=1

# The pipeline definition, when the variable is not set.
event ready_for_review false dev
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

# Opened: the push's pipeline may have passed its gate before the PR existed.
PIPES="{\"items\":[{\"id\":\"p-old\",\"vcs\":{\"revision\":\"ffff\"}},{\"id\":\"p1\",\"vcs\":{\"revision\":\"$SHA\"}}]}"
WFS='{"items":[{"id":"wf-n","name":"nightly-llm-e2e"},{"id":"wf1","name":"build-and-test"}]}'
event opened false dev
check "opened while the push's build still runs: no new pipeline, success" 0 "success: .*this commit's CircleCI pipeline" \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}" FAKE_CCI_PIPELINES="$PIPES" FAKE_CCI_WORKFLOWS="$WFS" \
  FAKE_CCI_JOBS='{"items":[{"name":"fmt","status":"success"},{"name":"build","status":"running"}]}'
logged 'GET https://circleci.com/api/v2/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4/pipeline\?branch=feat%2Fx ' \
  "did not list the branch's pipelines"
logged 'GET https://circleci.com/api/v2/pipeline/p1/workflow ' "did not pick the pipeline of the head commit"
logged 'GET https://circleci.com/api/v2/workflow/wf1/job ' "did not pick build-and-test"
not_logged "$RUN_URL" "started a second pipeline while the gate had not run"
check "opened after the push's build finished: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}" FAKE_CCI_PIPELINES="$PIPES" FAKE_CCI_WORKFLOWS="$WFS" \
  FAKE_CCI_JOBS='{"items":[{"name":"build","status":"success"},{"name":"rust-tests","status":"success"}]}'
logged "$RUN_URL" "no new pipeline after the gate had run"
check "opened, no pipeline for the head commit: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}" FAKE_CCI_PIPELINES='{"items":[]}'
check "opened, CircleCI lookup fails: new pipeline" 0 'success: .*pipeline 4242' \
  FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}" FAKE_CCI_FAIL=1
logged "$RUN_URL" "a failed lookup did not fail open"
check "opened, no CIRCLECI_API_TOKEN: success (the push's gate ran after build)" 0 '^success:' \
  FAKE_PULLS="[$(pr 30 false dev)]"
not_logged circleci.com "called CircleCI without a token"
event opened true dev
check "opened as a draft: pending, no CircleCI call" 0 'pending: Not run: PR #30 is a draft' \
  FAKE_PULLS="[$(pr 30 true dev)]" "${CCI[@]}"
not_logged circleci.com "a draft called CircleCI"

# Fork PRs: no secrets and a read-only token; do nothing.
event ready_for_review false dev '| .pull_request.head.repo.full_name = "someone/ad4m"'
check "fork PR: nothing, exit 0" 0 'fork' FAKE_PULLS="[$(pr 30 false dev)]" "${CCI[@]}"
[ -s "$TMP/curl.log" ] && { echo "  FAIL: a fork PR made requests"; FAILED=1; }

if [ "$FAILED" != 0 ]; then
  echo "FAILED"
  exit 1
fi
echo "all passed"
