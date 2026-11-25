#!/usr/bin/env bash
# The GitHub side of CircleCI's heavy_gate (.circleci/scripts/should-run-heavy.sh),
# run by .github/workflows/heavy-suites.yml on pull_request events.
#
# CircleCI runs no CI on a draft PR: every job halts green (draft_gate for the
# light jobs, heavy_gate for the heavy suites, which also halt on a stacked PR
# (base not dev) or a branch with no PR). CircleCI starts a pipeline on a push, not when a
# PR gets ready, is retargeted to dev, is reopened or gets the full-ci label, so
# such a PR kept green checks for suites that never ran. This script:
#
# - asks the same should-run-heavy.sh the same question. Skip: the commit
#   status `ci/heavy-suites` goes pending at once. A PR that is ready but
#   stacked (the heavy suites are not due) still gets its light jobs: when this
#   commit's newest pipeline halted `build` on the draft, it starts a pipeline
#   for the branch and does not wait for it.
# - Run: it watches this commit's newest CircleCI pipeline (the project is
#   public, so reads need no token) and sets success only when its heavy jobs
#   are past heavy_gate, running or done. Their ci/circleci checks stay the
#   pass or fail. While CircleCI's own pipeline builds, it waits. When that
#   pipeline halted the heavy jobs or stopped, or there is none after a grace
#   time, it starts one for the branch, unless GitHub says the branch head moved
#   on (a pipeline that checks out another commit never counts for this one),
#   and waits for that, up to HEAVY_SUITES_WAIT_SECS of wall time.
#
# Never a green without a run: every other outcome is pending, and each pending
# description names what clears it (a push, the PR getting ready, or a re-run of
# this workflow, which looks again and starts what is missing). That includes
# the ones posted while it waits: a cancel or the job's timeout can end the run
# after any of them. A failed PR
# lookup starts nothing. Without CIRCLECI_API_TOKEN it starts nothing and says
# so. A fork PR gets nothing (no secrets, read-only token, and CircleCI does not
# build fork PRs on this project).
#
#   GITHUB_EVENT_PATH, GITHUB_REPOSITORY, GITHUB_TOKEN (statuses: write),
#   GITHUB_API_URL, GITHUB_SERVER_URL, GITHUB_RUN_ID   from GitHub Actions
#   CIRCLECI_API_TOKEN               optional; a CircleCI personal API token
#   CIRCLECI_PIPELINE_DEFINITION_ID  optional; looked up when the project has one
#   CIRCLECI_PROJECT_SLUG            default the coasys/ad4m GitHub App project
#   CIRCLECI_API                     default https://circleci.com/api/v2
#   HEAVY_GATE_COMMIT_MSG            default: `git log -1 --format=%B <head sha>`
#   HEAVY_SUITES_WAIT_SECS           default 1800: how long to wait for the heavy jobs
#   HEAVY_SUITES_POLL_SECS           default 60
#   HEAVY_SUITES_GRACE_SECS          default 180: wait for CircleCI's own pipeline
#                                    to appear before starting one
#
# Test: bash .github/scripts/heavy-suites-pr.test.sh
set -uo pipefail

now() { date +%s; }
start="$(now)"
DIR="$(cd "$(dirname "$0")" && pwd)"
GATE="$DIR/../../.circleci/scripts/should-run-heavy.sh"
CONTEXT=ci/heavy-suites

ev="${GITHUB_EVENT_PATH:?GITHUB_EVENT_PATH is unset}"
repo="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY is unset}"
gh_api="${GITHUB_API_URL:-https://api.github.com}"
cci_api="${CIRCLECI_API:-https://circleci.com/api/v2}"
slug="${CIRCLECI_PROJECT_SLUG:-circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4}"

field() { jq -r "$1 // empty" "$ev"; }
action="$(field .action)"
sha="$(field .pull_request.head.sha)"
branch="$(field .pull_request.head.ref)"
number="$(field .pull_request.number)"
head_repo="$(field .pull_request.head.repo.full_name)"

if [ "$head_repo" != "$repo" ]; then
  echo "fork PR (${head_repo:-unknown} into $repo): no secrets and a read-only token here; no status, no pipeline"
  exit 0
fi
[ -n "$sha" ] && [ -n "$branch" ] || { echo "::error::no head sha or branch in the event"; exit 1; }

case "$action" in
  opened|reopened|ready_for_review|synchronize|converted_to_draft) ;;
  edited)
    if [ -z "$(field .changes.base)" ]; then echo "edited, but not a base change: nothing to do"; exit 0; fi
    ;;
  labeled|unlabeled)
    if [ "$(field .label.name)" != full-ci ]; then echo "$action, but not the full-ci label: nothing to do"; exit 0; fi
    ;;
  *) echo "pull_request $action: nothing to do"; exit 0 ;;
esac

# The gate's answer for this branch, as CircleCI would get it now.
msg="${HEAVY_GATE_COMMIT_MSG-$(git log -1 --format=%B "$sha" 2>/dev/null)}"
# One workflow id and cache dir make the gate's second question (light, below) reuse this lookup.
cache="$(mktemp -d)"
trap 'rm -rf "$cache"' EXIT
gate() { # [light]
  env -u HEAVY_GATE_FORCE -u CIRCLE_TAG CIRCLE_WORKFLOW_ID="heavy-suites-${GITHUB_RUN_ID:-0}" \
    HEAVY_GATE_CACHE_DIR="$cache/gate" CIRCLE_BRANCH="$branch" HEAVY_GATE_REPO="$repo" \
    HEAVY_GATE_API="$gh_api" HEAVY_GATE_COMMIT_MSG="$msg" bash "$GATE" "$@"
}
rc=0
line="$(gate)" || rc=$?
echo "gate: $line (exit $rc)"
case "$rc" in
  0) verdict=run ;;
  10) verdict=skip ;;
  *) verdict=unsure ;;
esac
reason="${line#*: }"
reason="${reason%%$'\n'*}"
reason="${reason%% (http*}" # the request URL, too long for a status

# The poll loop ends at the deadline. The CircleCI reads of its last poll stop
# one poll interval past it, so a slow API cannot keep the run longer than
# that plus one request (bounded by --max-time and --retry) and the final status.
wait_s="${HEAVY_SUITES_WAIT_SECS:-1800}"
poll_s="${HEAVY_SUITES_POLL_SECS:-60}"
grace_s="${HEAVY_SUITES_GRACE_SECS:-180}"
deadline=$(( start + wait_s ))
past() { [ "$(now)" -ge $(( deadline + poll_s )) ]; }

auth=()
[ -n "${CIRCLECI_API_TOKEN:-}" ] && auth=(-H "Circle-Token: $CIRCLECI_API_TOKEN")
cci() { # GET <path under /api/v2>. The project is public: reads work without a token.
  curl -fsSL --max-time 20 --retry 2 ${auth[@]+"${auth[@]}"} "$cci_api$1" 2>/dev/null
}
cci_job() { # GET a job's v1.1 details; v2 has no steps.
  curl -fsSL --max-time 20 --retry 2 ${auth[@]+"${auth[@]}"} "${cci_api%/v2}/v1.1/project/$slug/$1" 2>/dev/null
}

definition() {
  if [ -n "${CIRCLECI_PIPELINE_DEFINITION_ID:-}" ]; then
    echo "$CIRCLECI_PIPELINE_DEFINITION_ID"
    return
  fi
  local id
  id="$(cci "/project/$slug" | jq -er '.id')" || return 1
  cci "/projects/$id/pipeline-definitions" | jq -er 'if (.items | length) == 1 then .items[0].id else error end'
}

# Starts a pipeline for the branch; prints "<id> <number>". No --retry: a
# retried POST can start two.
start_pipeline() {
  local def body
  def="$(definition)" || { echo "::warning::could not pick the CircleCI pipeline definition; set the repository variable CIRCLECI_PIPELINE_DEFINITION_ID" >&2; return 2; }
  body="$(jq -cn --arg d "$def" --arg b "$branch" '{definition_id: $d, config: {branch: $b}, checkout: {branch: $b}}')"
  curl -fsSL --max-time 30 -X POST -H "Circle-Token: $CIRCLECI_API_TOKEN" -H 'Content-Type: application/json' \
    -d "$body" "$cci_api/project/$slug/pipeline/run" 2>/dev/null | jq -er 'select(.id and .number) | "\(.id) \(.number)"'
}

# A heavy job runs heavy_gate's step (found by its name, "Stack gate ..."). It
# ran when a step follows that one; it halted when the job finished with that
# step last (`circleci-agent step halt` leaves exit 0 and status success, so the
# step list is the only sign). Verified on real jobs of this project. The light
# jobs' step is "Draft gate ...": GATE_PREFIX picks which one is looked for, so
# a light job is never counted as a heavy one.
# shellcheck disable=SC2016 # jq's $g, not the shell's
JOB_STATE='if (.steps | type) != "array" then "unread" else
  ([.steps[]?.name] | map(startswith($p)) | index(true)) as $g
  | if $g == null then "light" elif $g < ((.steps | length) - 1) then "ran"
    elif .lifecycle == "finished" then "halted" else "gate" end end'
GATE_PREFIX="Stack gate"
ONLY_JOB="" # when set, workflow_state and look read only the job of this name

# workflow_state <workflow id> <workflow status>: ran, halted, waiting or stopped.
# A job whose details cannot be read may have halted: while one is unread, the
# workflow did not run the suites as far as we know (waiting), unless another
# job halted.
workflow_state() {
  local jobs num st js ran=0 unread=0
  jobs="$(cci "/workflow/$1/job")" || { echo waiting; return; }
  while read -r num st; do
    [ "$num" = null ] && continue # not started
    if [ -f "$cache/$num" ]; then
      js="$(cat "$cache/$num")"
    else
      past && { echo waiting; return; }
      js="$(cci_job "$num" | jq -r --arg p "$GATE_PREFIX" "$JOB_STATE" 2>/dev/null)" || js=""
      case "$js" in light|ran|halted|gate) ;; *) unread=1; continue ;; esac
      case "$st" in running|queued|not_running|on_hold) ;; *) echo "$js" > "$cache/$num" ;; esac
    fi
    case "$js" in
      halted) echo halted; return ;;
      ran) ran=1 ;;
    esac
  done < <(jq -r --arg only "$ONLY_JOB" '.items[] | select($only == "" or .name == $only) | "\(.job_number) \(.status)"' <<<"$jobs" 2>/dev/null)
  if [ "$unread" = 1 ]; then echo waiting; return; fi
  if [ "$ran" = 1 ]; then echo ran; return; fi
  case "$2" in running|on_hold|failing|not_run) echo waiting ;; *) echo stopped ;; esac
}

# look: the state of this commit's newest build-and-test workflow, in its
# newest pipeline, as "<state> <pipeline number>"; "none" when the commit has
# none, "unknown" when CircleCI cannot be read. Only the newest counts: its jobs
# set the ci/circleci checks GitHub shows, so an older run (passed or failed)
# says nothing about them. A re-run in CircleCI adds a workflow to the pipeline.
look() {
  local enc list id num wfs wid wst
  enc="$(jq -rn --arg b "$branch" '$b|@uri')"
  list="$(cci "/project/$slug/pipeline?branch=$enc")" || { echo unknown; return; }
  while read -r id num; do
    [ -z "$id" ] && continue
    past && { echo unknown; return; }
    wfs="$(cci "/pipeline/$id/workflow")" || { echo "waiting $num"; return; }
    read -r wid wst < <(jq -r '[.items[] | select(.name == "build-and-test")] | sort_by(.created_at)
      | last // empty | "\(.id) \(.status)"' <<<"$wfs" 2>/dev/null)
    if [ -z "${wid:-}" ]; then
      # No workflow yet: still being set up. Only other workflows: an older
      # pipeline may have ours.
      case "$(jq -r '.items | length' <<<"$wfs" 2>/dev/null)" in
        ''|0) echo "waiting $num"; return ;;
      esac
      continue
    fi
    echo "$(workflow_state "$wid" "$wst") $num"
    return
  done < <(jq -r --arg s "$sha" '[.items[] | select((.trigger_parameters.git.checkout_sha // .vcs.revision) == $s)]
    | sort_by(-.number) | .[] | "\(.id) \(.number)"' <<<"$list")
  echo none
}

# The PR's head now, or nothing when GitHub does not answer.
pr_head() {
  curl -fsSL --max-time 20 --retry 2 -H "Authorization: Bearer ${GITHUB_TOKEN:-}" \
    -H 'Accept: application/vnd.github+json' "$gh_api/repos/$repo/pulls/$number" 2>/dev/null | jq -r '.head.sha // empty'
}

last=""
post() { # <state> <description>
  local desc="${2:0:140}" body
  [ "$1 $desc" = "$last" ] && return 0
  body="$(jq -cn --arg s "$1" --arg c "$CONTEXT" --arg d "$desc" \
    --arg u "${GITHUB_SERVER_URL:-https://github.com}/$repo/actions/runs/${GITHUB_RUN_ID:-}" \
    '{state: $s, context: $c, description: $d, target_url: $u}')"
  if ! curl -fsSL --max-time 20 --retry 2 -X POST -H "Authorization: Bearer ${GITHUB_TOKEN:-}" \
    -H 'Accept: application/vnd.github+json' -d "$body" "$gh_api/repos/$repo/statuses/$sha" >/dev/null 2>&1; then
    echo "::error::could not set the $CONTEXT status on $sha"
    exit 1
  fi
  last="$1 $desc"
  echo "$1: $desc"
}

# Every pending names what clears it; this is the one that always does. A
# cancel (a newer event) or the job's timeout can end the run after any status,
# so the ones it waits on name it too.
rerun="re-run the Heavy suites workflow (Details)."
RERUN="Re-run the Heavy suites workflow (Details)."
stays="If this stays, $rerun"

if [ "$verdict" = skip ]; then
  post pending "Not run: $reason. Runs when the PR is ready and targets dev."
  # The heavy suites are not due, but a ready PR still gets the light jobs. They
  # halted if the commit's pipeline was made while the PR was a draft, and no
  # push comes when it turns ready. `build` is the light job to look at: it halts
  # exactly when the others do (one lookup per workflow). Start a pipeline then;
  # no wait, the status stays pending either way.
  lrc=0
  gate light >/dev/null || lrc=$?
  if [ "$lrc" != 10 ]; then
    read -r st got <<<"$(ONLY_JOB=build GATE_PREFIX="Draft gate" look)"
    echo "light jobs: build ${st:-unknown} ${got:+(pipeline $got)}"
    if [ "$st" = halted ]; then
      if ! head="$(pr_head)" || [ -z "$head" ] || [ "$head" = "$sha" ]; then
        if [ -z "${CIRCLECI_API_TOKEN:-}" ]; then
          echo "::warning::CIRCLECI_API_TOKEN is not set: no new CircleCI pipeline for $branch (its light jobs halted on the draft)."
        elif started="$(start_pipeline)" && [ -n "$started" ]; then
          echo "started CircleCI pipeline ${started#* } for $branch: the light jobs halted in pipeline $got while the PR was a draft"
        else
          echo "::warning::could not start a CircleCI pipeline for $branch"
        fi
      else
        echo "head of PR #$number is now $head, not $sha: the new commit gets its own pipeline"
      fi
    fi
  fi
  exit 0
fi

# The gate runs the suites (or the PR state is unknown). Success only once the
# newest pipeline of this commit has its heavy jobs past the gate; the
# ci/circleci checks of those jobs are the pass or fail. Wait for CircleCI's own
# pipeline; start one only when that halted them, or there is none. The
# loop ends by the deadline (wall time from the start, API time included), so
# the final status is posted before the job's timeout-minutes.
post pending "Looking for this commit's heavy suites in CircleCI. $stays"
mine="" mine_n="" n="" i=0
while :; do
  i=$(( i + 1 ))
  read -r st got <<<"$(look)"
  [ "$st" = unknown ] || n="$got"
  echo "poll $i: $st ${got:+(pipeline $got)}"
  case "$st" in
    ran)
      post success "Heavy suites running in CircleCI pipeline $n; their ci/circleci checks have the result."
      exit 0 ;;
    waiting)
      [ -z "$mine" ] && post pending "Waiting for CircleCI pipeline $n to reach the heavy suites. $stays" ;;
    unknown) ;;
    none|halted|stopped)
      if [ -n "$mine" ]; then
        if [ "$st" = halted ] && [ "$n" = "$mine_n" ]; then
          post pending "CircleCI pipeline $mine_n halted the heavy suites (the PR changed?). $RERUN"
          exit 0
        fi
        if [ "$st" = stopped ] && [ "$n" = "$mine_n" ]; then
          post pending "CircleCI pipeline $mine_n stopped before the heavy suites. Push a fix, or re-run it in CircleCI, then $rerun"
          exit 0
        fi
        # Not listed yet, or it did not start.
        p="$(cci "/pipeline/$mine")"
        if [ "$(jq -r '.state // empty' <<<"$p")" = errored ]; then
          post pending "CircleCI pipeline $mine_n did not start (config error?). Fix and push, or $rerun"
          exit 0
        fi
        got="$(jq -r '.trigger_parameters.git.checkout_sha // .vcs.revision // empty' <<<"$p")"
        if [ -n "$got" ] && [ "$got" != "$sha" ]; then
          echo "pipeline $mine_n checked out $got, not $sha: the branch moved; the new commit gets its own status"
          exit 0
        fi
      elif [ "$st" = stopped ]; then
        post pending "CircleCI pipeline $n stopped before the heavy suites. Push a fix, or re-run it in CircleCI, then $rerun"
        exit 0
      elif [ "$st" = none ] && [ $(( $(now) - start )) -lt "$grace_s" ]; then
        : # CircleCI's own pipeline for a push may not be listed yet.
      elif [ "$verdict" = unsure ]; then
        post pending "PR state unknown ($reason). $RERUN"
        exit 0
      elif [ "$(now)" -ge "$deadline" ]; then
        break # the wait is over: start nothing now; the re-run will
      else
        if ! head="$(pr_head)" || [ -z "$head" ]; then
          echo "could not read PR #$number's head; starting the pipeline anyway"
        elif [ "$head" != "$sha" ]; then
          echo "head of PR #$number is now $head, not $sha: nothing to start for $sha; the new commit gets its own status"
          exit 0
        fi
        if [ -z "${CIRCLECI_API_TOKEN:-}" ]; then
          echo "::warning::CIRCLECI_API_TOKEN is not set: no new CircleCI pipeline for $branch."
          post pending "Heavy suites ${n:+halted in pipeline $n, }not run; no CIRCLECI_API_TOKEN. Push a commit, or set the secret and re-run (Details)."
          exit 0
        fi
        src=0
        started="$(start_pipeline)" || src=$?
        if [ "$src" != 0 ] || [ -z "$started" ]; then
          if [ "$src" = 2 ]; then why="set CIRCLECI_PIPELINE_DEFINITION_ID"; else why="CircleCI trigger failed"; fi
          echo "::warning::no new CircleCI pipeline for $branch: $why"
          post pending "Could not start a CircleCI pipeline ($why). $RERUN"
          exit 0
        fi
        read -r mine mine_n <<<"$started"
        echo "started CircleCI pipeline $mine_n for $branch"
        post pending "Heavy suites started in CircleCI pipeline $mine_n; waiting for them. $stays"
      fi ;;
  esac
  left=$(( deadline - $(now) ))
  [ "$left" -le 0 ] && break
  sleep $(( left < poll_s ? left : poll_s ))
done

if [ "$verdict" = unsure ] && [ -z "$mine" ]; then
  post pending "PR state unknown ($reason). $RERUN"
  exit 0
fi
which="${mine_n:-${n:-}}"
if [ -n "$which" ]; then
  post pending "CircleCI pipeline $which did not reach the heavy suites in $(( wait_s / 60 )) min. $RERUN"
else
  post pending "No CircleCI pipeline of this commit reached the heavy suites in $(( wait_s / 60 )) min. $RERUN"
fi
