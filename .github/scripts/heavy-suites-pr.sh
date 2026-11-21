#!/usr/bin/env bash
# The GitHub side of CircleCI's heavy_gate (.circleci/scripts/should-run-heavy.sh),
# run by .github/workflows/heavy-suites.yml on pull_request events.
#
# heavy_gate halts the heavy suites green on a draft PR, a stacked PR (base not
# dev) or a branch with no PR. CircleCI starts a pipeline on a push, not when a
# PR gets ready, is retargeted to dev, is reopened or gets the full-ci label, so
# such a PR kept green checks for suites that never ran. This script:
#
# - sets the commit status `ci/heavy-suites` on the PR's head commit: pending
#   when the gate skips the suites there, success when they run. It asks the
#   same script the same question, so the two agree.
# - starts a new CircleCI pipeline for the head branch when the PR's state
#   changed without a push (ready_for_review, reopened, base changed, full-ci
#   label added) and the gate now runs the suites; its gate decides again.
#   Also on `opened`, when the push's pipeline already passed its gate (its
#   `build` finished) before the PR existed.
#
# Never a silent green: the status turns success only when the suites run in a
# pipeline of this commit. A failed lookup leaves it pending and, on a state
# change, still starts a pipeline (CircleCI's own gate fails open).
# Without CIRCLECI_API_TOKEN it starts no pipeline, says so in the log, and
# leaves the status pending on a state change. A fork PR gets nothing (no
# secrets, read-only token).
#
#   GITHUB_EVENT_PATH, GITHUB_REPOSITORY, GITHUB_TOKEN (statuses: write),
#   GITHUB_API_URL, GITHUB_SERVER_URL, GITHUB_RUN_ID   from GitHub Actions
#   CIRCLECI_API_TOKEN               optional; a CircleCI personal API token
#   CIRCLECI_PIPELINE_DEFINITION_ID  optional; looked up when the project has one
#   CIRCLECI_PROJECT_SLUG            default the coasys/ad4m GitHub App project
#   CIRCLECI_API                     default https://circleci.com/api/v2
#   HEAVY_GATE_COMMIT_MSG            default: `git log -1 --format=%B <head sha>`
#
# Test: bash .github/scripts/heavy-suites-pr.test.sh
set -uo pipefail

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
head_repo="$(field .pull_request.head.repo.full_name)"

if [ "$head_repo" != "$repo" ]; then
  echo "fork PR (${head_repo:-unknown} into $repo): no secrets and a read-only token here; no status, no pipeline"
  exit 0
fi
[ -n "$sha" ] && [ -n "$branch" ] || { echo "::error::no head sha or branch in the event"; exit 1; }

trigger=false
case "$action" in
  opened|reopened|ready_for_review) trigger=true ;;
  synchronize|converted_to_draft) ;;
  edited)
    if [ -z "$(field .changes.base)" ]; then echo "edited, but not a base change: nothing to do"; exit 0; fi
    trigger=true
    ;;
  labeled|unlabeled)
    if [ "$(field .label.name)" != full-ci ]; then echo "$action, but not the full-ci label: nothing to do"; exit 0; fi
    [ "$action" = labeled ] && trigger=true
    ;;
  *) echo "pull_request $action: nothing to do"; exit 0 ;;
esac

# The gate's answer for this branch, as CircleCI would get it now.
msg="${HEAVY_GATE_COMMIT_MSG-$(git log -1 --format=%B "$sha" 2>/dev/null)}"
rc=0
line="$(env -u HEAVY_GATE_FORCE -u CIRCLE_TAG -u CIRCLE_WORKFLOW_ID \
  CIRCLE_BRANCH="$branch" HEAVY_GATE_REPO="$repo" HEAVY_GATE_API="$gh_api" \
  HEAVY_GATE_COMMIT_MSG="$msg" bash "$GATE")" || rc=$?
echo "gate: $line (exit $rc)"
case "$rc" in
  0) verdict=run ;;
  10) verdict=skip ;;
  *) verdict=unsure ;;
esac
reason="${line#*: }"
reason="${reason%%$'\n'*}"
reason="${reason%% (http*}" # the request URL, too long for a status

cci() { # GET <path>
  curl -fsSL --max-time 20 --retry 2 -H "Circle-Token: $CIRCLECI_API_TOKEN" "$cci_api$1" 2>/dev/null
}

# opened: did the gate in the head commit's pipeline already run? It runs when
# `build` finishes. Yes, or unknown: a new pipeline is needed.
gate_already_ran() {
  local enc pipe wf build
  enc="$(jq -rn --arg b "$branch" '$b|@uri')"
  pipe="$(cci "/project/$slug/pipeline?branch=$enc" \
    | jq -er --arg s "$sha" 'first(.items[] | select(.vcs.revision == $s) | .id)')" \
    || { echo "no CircleCI pipeline found for $sha"; return 0; }
  wf="$(cci "/pipeline/$pipe/workflow" | jq -er 'first(.items[] | select(.name == "build-and-test") | .id) // ""')" \
    || { echo "could not read pipeline $pipe's workflows"; return 0; }
  [ -z "$wf" ] && { echo "pipeline $pipe has no build-and-test workflow yet"; return 1; }
  build="$(cci "/workflow/$wf/job" | jq -er 'first(.items[] | select(.name == "build") | .status)')" \
    || { echo "could not read the build job of workflow $wf"; return 0; }
  echo "pipeline $pipe: build is $build"
  case "$build" in
    queued|not_running|blocked|running|on_hold) return 1 ;;
  esac
  return 0
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

# Starts a pipeline for the branch; prints its number. No --retry: a retried
# POST can start two.
start_pipeline() {
  local def body
  def="$(definition)" || { echo "::warning::could not pick the CircleCI pipeline definition; set the repository variable CIRCLECI_PIPELINE_DEFINITION_ID" >&2; return 2; }
  body="$(jq -cn --arg d "$def" --arg b "$branch" '{definition_id: $d, config: {branch: $b}, checkout: {branch: $b}}')"
  curl -fsSL --max-time 30 -X POST -H "Circle-Token: $CIRCLECI_API_TOKEN" -H 'Content-Type: application/json' \
    -d "$body" "$cci_api/project/$slug/pipeline/run" 2>/dev/null | jq -er '.number // .id'
}

started="" why=""
if [ "$trigger" = true ] && [ "$verdict" != skip ]; then
  if [ -z "${CIRCLECI_API_TOKEN:-}" ]; then
    echo "::warning::CIRCLECI_API_TOKEN is not set: no new CircleCI pipeline for $branch. Rerun its CircleCI workflow by hand."
    why="no CIRCLECI_API_TOKEN secret"
  elif [ "$action" = opened ] && ! gate_already_ran; then
    trigger=false
  else
    src=0
    started="$(start_pipeline)" || src=$?
    if [ -n "$started" ] && [ "$src" = 0 ]; then
      echo "started CircleCI pipeline $started for $branch"
    else
      started=""
      if [ "$src" = 2 ]; then why="set CIRCLECI_PIPELINE_DEFINITION_ID"; else why="CircleCI trigger failed"; fi
      echo "::warning::no new CircleCI pipeline for $branch: $why"
    fi
  fi
fi

if [ "$verdict" = skip ]; then
  state=pending desc="Not run: $reason. Runs when the PR is ready and targets dev."
elif [ "$verdict" = unsure ]; then
  state=pending desc="PR state unknown ($reason); re-run this check."
elif [ -n "$started" ]; then
  state=success desc="Heavy suites run in CircleCI pipeline $started."
elif [ "$trigger" = true ] && [ "$action" = opened ] && [ -z "${CIRCLECI_API_TOKEN:-}" ]; then
  # The push's gate runs after build, minutes after the push: it normally saw this PR.
  state=success desc="Heavy suites run in this commit's CircleCI pipeline."
elif [ "$trigger" = true ] && [ -z "${CIRCLECI_API_TOKEN:-}" ]; then
  state=pending desc="Not run yet: $why. Rerun the CircleCI workflow before merging."
elif [ "$trigger" = true ]; then
  state=pending desc="Not run yet: $why. Re-run this check to try again."
else
  state=success desc="Heavy suites run in this commit's CircleCI pipeline."
fi
desc="${desc:0:140}"

body="$(jq -cn --arg s "$state" --arg c "$CONTEXT" --arg d "$desc" \
  --arg u "${GITHUB_SERVER_URL:-https://github.com}/$repo/actions/runs/${GITHUB_RUN_ID:-}" \
  '{state: $s, context: $c, description: $d, target_url: $u}')"
if ! curl -fsSL --max-time 20 --retry 2 -X POST -H "Authorization: Bearer ${GITHUB_TOKEN:-}" \
  -H 'Accept: application/vnd.github+json' -d "$body" "$gh_api/repos/$repo/statuses/$sha" >/dev/null 2>&1; then
  echo "::error::could not set the $CONTEXT status on $sha"
  exit 1
fi
echo "$state: $desc"
