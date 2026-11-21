#!/usr/bin/env bash
# The 2-hourly dev tier (workflow dev-2-hourly): the jobs that left the PR
# workflow run there, on dev, every 2 hours when dev has moved. This script is
# the first and the last step of each of those jobs.
#
#   dev-tier.sh moved <job>   first step. Prints "run: ..." and exits 0, or
#                             "skip: ..." and exits 10 when the latest finished
#                             run of <job> in this tier on this commit passed
#                             (the caller halts green). A red run after a green
#                             one on the same commit runs again. Any lookup
#                             failure answers "run".
#   dev-tier.sh stash <job>   before the suite. The scheduled pipeline has no
#                             `build` job: the suite uses the stash of the push
#                             pipeline's `build` of this commit. Exits 0 when the
#                             stash is there (it waits while that build runs, up
#                             to DEV_TIER_WAIT_SECS); 11 when that build is red
#                             (the caller fails without an issue: the suite
#                             cannot run); 12 "build here" when the stash is gone
#                             and no build of the commit runs (`build` deletes
#                             stashes older than 12 hours, so a red tier on a
#                             quiet dev would otherwise never run again), when
#                             no build of the commit is found, or at the
#                             deadline. A CircleCI lookup that fails counts as
#                             "may be running" until the deadline. Any other
#                             exit: the caller builds here too.
#   dev-tier.sh red <job>     last step, on failure. Opens one GitHub issue
#                             titled "dev red: <job>", or comments on it when it
#                             is open. It lists the last green commit of this
#                             tier, the first red commit and the PRs merged
#                             between them. Exits 1 when GitHub refuses (the
#                             text is in the log then).
#   dev-tier.sh green <job>   last step, on success. Comments "green again" on
#                             the open "dev red: <job>" issue, if there is one.
#                             Never opens or closes an issue, never fails.
#
# One comment per change: the suite reruns every 2 hours while dev stays red,
# but `red` comments only on a red commit the issue has not reported yet, or
# after a green; `green` only after a red. The issue's body and the script's
# comments carry a hidden marker, `<!-- dev-tier: red|green <sha> -->`; the last
# one is the state the issue shows. When the comments cannot be read, it
# comments (noise rather than silence).
#
# The issue is found by listing the repository's open issues (all pages) and
# matching the title exactly, not by search (token-based, one page, indexed late).
# A failed page means no answer: `red` then opens nothing and exits 1.
#
# "Last green" is the newest successful run of <job> in this workflow on this
# branch, from the CircleCI v1.1 API (the nightly's llm-e2e check reads the same
# list). A halted run counts as successful, but `moved` halts only on a commit
# whose latest run passed, so a halted run is always on a green commit.
#
# Issues are opened only for dev. On any other branch (a run by hand) and
# without GITHUB_TOKEN, `red` prints the text it would post and exits 0.
#
#   CIRCLE_BRANCH, CIRCLE_SHA1, CIRCLE_BUILD_URL, CIRCLE_BUILD_NUM  from CircleCI
#   CIRCLECI_API_TOKEN    read-only CircleCI token (context nightly-llm-e2e)
#   GITHUB_TOKEN          GitHub token with Issues: write on the repo
#   DEV_TIER_NO_ISSUE     set by an earlier step: the failure is not the suite's
#                         (no build stash for the commit); `red` opens nothing
#   DEV_TIER_WORKFLOW     default dev-2-hourly
#   DEV_TIER_WAIT_SECS    stash: how long to wait for a running build, default 2700
#   DEV_TIER_POLL_SECS    stash: default 30
#   DEV_TIER_NONE_GRACE_SECS  stash: how long "no build of this commit yet" waits
#                         before building here (the push pipeline may start a
#                         moment after the schedule), default 300
#   CI_ARTIFACT_DIR       stash: the stash is $CI_ARTIFACT_DIR/<sha>, default /var/lib/ci-artifacts
#   DEV_TIER_REPO         default coasys/ad4m
#   DEV_TIER_CIRCLE_API   default https://circleci.com/api/v1.1/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4
#   DEV_TIER_GITHUB_API   default https://api.github.com
#
# Test: bash .circleci/scripts/dev-tier.test.sh
set -uo pipefail

mode="${1:-}"
job="${2:-}"
case "$mode" in
  moved|stash|red|green) ;;
  *) echo "usage: dev-tier.sh moved|stash|red|green <job>" >&2; exit 2 ;;
esac
[ -z "$job" ] && { echo "usage: dev-tier.sh moved|stash|red|green <job>" >&2; exit 2; }

branch="${CIRCLE_BRANCH:-dev}"
sha="${CIRCLE_SHA1:-}"
run_url="${CIRCLE_BUILD_URL:-}"
workflow="${DEV_TIER_WORKFLOW:-dev-2-hourly}"
repo="${DEV_TIER_REPO:-coasys/ad4m}"
circle_api="${DEV_TIER_CIRCLE_API:-https://circleci.com/api/v1.1/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4}"
gh_api="${DEV_TIER_GITHUB_API:-https://api.github.com}"
title="dev red: $job"
marker_re='<!-- dev-tier: (red|green) ([0-9a-f]{40}) -->'

# Prints "<sha> <build url>" of the newest successful run of $job in $workflow on
# $branch, or nothing. Reads up to 5 pages of 100 jobs (about 25 dev pipelines).
last_green() {
  local enc offset body hit
  local auth=()
  [ -n "${CIRCLECI_API_TOKEN:-}" ] && auth=(-H "Circle-Token: $CIRCLECI_API_TOKEN")
  enc="$(jq -rn --arg b "$branch" '$b|@uri')" || return 0
  for offset in 0 100 200 300 400; do
    body="$(curl -fsSL --max-time 20 --retry 2 "${auth[@]}" \
      "$circle_api/tree/$enc?filter=successful&limit=100&shallow=true&offset=$offset" 2>/dev/null)" || return 0
    hit="$(jq -r --arg w "$workflow" --arg j "$job" '
      if type != "array" then "stop"
      elif length == 0 then "stop"
      else (first(.[] | select(.workflows.workflow_name? == $w and .workflows.job_name? == $j and (.vcs_revision // "") != "")
                 | "\(.vcs_revision) \(.build_url // "")") // "")
      end' <<<"$body" 2>/dev/null)" || return 0
    case "$hit" in
      stop) return 0 ;;
      "") ;;
      *) echo "$hit"; return 0 ;;
    esac
  done
}

# Prints "<status> <build url>" of the latest finished run of $job in $workflow
# on $branch at commit $sha, or nothing. Running and queued runs (this one among
# them) do not count. Reads up to 5 pages of 100 jobs, all statuses.
last_result() {
  local enc offset body hit
  local auth=()
  [ -n "${CIRCLECI_API_TOKEN:-}" ] && auth=(-H "Circle-Token: $CIRCLECI_API_TOKEN")
  enc="$(jq -rn --arg b "$branch" '$b|@uri')" || return 0
  for offset in 0 100 200 300 400; do
    body="$(curl -fsSL --max-time 20 --retry 2 "${auth[@]}" \
      "$circle_api/tree/$enc?limit=100&shallow=true&offset=$offset" 2>/dev/null)" || return 0
    hit="$(jq -r --arg w "$workflow" --arg j "$job" --arg s "$sha" --arg me "${CIRCLE_BUILD_NUM:-}" '
      if type != "array" then "stop"
      elif length == 0 then "stop"
      else (first(.[] | select(.workflows.workflow_name? == $w and .workflows.job_name? == $j
                               and .vcs_revision == $s and ((.build_num // "") | tostring) != $me
                               and (.lifecycle // "finished") == "finished"
                               and (.status // "") != "not_run" and (.status // "") != "")
                 | "\(.status) \(.build_url // "")") // "")
      end' <<<"$body" 2>/dev/null)" || return 0
    case "$hit" in
      stop) return 0 ;;
      "") ;;
      *) echo "$hit"; return 0 ;;
    esac
  done
}

if [ "$mode" = moved ]; then
  [ -z "$sha" ] && { echo "run: no CIRCLE_SHA1"; exit 0; }
  read -r status result_url <<<"$(last_result)"
  case "${status:-}" in
    "")
      echo "run: no finished $workflow run of $job on $branch @ ${sha:0:12} found: dev moved, or the lookup failed"
      ;;
    success|fixed)
      echo "skip: $branch @ ${sha:0:12} already passed $job in $workflow (${result_url:-}): dev has not moved"
      exit 10
      ;;
    *)
      echo "run: the latest finished $workflow run of $job on $branch @ ${sha:0:12} is $status (${result_url:-}): run it again"
      ;;
  esac
  exit 0
fi

# Prints the state of the newest `build` (workflow build-and-test) of $sha on
# $branch: "running <url>", "failed <status> <url>" (failed, timed out),
# "done <status> <url>" (any other end: success, cancelled, ...), "none" (not in
# the last 500 jobs) or "unknown" (the lookup failed).
build_state() {
  local enc offset body hit
  local auth=()
  [ -n "${CIRCLECI_API_TOKEN:-}" ] && auth=(-H "Circle-Token: $CIRCLECI_API_TOKEN")
  enc="$(jq -rn --arg b "$branch" '$b|@uri')" || { echo unknown; return; }
  for offset in 0 100 200 300 400; do
    body="$(curl -fsSL --max-time 20 --retry 2 "${auth[@]}" \
      "$circle_api/tree/$enc?limit=100&shallow=true&offset=$offset" 2>/dev/null)" || { echo unknown; return; }
    hit="$(jq -r --arg s "$sha" '
      if type != "array" then "unknown"
      elif length == 0 then "none"
      else (first(.[] | select(.workflows.workflow_name? == "build-and-test" and .workflows.job_name? == "build"
                               and .vcs_revision == $s and (.status // "") != "not_run")
                 | if (.lifecycle // "finished") != "finished" then "running \(.build_url // "")"
                   elif (.status == "failed" or .status == "timedout") then "failed \(.status) \(.build_url // "")"
                   else "done \(.status // "unknown") \(.build_url // "")"
                   end) // "")
      end' <<<"$body" 2>/dev/null)" || { echo unknown; return; }
    [ -n "$hit" ] && { echo "$hit"; return; }
  done
  echo none
}

if [ "$mode" = stash ]; then
  [ -z "$sha" ] && { echo "build here: no CIRCLE_SHA1"; exit 12; }
  dir="${CI_ARTIFACT_DIR:-/var/lib/ci-artifacts}/$sha"
  wait_secs="${DEV_TIER_WAIT_SECS:-2700}"
  poll="${DEV_TIER_POLL_SECS:-30}"
  grace="${DEV_TIER_NONE_GRACE_SECS:-300}"
  start=$SECONDS
  said=""
  while :; do
    missing=""
    for f in ad4m ad4m-executor identity-store holder integration-inputs.tar; do
      [ -s "$dir/$f" ] || missing="$missing $f"
    done
    [ -z "$missing" ] && { echo "stash: $dir has build's outputs of $sha"; exit 0; }
    read -r state rest <<<"$(build_state)"
    case "$state" in
      running) note="build of ${sha:0:12} is running (${rest:-})" ;;
      failed)
        echo "build of ${sha:0:12} failed: ${rest:-}. Not building here: the suite does not run on a red build"
        exit 11
        ;;
      done)
        echo "build here: build of ${sha:0:12} finished (${rest:-}) but its stash is gone (missing:$missing; build deletes stashes older than 12 hours)"
        exit 12
        ;;
      none)
        note="no build of ${sha:0:12} on $branch in the last 500 jobs"
        if [ $((SECONDS - start)) -ge "$grace" ]; then
          echo "build here: $note, and no stash (missing:$missing)"
          exit 12
        fi
        ;;
      *) note="cannot tell whether a build of ${sha:0:12} is running (CircleCI lookup failed)" ;;
    esac
    if [ $((SECONDS - start)) -ge "$wait_secs" ]; then
      echo "build here: waited ${wait_secs}s for the stash of ${sha:0:12} (missing:$missing); $note"
      exit 12
    fi
    [ "$said" != "$note" ] && { echo "waiting for$missing in $dir: $note"; said="$note"; }
    sleep "$poll"
  done
fi

if [ "$mode" = red ] && [ -n "${DEV_TIER_NO_ISSUE:-}" ]; then
  echo "no issue: $DEV_TIER_NO_ISSUE"
  exit 0
fi

gh_auth=()
[ -n "${GITHUB_TOKEN:-}" ] && gh_auth=(-H "Authorization: Bearer $GITHUB_TOKEN")
gh_get() { curl -fsSL --max-time 20 --retry 2 -H 'Accept: application/vnd.github+json' "${gh_auth[@]}" "$1" 2>/dev/null; }
gh_post() { curl -fsSL --max-time 20 -X POST -H 'Accept: application/vnd.github+json' "${gh_auth[@]}" --data "$2" "$1" 2>/dev/null; }

# Prints "<number> [red|green <sha>]" of the open issue titled $title (the
# marker in its body, if any), or nothing. Lists the open issues page by page
# (pull requests are in that list too; skipped). Fails when a page fails or 30
# full pages go by, so the caller never opens a second issue blindly.
open_issue() {
  local page body hit
  for page in $(seq 1 30); do
    body="$(gh_get "$gh_api/repos/$repo/issues?state=open&per_page=100&page=$page")" || return 1
    hit="$(jq -r --arg t "$title" --arg re "$marker_re" '
      if type != "array" then "error"
      else (first(.[] | select(.title == $t and (has("pull_request") | not)))
              | "\(.number) \([(.body // "") | scan($re)] | last // [] | join(" "))")
           // (if length < 100 then "end" else "" end)
      end' <<<"$body" 2>/dev/null)" || return 1
    case "$hit" in
      error) return 1 ;;
      end) return 0 ;;
      "") ;;
      *) echo "${hit% }"; return 0 ;;
    esac
  done
  return 1
}

# issue_state <number> <state from the body>: prints the issue's last marker,
# "red|green <sha>", from its body and comments (oldest first), or nothing.
# Fails when the comments cannot be read.
issue_state() {
  local state="$2" page body out
  for page in $(seq 1 30); do
    body="$(gh_get "$gh_api/repos/$repo/issues/$1/comments?per_page=100&page=$page")" || return 1
    out="$(jq -r --arg re "$marker_re" '
      if type != "array" then error("not a list")
      else "\(length) \([.[] | (.body // "") | scan($re)] | last // [] | join(" "))"
      end' <<<"$body" 2>/dev/null)" || return 1
    [ -n "${out#* }" ] && state="${out#* }"
    [ "${out%% *}" -lt 100 ] && { echo "$state"; return 0; }
  done
  echo "$state"
}

# post_comment <number> <text>: prints what happened; fails when GitHub refuses.
post_comment() {
  gh_post "$gh_api/repos/$repo/issues/$1/comments" "$(jq -cn --arg b "$2" '{body:$b}')" >/dev/null
}

if [ "$mode" = green ]; then
  [ "$branch" != dev ] && { echo "not dev ($branch): nothing to note"; exit 0; }
  [ -z "${GITHUB_TOKEN:-}" ] && { echo "GITHUB_TOKEN is not set: nothing to note"; exit 0; }
  found="$(open_issue)" || { echo "could not list GitHub issues; nothing noted"; exit 0; }
  [ -z "$found" ] && { echo "no open issue \"$title\""; exit 0; }
  n="${found%% *}"
  body_state=""
  [ "$found" != "$n" ] && body_state="${found#* }"
  if state="$(issue_state "$n" "$body_state")"; then
    case "$state" in
      green\ *) echo "no comment: #$n already says green (at ${state#green }); nothing changed"; exit 0 ;;
    esac
  fi
  text="Green again at $sha ([run]($run_url)). If this commit is one of the red ones above, the failure was flaky: label this issue \`flaky\`. Close the issue when the cause is fixed or reverted.

<!-- dev-tier: green $sha -->"
  if post_comment "$n" "$text"; then
    echo "commented on #$n"
  else
    echo "could not comment on #$n"
  fi
  exit 0
fi

# --- red
read -r green green_url <<<"$(last_green)"
green="${green:-}"

# The commits on dev's first-parent line between the last green and this commit,
# one line each: the PR with its title and author when the subject names one.
changes() {
  if [ -z "$green" ]; then
    echo "No green run of this tier on record, so there is no range to list."
    return
  fi
  if [ "$green" = "$sha" ]; then
    echo "This commit passed this tier before: probably a flaky failure."
    return
  fi
  if ! git cat-file -e "$green^{commit}" 2>/dev/null || ! git cat-file -e "$sha^{commit}" 2>/dev/null; then
    echo "The last green commit is not in this checkout; see the compare link."
    return
  fi
  local short subj n pr count=0
  while IFS=$'\t' read -r short subj; do
    count=$((count + 1))
    if [ "$count" -gt 50 ]; then
      echo "- ... more; see the compare link"
      break
    fi
    n=""
    if [[ "$subj" =~ ^Merge\ pull\ request\ \#([0-9]+) ]]; then n="${BASH_REMATCH[1]}"
    elif [[ "$subj" =~ \(\#([0-9]+)\)$ ]]; then n="${BASH_REMATCH[1]}"
    fi
    if [ -z "$n" ]; then
      echo "- $short $subj"
    elif pr="$(gh_get "$gh_api/repos/$repo/pulls/$n" | jq -er '"\(.title) (@\(.user.login))"' 2>/dev/null)"; then
      echo "- #$n $pr"
    else
      echo "- #$n $subj"
    fi
  done < <(git log --first-parent --format='%h%x09%s' "$green..$sha" 2>/dev/null)
}

list="$(changes)"
if [ -n "$green" ]; then
  green_line="$green${green_url:+ ([run]($green_url))}"
  compare="- Changes: https://github.com/$repo/compare/$green...$sha"
else
  green_line="none on record"
  compare=""
fi

# TODO: assign the authors of the listed PRs (CI-PLAN section 3, step 2) once
# Nico agrees. Until then the issue names them and assigns nobody.
body="The 2-hourly dev tier job \`$job\` (workflow \`$workflow\`) failed on dev.

- First red commit: $sha ([run]($run_url))
- Last green commit of this tier: $green_line${compare:+
$compare}

PRs and commits merged between them (first-parent line of dev):

$list

Flaky? Rerun the job on the same commit. If it goes green, label this issue \`flaky\`.
Close this issue when dev is green again because the cause was fixed or reverted.

<!-- dev-tier: red $sha -->"

# comment_text <Still red|Red again>
comment_text() {
  echo "$1 at $sha ([run]($run_url)).

- Last green commit of this tier: $green_line${compare:+
$compare}

PRs and commits merged between them:

$list

<!-- dev-tier: red $sha -->"
}

show() { echo "----"; echo "$1"; echo "----"; }

if [ "$branch" != dev ]; then
  echo "not dev ($branch): no issue opened. It would say:"
  show "$body"
  exit 0
fi
if [ -z "${GITHUB_TOKEN:-}" ]; then
  echo "GITHUB_TOKEN is not set: no issue opened. It would say:"
  show "$body"
  exit 0
fi

if ! found="$(open_issue)"; then
  echo "could not list GitHub issues to find \"$title\"; no issue opened. It would say:"
  show "$body"
  exit 1
fi
if [ -n "$found" ]; then
  n="${found%% *}"
  body_state=""
  [ "$found" != "$n" ] && body_state="${found#* }"
  head="Still red"
  if state="$(issue_state "$n" "$body_state")"; then
    case "$state" in
      "red $sha") echo "no comment: #$n already reports red at ${sha:0:12}; the next comment comes with another red commit or a change of state"; exit 0 ;;
      green\ *) head="Red again" ;;
    esac
  else
    echo "could not read the comments of #$n; commenting anyway"
  fi
  comment="$(comment_text "$head")"
  if post_comment "$n" "$comment"; then
    echo "commented on #$n"
    exit 0
  fi
  echo "could not comment on #$n. It would say:"
  show "$comment"
  exit 1
fi
if answer="$(gh_post "$gh_api/repos/$repo/issues" "$(jq -cn --arg t "$title" --arg b "$body" '{title:$t, body:$b}')")"; then
  echo "opened $(jq -r '.html_url // .number' <<<"$answer" 2>/dev/null)"
  exit 0
fi
echo "could not open the issue \"$title\". It would say:"
show "$body"
exit 1
