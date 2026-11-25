#!/usr/bin/env bash
# Decides whether this CI job runs or halts green at its start. Two questions,
# one lookup:
#
#   (no argument)  the heavy suites (rust-tests, the integration suites). A stack
#                  of PRs pays them once, at the PR that targets dev, instead of
#                  once per branch per push.
#   light          every other job of a PR pipeline (build, fmt, clippy, ...).
#                  Only a draft PR halts them.
#
# Prints one line, "run: <reason>" or "skip: <reason>", and exits 0 to run or
# 10 to skip. Every lookup that fails also answers "run", with exit 11: run,
# but only because the PR state is unknown. Any other exit is a failure of this
# script. heavy_gate runs the suite on anything but 10 (fails open); the GitHub
# workflow heavy-suites.yml shows 11 as a pending status, not a green one.
#
# Runs:  HEAVY_GATE_FORCE=true (the nightly pipeline), a tag, a push to dev,
#        staging or main, a head commit whose message holds [full-ci], an open
#        PR with the `full-ci` label, an open non-draft PR whose base is dev.
# Skips: a branch with no open PR, a draft PR, a PR whose base is another
#        branch (a PR in a stack).
# `light` runs the same, except that a branch with no open PR and a ready PR
# whose base is not dev also run. It skips only when every open PR of the branch
# is a draft (and none has the full-ci label).
#
# The PR comes from the public GitHub API, unauthenticated (60 requests an hour
# per IP; every runner shares one). GITHUB_TOKEN, when set, is sent and raises
# that limit. Both answers are kept per workflow (CIRCLE_WORKFLOW_ID) in
# HEAVY_GATE_CACHE_DIR, so the jobs of one workflow, heavy or light, make one
# request and agree. A rerun is a new workflow and asks again.
#
#   CIRCLE_BRANCH, CIRCLE_TAG, CIRCLE_WORKFLOW_ID, CIRCLE_REPOSITORY_URL  from CircleCI
#   HEAVY_GATE_FORCE        "true": always run
#   HEAVY_GATE_COMMIT_MSG   default: `git log -1 --format=%B`
#   HEAVY_GATE_REPO         owner/repo; default from CIRCLE_REPOSITORY_URL, else coasys/ad4m
#   HEAVY_GATE_API          default https://api.github.com
#   HEAVY_GATE_CACHE_DIR    default $CI_ARTIFACT_DIR/heavy-gate (no cache when not writable)
#
# Test: bash .circleci/scripts/should-run-heavy.test.sh
set -uo pipefail

run() { echo "run: $*"; exit 0; }
unsure() { echo "run: $*"; exit 11; }

mode="${1:-heavy}"
case "$mode" in
  heavy|light) ;;
  *) echo "usage: should-run-heavy.sh [light]" >&2; exit 2 ;;
esac

branch="${CIRCLE_BRANCH:-}"

[ "${HEAVY_GATE_FORCE:-}" = true ] && run "HEAVY_GATE_FORCE=true"
[ -n "${CIRCLE_TAG:-}" ] && run "tag ${CIRCLE_TAG}"
[ -z "$branch" ] && run "no CIRCLE_BRANCH"
case "$branch" in
  dev|staging|main) run "push to $branch" ;;
esac

msg="${HEAVY_GATE_COMMIT_MSG-$(git log -1 --format=%B 2>/dev/null)}"
case "$msg" in
  *"[full-ci]"*) run "[full-ci] in the commit message" ;;
esac

command -v curl >/dev/null || unsure "curl not installed"
command -v jq >/dev/null || unsure "jq not installed"

repo="${HEAVY_GATE_REPO:-}"
if [ -z "$repo" ]; then
  repo="$(printf '%s' "${CIRCLE_REPOSITORY_URL:-}" | sed -nE 's#^.*github\.com[:/]([^/]+/[^/]+)$#\1#p')"
  repo="${repo%.git}"
  repo="${repo:-coasys/ad4m}"
fi
api="${HEAVY_GATE_API:-https://api.github.com}"

cache=""
cache_dir="${HEAVY_GATE_CACHE_DIR:-${CI_ARTIFACT_DIR:-/var/lib/ci-artifacts}/heavy-gate}"
if [ -n "${CIRCLE_WORKFLOW_ID:-}" ] && mkdir -p "$cache_dir" 2>/dev/null && [ -w "$cache_dir" ]; then
  find "$cache_dir" -maxdepth 1 -type f -mmin +1440 -delete 2>/dev/null || true
  cache="$cache_dir/${CIRCLE_WORKFLOW_ID}--$(printf '%s' "$branch" | tr -c 'A-Za-z0-9._-' '_')"
  # Line 1 answers the heavy jobs, line 2 the light ones.
  if [ -s "$cache" ] && [ "$(wc -l < "$cache")" -ge 2 ]; then
    if [ "$mode" = light ]; then line="$(sed -n 2p "$cache")"; else line="$(sed -n 1p "$cache")"; fi
    case "$line" in
      "run: "*) echo "$line (cached for this workflow)"; exit 0 ;;
      "skip: "*) echo "$line (cached for this workflow)"; exit 10 ;;
    esac
  fi
fi

# Open PRs whose head is this branch. A fork PR's branch is `pull/<n>`.
case "$branch" in
  pull/[0-9]*) url="$api/repos/$repo/pulls/${branch#pull/}" ;;
  *)
    owner="${repo%%/*}"
    enc="$(jq -rn --arg b "$owner:$branch" '$b|@uri')"
    url="$api/repos/$repo/pulls?state=open&per_page=100&head=$enc"
    ;;
esac
auth=()
[ -n "${GITHUB_TOKEN:-}" ] && auth=(-H "Authorization: Bearer $GITHUB_TOKEN")
body="$(curl -fsSL --max-time 20 --retry 2 -H 'Accept: application/vnd.github+json' ${auth[@]+"${auth[@]}"} "$url" 2>/dev/null)" \
  || unsure "GitHub API request failed ($url)"

# One verdict per question for all the branch's open PRs: run if any of them
# asks for it. Two lines: the heavy jobs', then the light jobs'.
verdicts="$(jq -r '
  def fullci: [.labels[]?.name] | index("full-ci");
  (if type == "object" and has("number") then [.] else . end) as $prs
  | if ($prs | type) != "array" then "run: unexpected GitHub API answer", "run: unexpected GitHub API answer"
    else ($prs | map(select((.state // "open") == "open"))) as $open
    | if ($open | length) == 0 then "skip: no open PR for this branch", "run: no open PR for this branch"
      else
        (if any($open[]; fullci) then
          "run: PR #\([$open[] | select(fullci) | .number][0]) has the full-ci label"
        elif any($open[]; .draft != true and .base.ref == "dev") then
          "run: PR #\([$open[] | select(.draft != true and .base.ref == "dev") | .number][0]) targets dev"
        else "skip: " + ([$open[] | "PR #\(.number) " + (if .draft == true then "is a draft" else "targets \(.base.ref), not dev" end)] | join(", "))
        end),
        (if any($open[]; fullci) then
          "run: PR #\([$open[] | select(fullci) | .number][0]) has the full-ci label"
        elif any($open[]; .draft != true) then
          "run: PR #\([$open[] | select(.draft != true) | .number][0]) is ready for review"
        else "skip: " + ([$open[] | "PR #\(.number) is a draft"] | join(", "))
        end)
      end
    end' <<<"$body" 2>/dev/null)"

case "$verdicts" in
  "run: unexpected"*) unsure "unexpected GitHub API answer" ;;
esac
heavy_v="$(sed -n 1p <<<"$verdicts")"
light_v="$(sed -n 2p <<<"$verdicts")"
case "$heavy_v:$light_v" in
  "run: "*:"run: "*|"run: "*:"skip: "*|"skip: "*:"run: "*|"skip: "*:"skip: "*) ;;
  *) unsure "could not read the GitHub API answer" ;;
esac
if [ -n "$cache" ]; then
  printf '%s\n%s\n' "$heavy_v" "$light_v" > "$cache.tmp.$$" && mv "$cache.tmp.$$" "$cache"
fi
if [ "$mode" = light ]; then verdict="$light_v"; else verdict="$heavy_v"; fi
echo "$verdict"
case "$verdict" in
  "skip: "*)
    if [ "$mode" = light ]; then
      echo "CI skipped on a draft PR. To run it: mark the PR ready (heavy-suites.yml then starts a new pipeline), or add the full-ci label, or put [full-ci] in the commit message."
    else
      echo "Heavy suites skipped. To run them: mark the PR ready and target dev, or add the full-ci label (heavy-suites.yml then starts a new pipeline), or put [full-ci] in the commit message."
    fi
    exit 10
    ;;
esac
exit 0
