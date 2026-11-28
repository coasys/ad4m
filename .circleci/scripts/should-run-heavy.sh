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
# request and agree. One verdict per workflow, first writer wins: the file is
# created only if absent, and a job whose write loses answers what the file
# holds, so jobs that looked up at the same moment still answer alike. A failed lookup is the
# verdict too ("unsure", exit 11 for every later job). With no cache to share
# the verdict, a light job does not halt (exit 11). A rerun is a new workflow
# and asks again.
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

cache=""

# The cache file holds the workflow's one verdict: line 1 answers the heavy jobs,
# line 2 the light ones. A failed lookup is a verdict too ("unsure: <reason>" on
# both lines), so every job of the workflow gets the same answer. A file with
# fewer than two lines (an older format) is a miss.
read_cache() {
  [ -n "$cache" ] && [ -s "$cache" ] && [ "$(wc -l < "$cache")" -ge 2 ] || return 0
  local line
  if [ "$mode" = light ]; then line="$(sed -n 2p "$cache")"; else line="$(sed -n 1p "$cache")"; fi
  case "$line" in
    "run: "*) echo "$line (cached for this workflow)"; exit 0 ;;
    "skip: "*) echo "$line (cached for this workflow)"; exit 10 ;;
    "unsure: "*) echo "run: ${line#unsure: } (cached for this workflow)"; exit 11 ;;
  esac
}
# Creates the cache file only if it is absent (ln fails on an existing target, and
# the file appears complete): the first writer wins. When the write did not
# happen (another job's file is there, or no cache), callers read_cache and, if
# it holds no verdict, answer with their own.
write_cache() { # heavy-line light-line
  [ -n "$cache" ] || return 1
  local rc=1
  printf '%s\n%s\n' "$1" "$2" > "$cache.tmp.$$" && ln "$cache.tmp.$$" "$cache" 2>/dev/null && rc=0
  rm -f "$cache.tmp.$$"
  return $rc
}

run() { echo "run: $*"; exit 0; }
unsure() {
  write_cache "unsure: $*" "unsure: $*" || read_cache # another job may have answered first
  echo "run: $*"; exit 11
}

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

repo="${HEAVY_GATE_REPO:-}"
if [ -z "$repo" ]; then
  repo="$(printf '%s' "${CIRCLE_REPOSITORY_URL:-}" | sed -nE 's#^.*github\.com[:/]([^/]+/[^/]+)$#\1#p')"
  repo="${repo%.git}"
  repo="${repo:-coasys/ad4m}"
fi
api="${HEAVY_GATE_API:-https://api.github.com}"

cache_dir="${HEAVY_GATE_CACHE_DIR:-${CI_ARTIFACT_DIR:-/var/lib/ci-artifacts}/heavy-gate}"
if [ -n "${CIRCLE_WORKFLOW_ID:-}" ] && mkdir -p "$cache_dir" 2>/dev/null && [ -w "$cache_dir" ]; then
  find "$cache_dir" -maxdepth 1 -type f -mmin +1440 -delete 2>/dev/null || true
  cache="$cache_dir/${CIRCLE_WORKFLOW_ID}--$(printf '%s' "$branch" | tr -c 'A-Za-z0-9._-' '_')"
  read_cache
fi

command -v curl >/dev/null || unsure "curl not installed"
command -v jq >/dev/null || unsure "jq not installed"

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
if ! write_cache "$heavy_v" "$light_v"; then
  read_cache # a job that wrote first answers for the workflow
  # No shared verdict (no cache, or the write failed). A light job halts only on
  # the workflow's verdict, so its light jobs never split into ran and halted.
  case "$mode:$light_v" in
    "light:skip: "*) unsure "${light_v#skip: }, but no workflow cache to share the verdict" ;;
  esac
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
