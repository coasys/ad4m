#!/usr/bin/env bash
# Decides whether a by-path CI job (integration-tests-sfu-e2e) runs on this push
# or halts green at its start. On a PR the job runs only when the PR's own
# changes touch one of the paths given as arguments (git pathspecs, so
# `tests/js/tests/sfu*` is a glob). On dev it halts: the 2-hourly dev tier
# (workflow dev-2-hourly) runs the job there.
#
# Prints one line, "run: <reason>" or "skip: <reason>", and exits 0 to run or
# 10 to skip. Any other exit is a failure of this script; the caller runs the
# job then (fails open). Every git step that fails also answers "run".
#
# Runs:  PATHS_GATE_FORCE=true, a tag, a push to staging or main, a head commit
#        whose message holds [full-ci], a branch whose changes since it left dev
#        touch a path.
# Skips: a push to dev, a branch whose changes touch none of the paths.
#
# "The branch's changes" is `git diff <merge base with dev>..HEAD`. It fetches
# dev first, so a merge of dev into the branch brings no changes of its own.
#
#   CIRCLE_BRANCH, CIRCLE_TAG  from CircleCI
#   PATHS_GATE_FORCE       "true": always run
#   PATHS_GATE_COMMIT_MSG  default: `git log -1 --format=%B`
#   PATHS_GATE_BASE        default dev
#   PATHS_GATE_REMOTE      default origin
#
# Test: bash .circleci/scripts/paths-changed.test.sh
set -uo pipefail

run() { echo "run: $*"; exit 0; }
skip() { echo "skip: $*"; exit 10; }

branch="${CIRCLE_BRANCH:-}"
base="${PATHS_GATE_BASE:-dev}"
remote="${PATHS_GATE_REMOTE:-origin}"

[ "${PATHS_GATE_FORCE:-}" = true ] && run "PATHS_GATE_FORCE=true"
[ -n "${CIRCLE_TAG:-}" ] && run "tag ${CIRCLE_TAG}"
[ -z "$branch" ] && run "no CIRCLE_BRANCH"
case "$branch" in
  dev) skip "push to dev: the 2-hourly dev tier (dev-2-hourly) runs this job" ;;
  staging|main) run "push to $branch" ;;
esac
[ $# -eq 0 ] && run "no paths given"

msg="${PATHS_GATE_COMMIT_MSG-$(git log -1 --format=%B 2>/dev/null)}"
case "$msg" in
  *"[full-ci]"*) run "[full-ci] in the commit message" ;;
esac

timeout 120 git fetch --no-tags --quiet "$remote" "+refs/heads/$base:refs/remotes/$remote/$base" 2>/dev/null \
  || run "could not fetch $base from $remote"
mb="$(git merge-base "refs/remotes/$remote/$base" HEAD 2>/dev/null)" \
  || run "no merge base of HEAD and $remote/$base"
changed="$(git diff --name-only "$mb" HEAD -- "$@" 2>/dev/null)" \
  || run "git diff failed"

if [ -n "$changed" ]; then
  n="$(wc -l <<<"$changed")"
  run "$n changed file(s) under this job's paths since $remote/$base (${mb:0:12}): $(head -5 <<<"$changed" | tr '\n' ' ')"
fi
skip "no file under $* changed since $remote/$base (${mb:0:12}). This job runs on dev every 2 hours. To run it here: [full-ci] in the commit message, then rerun."
