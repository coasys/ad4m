#!/usr/bin/env bash
# Decides whether a by-path CI job (integration-tests-sfu-e2e) runs on this push
# or halts green at its start. On a PR the job halts only when every file the
# PR's own changes touch is on the skip list given as arguments: paths known not
# to reach the job. Any other file, a new or unknown one too, runs it.
#
# Prints one line, "run: <reason>" or "skip: <reason>", and exits 0 to run or
# 10 to skip. Any other exit is a failure of this script; the caller runs the
# job then (fails open). Every git step that fails also answers "run".
#
# Runs:  CI_FULL_CI set (heavy_gate's full-ci decision: the label, [full-ci] or
#        HEAVY_GATE_FORCE, written to $BASH_ENV by should-run-heavy.sh, which
#        must therefore run first), a tag, a push to dev, staging or main (the
#        2-hourly dev tier runs the job on dev), a branch with a changed file
#        that is not on the skip list.
# Skips: a branch whose changed files are all on the skip list, a branch with
#        no changes of its own.
#
# This header is the one description of the skip list syntax (the config's
# skip_paths parameter and AGENTS.md link here).
# Skip list entries, in order; the last entry that matches a file decides:
#   dir/     every file under dir/
#   pattern  with a slash: a shell pattern on the whole path, `*` crosses `/`
#            (tests/js/tests/mcp-* or */package.json)
#   pattern  without a slash: top-level files only (*.md is README.md, not
#            rust-executor/src/mcp/docs/overview.md)
#   !entry   a file it matches is not skipped (it runs the job)
#
# "The branch's changes" is `git diff --no-renames <merge base with dev>..HEAD`:
# a file moved out of a path counts as deleted there. It fetches dev first, so a
# merge of dev into the branch brings no changes of its own.
#
#   CIRCLE_BRANCH, CIRCLE_TAG  from CircleCI
#   CI_FULL_CI             set: always run (its value is the reason)
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

[ -n "${CI_FULL_CI:-}" ] && run "full-ci: $CI_FULL_CI (heavy_gate)"
[ -n "${CIRCLE_TAG:-}" ] && run "tag ${CIRCLE_TAG}"
[ -z "$branch" ] && run "no CIRCLE_BRANCH"
case "$branch" in
  dev|staging|main) run "push to $branch" ;;
esac

timeout 120 git fetch --no-tags --quiet "$remote" "+refs/heads/$base:refs/remotes/$remote/$base" 2>/dev/null \
  || run "could not fetch $base from $remote"
mb="$(git merge-base "refs/remotes/$remote/$base" HEAD 2>/dev/null)" \
  || run "no merge base of HEAD and $remote/$base"
changed="$(git diff --name-only --no-renames "$mb" HEAD 2>/dev/null)" \
  || run "git diff failed"
[ -z "$changed" ] && skip "no changes of its own since $remote/$base (${mb:0:12})"

# skipped <file>: true when the last skip list entry that matches it is not a "!".
skipped() {
  local f="$1" entry pat neg verdict=1
  for entry in "${@:2}"; do
    neg=0
    pat="$entry"
    if [ "${pat:0:1}" = "!" ]; then neg=1; pat="${pat:1}"; fi
    # Unquoted $pat on the right of == is the pattern match this list is for.
    # shellcheck disable=SC2053
    case "$pat" in
      */) [[ "$f" == "$pat"* ]] || continue ;;
      */*) [[ "$f" == $pat ]] || continue ;;
      *) [[ "$f" != */* && "$f" == $pat ]] || continue ;;
    esac
    if [ "$neg" = 1 ]; then verdict=1; else verdict=0; fi
  done
  return "$verdict"
}

others=()
while IFS= read -r f; do
  skipped "$f" "$@" || others+=("$f")
done <<<"$changed"

if [ "${#others[@]}" -gt 0 ]; then
  run "${#others[@]} changed file(s) not on the skip list since $remote/$base (${mb:0:12}): ${others[*]:0:5}"
fi
skip "all $(wc -l <<<"$changed") changed file(s) since $remote/$base (${mb:0:12}) are on the skip list. This job runs on dev every 2 hours. To run it here: the full-ci label or [full-ci] in the commit message, then rerun."
