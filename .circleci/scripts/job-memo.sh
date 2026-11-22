#!/usr/bin/env bash
# Remembers that a CI job passed on a set of input files, so the same job halts
# green when it meets the same inputs again: the push of dev after a merge whose
# tree a PR already tested, a merge of dev that brings only docs, a re-push that
# changes only files the job cannot read.
#
#   job-memo.sh check <job>    prints "run: <reason>" (exit 0) or
#                              "skip: <where it passed>" (exit 10)
#   job-memo.sh record <job>   stores the pass; call it as the job's last step,
#                              so it runs only when every step before it passed
#   job-memo.sh key <job>      prints the key
#
# The key is the SHA-256 of the job name and of `git ls-tree -r HEAD` (mode,
# blob hash and path of every file in the commit), without the paths the job is
# known not to read (`ignored`, below). A path that is on no list stays in the
# key, so a new directory makes every job run. .circleci/ is on no list: a
# change to the CI config or to this script makes every job run.
#
# Fails open: any exit other than 10 from `check` means "run", and a memo
# directory that is missing or not writable means "run". A job that this script
# does not know has no memo.
#
# Runs anyway: JOB_MEMO_FORCE=true (the nightly pipeline), a head commit whose
# message holds [full-ci].
#
#   JOB_MEMO_DIR          default $CI_ARTIFACT_DIR/job-memo
#   JOB_MEMO_FORCE        "true": `check` always answers run
#   JOB_MEMO_COMMIT_MSG   default: `git log -1 --format=%B`
#   JOB_MEMO_MAX_AGE_MIN  default 20160 (14 days); older entries are deleted
#   CIRCLE_BUILD_URL, CIRCLE_SHA1   from CircleCI, stored with the pass
#
# Test: bash .circleci/scripts/job-memo.test.sh
set -uo pipefail

# Path prefixes each job does not read, one per line. Add a path only with proof
# that no step of the job reads it: the Rust crates embed files from outside
# their own directories (AGENTS.md, ai-wind-tunnel/, bootstrap-languages/,
# docs-src/identity/, the deno snapshot built from core/).
ignored() {
  case "$1" in
    rust-tests|clippy)
      # docs/ is the built docs site. tests/js/ is read by the integration jobs
      # and by rust-client's tests (job rust-client-tests), not by these two.
      printf '%s\n' docs/ tests/js/ deploy/ .github/
      ;;
    *) return 1 ;;
  esac
}

mode="${1:-}"
job="${2:-}"
if [ -z "$job" ] || { [ "$mode" != check ] && [ "$mode" != record ] && [ "$mode" != key ]; }; then
  echo "usage: job-memo.sh check|record|key <job>" >&2
  exit 2
fi

run() { echo "run: $*"; exit 0; }

key() {
  local prefixes tree
  prefixes="$(ignored "$job")" || return 1
  tree="$(git ls-tree -r HEAD)" || return 1
  [ -n "$tree" ] || return 1
  {
    printf 'job %s\n' "$job"
    # ls-tree prints "<mode> <type> <hash>\t<path>".
    awk -F '\t' -v prefixes="$prefixes" '
      BEGIN { n = split(prefixes, p, "\n") }
      { for (i = 1; i <= n; i++) if (p[i] != "" && index($2, p[i]) == 1) next; print }
    ' <<<"$tree"
  } | sha256sum | cut -d' ' -f1
}

if [ "$mode" = key ]; then
  key || { echo "no key for job $job" >&2; exit 1; }
  exit 0
fi

dir="${JOB_MEMO_DIR:-${CI_ARTIFACT_DIR:-/var/lib/ci-artifacts}/job-memo}"

if [ "$mode" = check ]; then
  [ "${JOB_MEMO_FORCE:-}" = true ] && run "JOB_MEMO_FORCE=true"
  msg="${JOB_MEMO_COMMIT_MSG-$(git log -1 --format=%B 2>/dev/null)}"
  case "$msg" in
    *"[full-ci]"*) run "[full-ci] in the commit message" ;;
  esac
  k="$(key)" || run "no key for job $job"
  [ ${#k} = 64 ] || run "no key for job $job"
  entry="$dir/$job--$k"
  [ -s "$entry" ] || run "no pass on record for these inputs (key ${k:0:12})"
  echo "skip: $job passed on the same inputs: $(head -1 "$entry")"
  echo "Nothing this job reads has changed since then (key ${k:0:12}). To run it anyway, put [full-ci] in the commit message."
  exit 10
fi

# record
k="$(key)" || { echo "not recorded: no key for job $job"; exit 0; }
[ ${#k} = 64 ] || { echo "not recorded: no key for job $job"; exit 0; }
if ! mkdir -p "$dir" 2>/dev/null || ! [ -w "$dir" ]; then
  echo "not recorded: $dir is not writable"
  exit 0
fi
find "$dir" -maxdepth 1 -type f -mmin "+${JOB_MEMO_MAX_AGE_MIN:-20160}" -delete 2>/dev/null || true
entry="$dir/$job--$k"
printf '%s commit %s at %s\n' "${CIRCLE_BUILD_URL:-(no job URL)}" "${CIRCLE_SHA1:-$(git rev-parse HEAD 2>/dev/null)}" "$(date -u +%Y-%m-%dT%H:%M:%SZ)" \
  > "$entry.tmp.$$" && mv "$entry.tmp.$$" "$entry" || { echo "not recorded: could not write $entry"; exit 0; }
echo "recorded: $job passed on key ${k:0:12}"
exit 0
