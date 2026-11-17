#!/usr/bin/env bash
# The nightly feed-scale check: WE's `test:feed-scale` (apps/we-web/tests/live/feed-scale/) against
# the executor and the SDK of one ad4m commit. A space holding FEED_SCALE_MESSAGES messages (a
# million) of WE's real message class, written through the executor's own write path, opened in
# Chromium: how long the Feed's newest page and the page before it take, how long the executor
# answers the Feed's query meanwhile, its memory, the CPU it uses over the loads (WE's watch over
# the space's messages included), and how long the seeding took. No model, no GPU.
# The nightly job `feed-scale` in .circleci/config.yml runs one step per subcommand; `all` runs
# them in order, for a run by hand.
#
#   scripts/feed-scale.sh preflight   exit 10 when this ad4m commit and this WE commit were already
#                                     measured at this size (the job then halts green); clears the
#                                     artifacts
#   scripts/feed-scale.sh executor    \
#   scripts/feed-scale.sh sdk          | coasys-spaces.sh's steps of the same names, in this job's
#   scripts/feed-scale.sh we           | own work dir: the executor for the commit, the packed SDK,
#   scripts/feed-scale.sh build-web   /  WE with that SDK, we-web built with the Coasys seed
#   scripts/feed-scale.sh test        run test:feed-scale; its log, results.json and the executor
#                                     log go to the artifacts. The executor's data (tens of GB at a
#                                     million messages) goes to <work>/tmp, emptied before and after
#   scripts/feed-scale.sh perf        hand metrics.json to the performance history:
#                                     scripts/perf/record, scripts/perf/report (the trend page),
#                                     then scripts/perf/check on a passed run (a serious
#                                     regression fails the step)
#   scripts/feed-scale.sh record      note this commit pair as measured (for preflight)
#   scripts/feed-scale.sh summary     print the run's summary line (the job log's last)
#   scripts/feed-scale.sh all         every step above, preflight forced
#
# Environment (all optional):
#   AD4M_SHA                the ad4m commit (CIRCLE_SHA1, else HEAD)
#   WE_REPO, WE_REF         WE's repository and the branch, tag or full commit (dev)
#   FEED_SCALE_WORK         work dir: WE clone, packed SDK, executor, WE's pnpm, executor data
#                           (~/.cache/ad4m-ci/feed-scale). Not /tmp, a shared tmpfs on the CI host
#   FEED_SCALE_ARTIFACTS    results.json, logs, failure screenshots (<work>/artifacts); preflight
#                           empties it
#   FEED_SCALE_STATE        the last measured commit pair (~/.local/state/ad4m-ci/feed-scale)
#   FEED_SCALE_FORCE=1      preflight never skips
#   FEED_SCALE_NO_BUILD=1   fail instead of building an executor that is not published
#   EXECUTOR_PUBLISH_DIR    the published executors (/mnt/hdd/executor-artifacts)
#   FEED_SCALE_MESSAGES     messages seeded (1000000); a run at another size is its own series
#   FEED_SCALE_*            every other knob of the suite (runs, seeding mode and tuning, page
#                           timeout) passes through to it; see the suite's README
#   PERF_HISTORY_*          where scripts/perf keeps the history (a git repository; see the header
#                           of scripts/perf/perf.mjs). PERF_ARTIFACTS_DIR: <artifacts>/perf
set -euo pipefail

REPO="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
AD4M_SHA="${AD4M_SHA:-${CIRCLE_SHA1:-$(git -C "$REPO" rev-parse HEAD)}}"
WE_REPO="${WE_REPO:-https://github.com/coasys/we.git}"
WE_REF="${WE_REF:-dev}"
WORK="${FEED_SCALE_WORK:-${XDG_CACHE_HOME:-$HOME/.cache}/ad4m-ci/feed-scale}"
ARTIFACTS="${FEED_SCALE_ARTIFACTS:-$WORK/artifacts}"
STATE="${FEED_SCALE_STATE:-${XDG_STATE_HOME:-$HOME/.local/state}/ad4m-ci/feed-scale}"
MESSAGES="${FEED_SCALE_MESSAGES:-1000000}"
JOB=feed-scale

WE_DIR="$WORK/we"
EXECUTOR="$WORK/ad4m-executor"
DATA="$WORK/tmp"
RESULTS="$ARTIFACTS/results.json"
# The same run's metrics alone, as scripts/perf/record reads them ({"metrics": [...]}).
METRICS="$ARTIFACTS/metrics.json"
WE_PACKAGE="@we/app-web"
SKIP=10

log() { echo "feed-scale: $*"; }
die() {
    echo "feed-scale: ERROR: $*" >&2
    exit 1
}

is_sha() { printf '%s' "$1" | grep -Eq '^[0-9a-f]{40}$'; }

[[ $MESSAGES =~ ^[1-9][0-9]*$ ]] || die "FEED_SCALE_MESSAGES '$MESSAGES' is not a positive whole number"

# coasys-spaces.sh's step of the same name, in this job's work dir. The steps are the same; the
# work dir is not, because the two jobs are in different serial groups and may run at once.
coasys_spaces() {
    COASYS_SPACES_WORK="$WORK" \
        COASYS_SPACES_ARTIFACTS="$ARTIFACTS" \
        COASYS_SPACES_NO_BUILD="${FEED_SCALE_NO_BUILD:-}" \
        AD4M_SHA="$AD4M_SHA" WE_REPO="$WE_REPO" WE_REF="$WE_REF" \
        bash "$REPO/scripts/coasys-spaces.sh" "$1"
}

# What a measurement is recorded under: both commits and the size.
run_key() { echo "$AD4M_SHA $1 messages=$MESSAGES"; }

cmd_preflight() {
    rm -rf "$ARTIFACTS"
    mkdir -p "$ARTIFACTS" "$WORK"
    local we_sha last changes
    rm -f "$WORK/unrecorded"
    changes="$(git -C "$REPO" status --porcelain)"
    if [ -n "$changes" ]; then
        printf '%s\n' "$changes" >"$WORK/unrecorded"
        log "the checkout has changes; this run will not be recorded:"
        printf '%s\n' "$changes" | head -20
        return 0
    fi
    # coasys-spaces.sh refuses a WE_REF that is not a name; so does this, before ls-remote.
    [[ $WE_REF =~ ^[A-Za-z0-9._/-]+$ && $WE_REF != -* ]] || die "WE_REF '$WE_REF' is not a branch, tag or commit name"
    if is_sha "$WE_REF"; then
        we_sha="$WE_REF"
    else
        we_sha="$(git ls-remote "$WE_REPO" "refs/heads/$WE_REF" "refs/tags/$WE_REF" "refs/tags/$WE_REF^{}" |
            awk '$2 ~ /\^\{\}$/ {peeled = $1} NR == 1 {first = $1} END {print (peeled != "" ? peeled : first)}' || true)"
    fi
    if [ -z "$we_sha" ]; then
        log "cannot resolve WE ref '$WE_REF' on $WE_REPO; running"
        return 0
    fi
    if [ "${FEED_SCALE_FORCE:-}" = 1 ]; then
        log "FEED_SCALE_FORCE=1; running ad4m $AD4M_SHA with WE $we_sha"
        return 0
    fi
    last="$(cat "$STATE/last-success" 2>/dev/null || true)"
    if [ "$last" = "$(run_key "$we_sha")" ]; then
        log "ad4m $AD4M_SHA with WE $we_sha already measured ($last); skipping"
        return "$SKIP"
    fi
    log "ad4m $AD4M_SHA with WE $we_sha, $MESSAGES messages (last measured: ${last:-none}); running"
}

# WE's own pnpm, as coasys-spaces.sh's steps installed it.
use_we_pnpm() {
    local bin="$WORK/pnpm/node_modules/.bin"
    [ -x "$bin/pnpm" ] || die "no pnpm in $WORK/pnpm (run the we step)"
    export PATH="$bin:$PATH"
}

# The executor's data directory: the suite's harness makes it under TMPDIR and removes it when the
# executor stops. A killed run leaves one behind, of the size of the run. One run at a time uses
# this dir (serial group nightly-feed-scale), so whatever is in it is stale.
empty_data() {
    rm -rf "$DATA"
    mkdir -p "$DATA"
}

cmd_test() {
    [ -x "$EXECUTOR" ] || die "no executor at $EXECUTOR (run the executor step)"
    # `pnpm --filter` that matches no package, or a package without the script, runs nothing and
    # exits 0.
    node -e '
      const pkg = require(process.argv[1]);
      if (pkg.name !== process.argv[2] || !pkg.scripts?.["test:feed-scale"]) process.exit(1);
    ' "$WE_DIR/apps/we-web/package.json" "$WE_PACKAGE" ||
        die "WE $WE_REF has no $WE_PACKAGE with a test:feed-scale script at apps/we-web"
    use_we_pnpm
    mkdir -p "$ARTIFACTS"
    rm -f "$RESULTS"
    {
        echo "ad4m $AD4M_SHA"
        echo "we $(cat "$WORK/we.sha" 2>/dev/null || echo "?") ($WE_REF)"
        echo "executor $(cat "$WORK/executor.source" 2>/dev/null || echo "?")"
        echo "messages $MESSAGES"
        echo "data $DATA ($(df -h --output=avail "$WORK" | tail -1 | tr -d ' ') free)"
    } | tee "$ARTIFACTS/run.txt"
    empty_data
    local rc=0
    (
        cd "$WE_DIR"
        TMPDIR="$DATA" \
            AD4M_EXECUTOR="$EXECUTOR" \
            FEED_SCALE_MESSAGES="$MESSAGES" \
            FEED_SCALE_ARTIFACTS="$ARTIFACTS" \
            pnpm --filter "$WE_PACKAGE" test:feed-scale 2>&1 | tee "$ARTIFACTS/test-feed-scale.log"
        exit "${PIPESTATUS[0]}"
    ) || rc=$?
    empty_data
    return "$rc"
}

# The suite's verdict, from results.json: passed, failed, or none when it wrote no results.
results_status() {
    [ -s "$RESULTS" ] || {
        echo none
        return
    }
    node -e 'const r = require(process.argv[1]); console.log(r.status === "passed" ? "passed" : "failed")' "$RESULTS"
}

# The performance history's own tool (scripts/perf; its contract is the header of
# scripts/perf/perf.mjs). This job keeps no history and no limit of its own: it hands over
# metrics.json, which the suite writes in the results format, and asks for the verdict. Its limits
# are the `feed-scale` rules in scripts/perf/thresholds.json.
cmd_perf() {
    local status we_sha run_id
    [ -x "$REPO/scripts/perf/record" ] && [ -x "$REPO/scripts/perf/check" ] ||
        die "no scripts/perf/record and scripts/perf/check in this checkout (the performance history tool)"
    status="$(results_status)"
    if [ "$status" = none ]; then
        log "no results.json (the suite stopped before writing one): nothing recorded"
        return 0
    fi
    [ -s "$METRICS" ] || die "results.json but no metrics.json in $ARTIFACTS (a WE suite older than metrics.json?)"
    we_sha="$(cat "$WORK/we.sha" 2>/dev/null || true)"
    # One run id for record and check. In CI it is the one record would choose itself; by hand,
    # record's own (local-<now>) is one that check cannot work out again.
    run_id="${PERF_RUN_ID:-${CIRCLE_BUILD_NUM:+circleci-$CIRCLE_BUILD_NUM}}"
    run_id="${run_id:-local-$JOB-$(date -u +%Y%m%dT%H%M%SZ)}"
    # For both tools: record writes records.jsonl here, and check reads the run from it when the
    # push to the history failed.
    export PERF_ARTIFACTS_DIR="${PERF_ARTIFACTS_DIR:-$ARTIFACTS/perf}"
    "$REPO/scripts/perf/record" --job "$JOB" --ad4m-sha "$AD4M_SHA" ${we_sha:+--we-sha "$we_sha"} \
        --run-id "$run_id" --status "$status" "$METRICS"
    # The trend page, beside the records in the artifacts. It never fails the step.
    if [ -x "$REPO/scripts/perf/report" ]; then
        "$REPO/scripts/perf/report" --job "$JOB" || log "the trend page was not drawn"
    fi
    # A failed run is recorded, not judged: it makes no baseline, and the job is red already.
    if [ "$status" = passed ]; then
        "$REPO/scripts/perf/check" --job "$JOB" --run-id "$run_id"
    fi
}

cmd_record() {
    local we_sha
    if [ -e "$WORK/unrecorded" ]; then
        log "not recorded: the checkout had changes at preflight ($(wc -l <"$WORK/unrecorded") paths)"
        return 0
    fi
    [ "$(results_status)" = passed ] || die "no passed results in $RESULTS"
    we_sha="$(cat "$WORK/we.sha" 2>/dev/null || true)"
    is_sha "$we_sha" || die "no tested WE commit in $WORK/we.sha"
    mkdir -p "$STATE"
    run_key "$we_sha" >"$STATE/last-success.tmp"
    mv "$STATE/last-success.tmp" "$STATE/last-success"
    log "recorded $(cat "$STATE/last-success") as measured"
}

cmd_summary() {
    if [ ! -s "$RESULTS" ]; then
        echo "feed-scale failed: no results ($RESULTS); the log of the step that stopped says why"
        return 0
    fi
    node -e 'console.log(require(process.argv[1]).summary ?? "feed-scale: results.json has no summary line")' "$RESULTS"
}

case "${1:-}" in
    preflight) cmd_preflight ;;
    executor | sdk | we | build-web) coasys_spaces "$1" ;;
    test) cmd_test ;;
    perf) cmd_perf ;;
    record) cmd_record ;;
    summary) cmd_summary ;;
    all)
        FEED_SCALE_FORCE=1 cmd_preflight
        coasys_spaces executor
        coasys_spaces sdk
        coasys_spaces we
        coasys_spaces build-web
        rc=0
        (cmd_test) || rc=$?
        (cmd_perf) || rc=$((rc ? rc : $?))
        if [ "$rc" = 0 ]; then cmd_record; fi
        cmd_summary
        exit "$rc"
        ;;
    *)
        awk 'NR > 1 && /^#/ { sub(/^# ?/, ""); print; next } NR > 1 { exit }' "$0" >&2
        exit 2
        ;;
esac
