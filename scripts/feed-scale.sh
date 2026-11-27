#!/usr/bin/env bash
# The nightly feed-scale check: WE's `test:feed-scale` (apps/we-web/e2e/feed-scale/) against
# the executor and the SDK of one ad4m commit. A space holding FEED_SCALE_MESSAGES messages (a
# million) of WE's real message class, generated signed into a file and loaded by the executor's
# perspective import, opened in Chromium: how long the Feed's newest page and the page before it
# take, how long the executor answers the Feed's query meanwhile, its memory, the CPU it uses over
# the loads (WE's watch over the space's messages included), and how long the import took. No
# model, no GPU.
# The nightly job `feed-scale` in .circleci/config.yml runs one step per subcommand; `all` runs
# them in order, for a run by hand.
#
#   scripts/feed-scale.sh preflight   exit 10 when this ad4m commit and this WE commit were already
#                                     measured at this size (the job then halts green); kills the
#                                     last run's leftovers, clears the artifacts and pins the
#                                     resolved WE commit (<work>/we.pinned) for the we step
#   scripts/feed-scale.sh executor    \
#   scripts/feed-scale.sh sdk          | coasys-spaces.sh's steps of the same names, in this job's
#   scripts/feed-scale.sh we           | own work dir: the executor for the commit, the packed SDK,
#   scripts/feed-scale.sh build-web   /  WE with that SDK, we-web built with the Coasys seed
#   scripts/feed-scale.sh test        run test:feed-scale; its log, results.json and the executor
#                                     log go to the artifacts. The executor's data (tens of GB at a
#                                     million messages) goes to <work>/tmp, emptied before and
#                                     after — also when the step is stopped: the suite runs in a
#                                     process group of its own so a kill reaches the executor and
#                                     the browser too; a child the launcher leaves behind is
#                                     ended too. The step clears results.json and metrics.json and
#                                     writes <artifacts>/build.id first
#   scripts/feed-scale.sh cleanup     kill what a stopped run left (the executor, a process with
#                                     its data under <work>/tmp; both paths matched as literal
#                                     text) and empty the data dir
#   scripts/feed-scale.sh perf        hand metrics.json to the performance history, only for the
#                                     results of this build's test step (build.id):
#                                     scripts/perf/record, scripts/perf/report (the trend page),
#                                     then scripts/perf/check on a passed run (a serious
#                                     regression fails the step)
#   scripts/feed-scale.sh record      note this commit pair as measured (for preflight)
#   scripts/feed-scale.sh summary     print the run's summary line (the job log's last)
#   scripts/feed-scale.sh all         every step above, preflight forced; test and perf each run as a
#                                     process of their own, and `all` fails if either fails
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
#   FEED_SCALE_MIN_FREE_GB  free GB on <work> the test step insists on before it seeds (40; a
#                           million-message store is ~12 GB on disk, and its links file ~6 GB,
#                           twice that while the file is put together)
#   FEED_SCALE_*            every other knob of the suite (runs, generation workers, page
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
MIN_FREE_GB="${FEED_SCALE_MIN_FREE_GB:-40}"
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
[[ $MIN_FREE_GB =~ ^[0-9]+$ ]] || die "FEED_SCALE_MIN_FREE_GB '$MIN_FREE_GB' is not a whole number"

# $DATA and $ARTIFACTS get rm -rf, whatever the subcommand. Refuse the values that would make
# that rm -rf disastrous — a relative path, / or $HOME itself.
guard_rm_dirs() {
    local var val norm home
    home="$(printf '%s' "$HOME" | tr -s /)"
    home="${home%/}"
    for var in WORK ARTIFACTS; do
        val="${!var}"
        case "$val" in
            /*) ;;
            *) die "$var '$val' is not an absolute path" ;;
        esac
        # A `.` or `..` part can name any directory: refuse it, do not resolve it.
        case "/$val/" in
            */./* | */../*) die "$var '$val' has a . or .. part" ;;
        esac
        # Compare the path as rm -rf sees it: repeated slashes are one slash, a trailing slash
        # is none. Nothing left is / (also //); $HOME spelled as $HOME// is $HOME.
        norm="$(printf '%s' "$val" | tr -s /)"
        norm="${norm%/}"
        if [ -z "$norm" ] || [ "$norm" = "$home" ]; then
            die "$var is '${!var}'; a run rm -rf's under it"
        fi
    done
}
guard_rm_dirs

# coasys-spaces.sh's step of the same name, in this job's work dir. The steps are the same; the
# work dir is not, because the two jobs are in different serial groups and may run at once.
coasys_spaces() {
    COASYS_SPACES_WORK="$WORK" \
        COASYS_SPACES_ARTIFACTS="$ARTIFACTS" \
        COASYS_SPACES_NO_BUILD="${FEED_SCALE_NO_BUILD:-}" \
        AD4M_SHA="$AD4M_SHA" WE_REPO="$WE_REPO" WE_REF="${2:-$WE_REF}" \
        bash "$REPO/scripts/coasys-spaces.sh" "$1"
}

# What the we step clones: the commit preflight pinned in we.pinned, or WE_REF when the steps
# run without a preflight that resolved one. coasys-spaces.sh's we takes a full sha.
we_ref() {
    local pin
    pin="$(cat "$WORK/we.pinned" 2>/dev/null || true)"
    echo "${pin:-$WE_REF}"
}

cmd_we() { coasys_spaces we "$(we_ref)"; }

# What a measurement is recorded under: both commits and the size.
run_key() { echo "$AD4M_SHA $1 messages=$MESSAGES"; }

cmd_preflight() {
    # Whatever a stopped run left behind: a live executor or suite, and its data.
    cmd_cleanup
    rm -rf "$ARTIFACTS"
    mkdir -p "$ARTIFACTS" "$WORK"
    local we_sha last changes
    rm -f "$WORK/unrecorded" "$WORK/we.pinned"
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
    # The we step clones exactly this commit: a push to WE_REF between now and then must not
    # measure another WE than the skip check's.
    printf '%s\n' "$we_sha" >"$WORK/we.pinned"
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

# A literal string as an extended regular expression (pkill -f reads its pattern as one): a `.`
# in $HOME/.cache must not match any character.
regex_quote() { printf '%s' "$1" | sed 's/[][\\.^$*+?(){}|]/\\&/g'; }

# What a stopped step can leave on a shared runner: the executor still running, a suite
# process with its data under $DATA. Killed by command line, this user's processes only, the
# two paths matched as literal text; no match is not an error. The executor's path must be the
# whole program name (`@we/e2e` starts it by that path), so a sibling binary with a longer name
# survives. Then the data dir goes.
cmd_cleanup() {
    pkill -u "$(id -u)" -f "^$(regex_quote "$EXECUTOR")( |\$)" 2>/dev/null || true
    pkill -u "$(id -u)" -f "$(regex_quote "$DATA/")" 2>/dev/null || true
    empty_data
}

# The way out of the suite in cmd_test, whether it returned or the step was stopped (a
# timeout's or a cancel's TERM): TERM the suite's whole process group — the executor and
# the browser it opened, not just pnpm — give it a few seconds, KILL it, wait for it and
# empty the data dir. The status the function had stays the step's.
suite_cleanup() {
    local rc=$?
    trap - EXIT TERM INT
    if [ -n "${suite_pid:-}" ] && kill -0 -- "-$suite_pid" 2>/dev/null; then
        # The group can be gone between the check and the kill; ESRCH must not end the
        # cleanup early under set -e.
        kill -TERM -- "-$suite_pid" 2>/dev/null || true
        local i
        for i in 1 2 3 4 5; do
            kill -0 -- "-$suite_pid" 2>/dev/null || break
            sleep 1
        done
        kill -KILL -- "-$suite_pid" 2>/dev/null || true
        wait "$suite_pid" 2>/dev/null || true
    fi
    empty_data
    exit "$rc"
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
    # Both files are this attempt's or none: perf must not pair a new verdict with the metrics of an
    # earlier attempt. The marker says the files below belong to this build (see results_status).
    rm -f "$RESULTS" "$METRICS" "$ARTIFACTS/build.id"
    build_id >"$ARTIFACTS/build.id"
    {
        echo "ad4m $AD4M_SHA"
        echo "we $(cat "$WORK/we.sha" 2>/dev/null || echo "?") ($WE_REF)"
        echo "executor $(cat "$WORK/executor.source" 2>/dev/null || echo "?")"
        echo "messages $MESSAGES"
        echo "data $DATA ($(df -h --output=avail "$WORK" | tail -1 | tr -d ' ') free)"
    } | tee "$ARTIFACTS/run.txt"
    empty_data
    # suite_pid is the script's, not the function's: the EXIT trap (suite_cleanup) runs after
    # this function returned.
    local free_kb rc=0
    # A million-message store is ~12 GB on disk, its links file up to 12 GB more; starting under
    # the minimum is a suite that dies half-way, mid-measurement.
    free_kb="$(df -k --output=avail "$WORK" | tail -1 | tr -d ' ')"
    [ "$free_kb" -ge "$((MIN_FREE_GB * 1024 * 1024))" ] ||
        die "only $((free_kb / 1024 / 1024)) GB free on $WORK; a run needs $MIN_FREE_GB GB (FEED_SCALE_MIN_FREE_GB)"
    # setsid gives the suite a session — and so a process group — of its own, so
    # suite_cleanup's kill reaches the executor and the browser, not just pnpm. -w: if setsid
    # has to fork (its caller leads a group), it still waits, so $! lives as long as the suite.
    suite_pid=
    trap suite_cleanup EXIT
    trap 'exit 143' TERM
    trap 'exit 130' INT
    TMPDIR="$DATA" \
    AD4M_EXECUTOR="$EXECUTOR" \
    FEED_SCALE_MESSAGES="$MESSAGES" \
    FEED_SCALE_ARTIFACTS="$ARTIFACTS" \
        setsid -w bash -c '
            cd "$1" || exit 1
            pnpm --filter "$2" test:feed-scale 2>&1 | tee "$3"
            st=("${PIPESTATUS[@]}")
            # pnpm when it failed, else tee: a truncated log fails the step too.
            exit $((st[0] != 0 ? st[0] : st[1]))
        ' -- "$WE_DIR" "$WE_PACKAGE" "$ARTIFACTS/test-feed-scale.log" &
    suite_pid=$!
    wait "$suite_pid" || rc=$?
    # Not disarmed: the launcher can return while the suite's group still holds a child. The
    # EXIT trap, suite_cleanup, ends the group if it is alive, empties the data dir and keeps
    # this status.
    return "$rc"
}

# What marks this build's files in the artifacts: the CI build number (none by hand) and the ad4m
# commit. The artifacts dir outlives the job on a runner, so a job that stopped before its test
# step must not read the files of the job before it.
build_id() { echo "${CIRCLE_BUILD_NUM:-local} $AD4M_SHA"; }

# The suite's verdict, from results.json: passed, failed, or none when it wrote no results, or
# when the results are not of this build's test step.
results_status() {
    [ -s "$RESULTS" ] && [ "$(cat "$ARTIFACTS/build.id" 2>/dev/null || true)" = "$(build_id)" ] || {
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
    executor | sdk | build-web) coasys_spaces "$1" ;;
    we) cmd_we ;;
    test) cmd_test ;;
    cleanup) cmd_cleanup ;;
    perf) cmd_perf ;;
    record) cmd_record ;;
    summary) cmd_summary ;;
    all)
        FEED_SCALE_FORCE=1 cmd_preflight
        coasys_spaces executor
        coasys_spaces sdk
        cmd_we
        coasys_spaces build-web
        rc=0
        # Each in its own process: inside a subshell on the left of `||` errexit is off, and a
        # failing step inside perf would pass when its last command did.
        bash "$0" test || rc=$?
        p=0
        bash "$0" perf || p=$?
        rc=$((rc ? rc : p))
        if [ "$rc" = 0 ]; then cmd_record; fi
        cmd_summary
        exit "$rc"
        ;;
    *)
        awk 'NR > 1 && /^#/ { sub(/^# ?/, ""); print; next } NR > 1 { exit }' "$0" >&2
        exit 2
        ;;
esac
