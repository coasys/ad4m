#!/usr/bin/env bash
# Tests for the decisions in scripts/feed-scale.sh that need no build, no browser and no network:
# skipping a commit pair already measured at this size, the steps it borrows from coasys-spaces.sh
# running in its own work dir, the suite run with its data off /tmp and its exit status kept, and
# what it hands to the performance history (scripts/perf/record and check, faked here). A fake WE
# whose test:feed-scale writes results; about a second.
# Run: bash scripts/feed-scale.test.sh

set -uo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
FAILED=0
SHA=0123456789abcdef0123456789abcdef01234567
WE_SHA=89abcdef0123456789abcdef0123456789abcdef

fail() {
    echo "  FAIL: $*"
    FAILED=1
}

# A clean git repository standing in for the ad4m checkout, with both scripts and a fake
# scripts/perf that logs how it was called; the script's environment pointed into $TMP.
setup() {
    TMP="$(mktemp -d)"
    mkdir -p "$TMP/ad4m/scripts/perf"
    cp "$HERE/feed-scale.sh" "$HERE/coasys-spaces.sh" "$TMP/ad4m/scripts/"
    # Each call is logged as its arguments, and the PERF_ARTIFACTS_DIR it saw as a line of its own.
    for tool in record check report; do
        printf '#!/usr/bin/env bash\necho "%s $*" >>"%s/perf-calls"\necho "%s sees PERF_ARTIFACTS_DIR=$PERF_ARTIFACTS_DIR" >>"%s/perf-env"\nexit "${FAKE_PERF_%s_EXIT:-0}"\n' \
            "$tool" "$TMP" "$tool" "$TMP" "${tool^^}" >"$TMP/ad4m/scripts/perf/$tool"
        chmod +x "$TMP/ad4m/scripts/perf/$tool"
    done
    # The CI job that runs these tests has its own build number; a run by hand has none.
    unset CIRCLE_BUILD_NUM PERF_RUN_ID PERF_ARTIFACTS_DIR
    git init -q "$TMP/ad4m"
    git -C "$TMP/ad4m" add -A
    git -C "$TMP/ad4m" -c user.name=t -c user.email=t@t commit -q -m ad4m
    SCRIPT="$TMP/ad4m/scripts/feed-scale.sh"
    export AD4M_SHA="$SHA"
    export FEED_SCALE_WORK="$TMP/work"
    export FEED_SCALE_ARTIFACTS="$TMP/artifacts"
    export FEED_SCALE_STATE="$TMP/state"
    export EXECUTOR_PUBLISH_DIR="$TMP/publish"
    export FEED_SCALE_NO_BUILD=1
    unset FEED_SCALE_FORCE FEED_SCALE_MESSAGES WE_REF COASYS_SPACES_WORK COASYS_SPACES_ARTIFACTS FAKE_SUITE
    # CircleCI's BASH_ENV re-exports PATH in every non-interactive bash (the suite's `bash -c`
    # too), which would drop the fakes a test puts first in PATH.
    unset BASH_ENV
    # The fakes write a few KB: the disk guard is off, except in its own test (the CI host's
    # temp dir has less than the default 40 GB free).
    export FEED_SCALE_MIN_FREE_GB=0
    mkdir -p "$EXECUTOR_PUBLISH_DIR"
}

# A local repository stands in for WE; $WE_HEAD is its dev.
make_we_remote() {
    export WE_REPO="$TMP/we.git"
    git init -q -b dev "$WE_REPO"
    git -C "$WE_REPO" -c user.name=t -c user.email=t@t commit -q --allow-empty -m one
    WE_HEAD="$(git -C "$WE_REPO" rev-parse HEAD)"
}

# What the we and executor steps leave: an executor, WE's pnpm (a fake that runs the script with
# node, as `pnpm --filter <pkg> <script>` would), and a WE whose test:feed-scale is a small Node
# program. FAKE_SUITE says what it does: pass, fail (results written, exit 1), or crash (no results).
make_installed_we() {
    local work="$FEED_SCALE_WORK"
    mkdir -p "$work/we/apps/we-web" "$work/pnpm/node_modules/.bin"
    printf '#!/bin/sh\nexit 0\n' >"$work/ad4m-executor"
    chmod +x "$work/ad4m-executor"
    echo "$WE_SHA" >"$work/we.sha"
    echo published >"$work/executor.source"
    cat >"$work/pnpm/node_modules/.bin/pnpm" <<'EOF'
#!/usr/bin/env bash
# pnpm --filter <package> <script>: runs the fake suite from apps/we-web.
[ "$1" = --filter ] || exit 9
cd apps/we-web && exec node suite.mjs
EOF
    chmod +x "$work/pnpm/node_modules/.bin/pnpm"
    echo '{"name": "@we/app-web", "scripts": {"test:feed-scale": "node suite.mjs"}}' >"$work/we/apps/we-web/package.json"
    cat >"$work/we/apps/we-web/suite.mjs" <<'EOF'
import { writeFileSync, mkdtempSync, readdirSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { join } from 'node:path';
import { spawn } from 'node:child_process';
const mode = process.env.FAKE_SUITE ?? 'pass';
// What the harness does: a data dir under TMPDIR.
mkdtempSync(join(tmpdir(), 'we-live-web-'));
writeFileSync(join(process.env.FEED_SCALE_ARTIFACTS, 'env.json'), JSON.stringify({
  TMPDIR: process.env.TMPDIR, AD4M_EXECUTOR: process.env.AD4M_EXECUTOR,
  FEED_SCALE_MESSAGES: process.env.FEED_SCALE_MESSAGES, FEED_SCALE_RUNS: process.env.FEED_SCALE_RUNS,
  data: readdirSync(process.env.TMPDIR),
}));
if (mode === 'crash') process.exit(3);
if (mode === 'orphan') {
  // The launcher returns, a child of its group keeps running.
  const child = spawn('sleep', ['300'], { stdio: 'ignore' });
  writeFileSync(join(process.env.FEED_SCALE_ARTIFACTS, 'suite-child.pid'), String(child.pid));
  process.exit(7);
}
if (mode === 'hang') {
  // A run stopped mid-suite: a marker in the data dir and a child in the same process
  // group. The test kills the step and checks both are gone.
  writeFileSync(join(tmpdir(), 'left.txt'), 'x');
  const child = spawn('sleep', ['300']);
  writeFileSync(join(process.env.FEED_SCALE_ARTIFACTS, 'suite-child.pid'), String(child.pid));
  writeFileSync(join(process.env.FEED_SCALE_ARTIFACTS, 'suite-up'), '');
  await new Promise(() => {});
}
const status = mode === 'pass' ? 'passed' : 'failed';
const metrics = [{ metric: 'feed.newest_page.median', value: 1, unit: 'ms', better: 'lower', params: {} }];
writeFileSync(join(process.env.FEED_SCALE_ARTIFACTS, 'results.json'), JSON.stringify({
  job: 'feed-scale', status, summary: `feed-scale ${status}: messages=${process.env.FEED_SCALE_MESSAGES}`,
  metrics,
}));
if (mode !== 'no-metrics') writeFileSync(join(process.env.FEED_SCALE_ARTIFACTS, 'metrics.json'), JSON.stringify({ metrics }));
console.log(`suite ${status}`);
process.exit(status === 'passed' ? 0 : 1);
EOF
}

# $1: the expected exit status of preflight.
preflight_exits() {
    local rc=0
    out="$(bash "$SCRIPT" preflight 2>&1)" || rc=$?
    [ "$rc" = "$1" ] || fail "preflight exited $rc, not $1: $out"
}

test_preflight_skips_a_measured_pair() {
    make_we_remote
    make_installed_we
    echo "$WE_HEAD" >"$FEED_SCALE_WORK/we.sha"
    mkdir -p "$FEED_SCALE_ARTIFACTS" && touch "$FEED_SCALE_ARTIFACTS/old.png"
    preflight_exits 0
    [ ! -e "$FEED_SCALE_ARTIFACTS/old.png" ] || fail "preflight left the previous run's artifacts"

    bash "$SCRIPT" test >/dev/null 2>&1 || fail "the fake suite did not pass"
    bash "$SCRIPT" record >/dev/null || fail "record failed"
    [ "$(cat "$FEED_SCALE_STATE/last-success")" = "$SHA $WE_HEAD messages=1000000" ] ||
        fail "record wrote the wrong key: $(cat "$FEED_SCALE_STATE/last-success")"

    preflight_exits 10
    FEED_SCALE_FORCE=1 preflight_exits 0
    AD4M_SHA=1111111111111111111111111111111111111111 preflight_exits 0
    # Another size is another measurement.
    FEED_SCALE_MESSAGES=100000 preflight_exits 0
    WE_REF="$WE_HEAD" preflight_exits 10
    WE_REF=no-such-branch preflight_exits 0
    git -C "$WE_REPO" -c user.name=t -c user.email=t@t commit -q --allow-empty -m two
    preflight_exits 0
}

test_record_needs_a_passed_run() {
    make_installed_we
    out="$(bash "$SCRIPT" record 2>&1)" && fail "record passed with no results: $out"
    FAKE_SUITE=fail bash "$SCRIPT" test >/dev/null 2>&1
    out="$(bash "$SCRIPT" record 2>&1)" && fail "record passed on a failed run: $out"
    [ ! -e "$FEED_SCALE_STATE/last-success" ] || fail "a failed run was recorded as measured"
}

test_dirty_checkout_is_not_recorded() {
    make_we_remote
    make_installed_we
    echo '# an edit' >>"$SCRIPT"
    preflight_exits 0
    grep -qF "will not be recorded" <<<"$out" || fail "preflight does not say the run is not recorded: $out"
    bash "$SCRIPT" test >/dev/null 2>&1
    bash "$SCRIPT" record >/dev/null 2>&1 || fail "record failed on a dirty checkout"
    [ ! -e "$FEED_SCALE_STATE/last-success" ] || fail "a dirty checkout was recorded as measured"
}

test_suite_runs_with_its_data_in_the_work_dir() {
    make_installed_we
    mkdir -p "$FEED_SCALE_WORK/tmp/we-live-web-stale"
    out="$(FEED_SCALE_MESSAGES=10000 FEED_SCALE_RUNS=2 bash "$SCRIPT" test 2>&1)" || fail "test failed: $out"
    local env="$FEED_SCALE_ARTIFACTS/env.json"
    [ "$(jq -r .TMPDIR "$env")" = "$FEED_SCALE_WORK/tmp" ] || fail "the suite's TMPDIR is $(jq -r .TMPDIR "$env")"
    [ "$(jq -r .AD4M_EXECUTOR "$env")" = "$FEED_SCALE_WORK/ad4m-executor" ] || fail "the suite ran another executor"
    [ "$(jq -r .FEED_SCALE_MESSAGES "$env")" = 10000 ] || fail "FEED_SCALE_MESSAGES did not reach the suite"
    [ "$(jq -r .FEED_SCALE_RUNS "$env")" = 2 ] || fail "FEED_SCALE_RUNS did not pass through"
    # The stale dir was gone before the suite started; the suite's own was gone after it ended.
    # (Node may add its own entries there, as `node-compile-cache` on the CI host.)
    [ "$(jq -r '.data | index("we-live-web-stale")' "$env")" = null ] || fail "the data dir was not emptied first: $(jq -c .data "$env")"
    [ -z "$(ls -A "$FEED_SCALE_WORK/tmp")" ] || fail "the data dir was left with: $(ls "$FEED_SCALE_WORK/tmp")"
    grep -qF "suite passed" "$FEED_SCALE_ARTIFACTS/test-feed-scale.log" || fail "the suite's log is not in the artifacts"
    grep -qF "messages 10000" "$FEED_SCALE_ARTIFACTS/run.txt" || fail "run.txt does not say the size"
}

test_suite_failure_fails_the_step() {
    make_installed_we
    FAKE_SUITE=fail bash "$SCRIPT" test >/dev/null 2>&1 && fail "a failing suite passed the step"
    FAKE_SUITE=crash bash "$SCRIPT" test >/dev/null 2>&1 && fail "a crashing suite passed the step"
    [ -z "$(ls -A "$FEED_SCALE_WORK/tmp")" ] || fail "a failed run left its data"
}

test_a_stopped_run_is_cleaned_up() {
    make_installed_we
    FAKE_SUITE=hang bash "$SCRIPT" test >/dev/null 2>&1 &
    local tpid=$! i
    for i in $(seq 100); do [ -e "$FEED_SCALE_ARTIFACTS/suite-up" ] && break; sleep 0.1; done
    [ -e "$FEED_SCALE_ARTIFACTS/suite-up" ] || fail "the suite did not start"
    [ -e "$FEED_SCALE_WORK/tmp/left.txt" ] || fail "the suite left no file in the data dir"
    kill -TERM "$tpid"
    local rc=0
    wait "$tpid" || rc=$?
    [ "$rc" -ne 0 ] || fail "a killed step exited 0"
    [ -z "$(ls -A "$FEED_SCALE_WORK/tmp")" ] || fail "a killed run left its data: $(ls -A "$FEED_SCALE_WORK/tmp")"
    # The group's signal can leave the child a zombie until its parent is reaped: a zombie
    # (state Z) or a missing process is stopped; only a running one survived.
    local child state=
    child="$(cat "$FEED_SCALE_ARTIFACTS/suite-child.pid")"
    for i in $(seq 50); do
        state="$(ps -o stat= -p "$child" 2>/dev/null | tr -d ' ' || true)"
        case "$state" in '' | Z*) break ;; esac
        sleep 0.1
    done
    case "$state" in
        '' | Z*) ;;
        *) fail "a process from the suite's group survived (state $state)" ;;
    esac
}

# The suite's launcher returns while a child of its group still runs: the step must end the
# group before it empties the data dir, and keep the suite's status.
test_a_child_left_by_a_finished_suite_is_stopped() {
    make_installed_we
    local rc=0 child state=
    FAKE_SUITE=orphan bash "$SCRIPT" test >/dev/null 2>&1 || rc=$?
    [ "$rc" = 7 ] || fail "the suite's status 7 was not kept: $rc"
    child="$(cat "$FEED_SCALE_ARTIFACTS/suite-child.pid")"
    for i in $(seq 50); do
        state="$(ps -o stat= -p "$child" 2>/dev/null | tr -d ' ' || true)"
        case "$state" in '' | Z*) break ;; esac
        sleep 0.1
    done
    case "$state" in
        '' | Z*) ;;
        *) fail "the child of the finished suite is still running (state $state)" ;;
    esac
    [ -z "$(ls -A "$FEED_SCALE_WORK/tmp")" ] || fail "the data dir was left with: $(ls "$FEED_SCALE_WORK/tmp")"
}

# cleanup kills by command line: the paths are matched as text, so a `.` in them is no wildcard.
test_cleanup_kills_only_this_runs_processes() {
    make_installed_we
    local mine other
    bash -c 'while :; do sleep 1; done' "$FEED_SCALE_WORK/tmp/we-live-web-x" &
    mine=$!
    # The same path with its first `.` replaced by `X`.
    local decoy="${FEED_SCALE_WORK/./X}/tmp/we-live-web-x"
    [ "$decoy" != "$FEED_SCALE_WORK/tmp/we-live-web-x" ] || fail "the work dir has no dot to turn into X: $FEED_SCALE_WORK"
    bash -c 'while :; do sleep 1; done' "$decoy" &
    other=$!
    sleep 0.3
    bash "$SCRIPT" cleanup >/dev/null 2>&1 || fail "cleanup failed"
    local i
    for i in $(seq 50); do kill -0 "$mine" 2>/dev/null || break; sleep 0.1; done
    if kill -0 "$mine" 2>/dev/null; then
        fail "cleanup left a process with the data dir in its command line"
        kill "$mine" 2>/dev/null
    fi
    if kill -0 "$other" 2>/dev/null; then
        kill "$other" 2>/dev/null
    else
        fail "cleanup killed a process whose path only looks like the work dir ($decoy)"
    fi
    wait 2>/dev/null
}

# The executor is matched as the whole program name: a binary whose name only starts with it lives.
test_cleanup_kills_the_executor_not_a_longer_named_sibling() {
    make_installed_we
    local exe="$FEED_SCALE_WORK/ad4m-executor" mine other i
    # `exec -a` makes the path the program name, so the command line starts with it, as the
    # executor's does.
    bash -c 'exec -a "$0" sleep 600' "$exe" &
    mine=$!
    bash -c 'exec -a "$0" sleep 600' "$exe-debug" &
    other=$!
    sleep 0.3
    bash "$SCRIPT" cleanup >/dev/null 2>&1 || fail "cleanup failed"
    for i in $(seq 50); do kill -0 "$mine" 2>/dev/null || break; sleep 0.1; done
    if kill -0 "$mine" 2>/dev/null; then
        fail "cleanup left the executor running"
        kill "$mine" 2>/dev/null
    fi
    if kill -0 "$other" 2>/dev/null; then
        kill "$other" 2>/dev/null
    else
        fail "cleanup killed $exe-debug, whose name only starts with the executor's"
    fi
    wait 2>/dev/null
}

test_a_failing_tee_fails_the_step() {
    make_installed_we
    # A tee that writes what it is given but fails on the suite log, as ENOSPC would.
    mkdir -p "$TMP/bin"
    cat >"$TMP/bin/tee" <<'EOF'
#!/usr/bin/env bash
cat >"$1"
case "$1" in
    */test-feed-scale.log) exit 1 ;;
esac
EOF
    chmod +x "$TMP/bin/tee"
    PATH="$TMP/bin:$PATH" bash "$SCRIPT" test >/dev/null 2>&1 && fail "a failing tee passed the step"
    grep -qF "suite passed" "$FEED_SCALE_ARTIFACTS/test-feed-scale.log" || fail "the suite's log is not in the artifacts"
}

test_the_disk_guard_refuses_below_the_minimum() {
    make_installed_we
    out="$(FEED_SCALE_MIN_FREE_GB=99999999 bash "$SCRIPT" test 2>&1)" && fail "the step ran below the free-space minimum"
    grep -qF "FEED_SCALE_MIN_FREE_GB" <<<"$out" || fail "the reason is not given: $out"
    [ ! -e "$FEED_SCALE_ARTIFACTS/results.json" ] || fail "the suite ran below the minimum"
}

test_the_we_step_clones_the_pinned_commit() {
    make_we_remote
    preflight_exits 0
    [ "$(cat "$FEED_SCALE_WORK/we.pinned")" = "$WE_HEAD" ] ||
        fail "preflight pinned $(cat "$FEED_SCALE_WORK/we.pinned" 2>/dev/null || echo nothing)"
    # A pin that preflight cannot resolve does not survive the next preflight.
    echo stale >"$FEED_SCALE_WORK/we.pinned"
    WE_REF=no-such-branch preflight_exits 0
    [ ! -e "$FEED_SCALE_WORK/we.pinned" ] || fail "a stale pin survived a preflight"
    # A push to the ref between preflight and the we step does not change what is cloned.
    preflight_exits 0
    git -C "$WE_REPO" -c user.name=t -c user.email=t@t commit -q --allow-empty -m two
    printf '#!/usr/bin/env bash\necho "coasys-spaces $1 WE_REF=$WE_REF"\n' >"$TMP/ad4m/scripts/coasys-spaces.sh"
    out="$(bash "$SCRIPT" we)"
    grep -qxF "coasys-spaces we WE_REF=$WE_HEAD" <<<"$out" || fail "the we step did not use the pin: $out"
}

test_rm_rf_targets_are_guarded() {
    out="$(FEED_SCALE_WORK=relative bash "$SCRIPT" summary 2>&1)" && fail "a relative WORK was taken"
    grep -qF "not an absolute path" <<<"$out" || fail "the reason is not given: $out"
    out="$(FEED_SCALE_WORK=/ bash "$SCRIPT" summary 2>&1)" && fail "WORK=/ was taken"
    out="$(FEED_SCALE_WORK="$HOME" bash "$SCRIPT" summary 2>&1)" && fail "WORK=\$HOME was taken"
    out="$(FEED_SCALE_ARTIFACTS=relative bash "$SCRIPT" summary 2>&1)" && fail "a relative ARTIFACTS was taken"
    # Other spellings of the same directories. The guard dies before any rm -rf, so nothing runs
    # against the real $HOME here.
    local spelling
    for spelling in "$HOME//" "$HOME/" "$HOME///" "$HOME/./" "$HOME/x/.." // /// /. /..; do
        out="$(FEED_SCALE_ARTIFACTS="$spelling" bash "$SCRIPT" summary 2>&1)" && fail "ARTIFACTS='$spelling' was taken"
        out="$(FEED_SCALE_WORK="$spelling" bash "$SCRIPT" summary 2>&1)" && fail "WORK='$spelling' was taken"
    done
    # A directory below $HOME is fine.
    out="$(FEED_SCALE_ARTIFACTS="$HOME//x-guard-test/" bash "$SCRIPT" summary 2>&1)" || fail "a directory below HOME was refused: $out"
}

test_test_needs_the_script_in_we() {
    make_installed_we
    echo '{"name": "@we/app-web", "scripts": {}}' >"$FEED_SCALE_WORK/we/apps/we-web/package.json"
    out="$(bash "$SCRIPT" test 2>&1)" && fail "test passed without test:feed-scale in WE"
    grep -qF "no @we/app-web with a test:feed-scale script" <<<"$out" || fail "the reason is not given: $out"
}

test_perf_records_and_checks_a_passed_run() {
    make_installed_we
    bash "$SCRIPT" test >/dev/null 2>&1
    out="$(bash "$SCRIPT" perf 2>&1)" || fail "perf failed: $out"
    local run_id
    run_id="$(sed -n 's/^record .*--run-id \([^ ]*\) .*/\1/p' "$TMP/perf-calls")"
    [[ $run_id =~ ^local-feed-scale-[0-9]{8}T[0-9]{6}Z$ ]] || fail "a run by hand has no run id of its own: $(cat "$TMP/perf-calls")"
    grep -qxF "record --job feed-scale --ad4m-sha $SHA --we-sha $WE_SHA --run-id $run_id --status passed $FEED_SCALE_ARTIFACTS/metrics.json" "$TMP/perf-calls" ||
        fail "record was not called as expected: $(cat "$TMP/perf-calls")"
    grep -qxF "report --job feed-scale" "$TMP/perf-calls" || fail "report was not called: $(cat "$TMP/perf-calls")"
    # check judges the run record wrote, and finds it in records.jsonl when the push failed.
    grep -qxF "check --job feed-scale --run-id $run_id" "$TMP/perf-calls" || fail "check was not called: $(cat "$TMP/perf-calls")"
    for tool in record report check; do
        grep -qxF "$tool sees PERF_ARTIFACTS_DIR=$FEED_SCALE_ARTIFACTS/perf" "$TMP/perf-env" ||
            fail "$tool did not see the artifacts' perf dir: $(cat "$TMP/perf-env")"
    done
}

test_perf_needs_the_metrics_file() {
    make_installed_we
    FAKE_SUITE=no-metrics bash "$SCRIPT" test >/dev/null 2>&1 || true
    out="$(bash "$SCRIPT" perf 2>&1)" && fail "perf passed with results.json but no metrics.json"
    grep -qF "no metrics.json" <<<"$out" || fail "the reason is not given: $out"
    [ ! -s "$TMP/perf-calls" ] || fail "perf called the history without metrics: $(cat "$TMP/perf-calls")"
}

test_perf_in_ci_uses_the_build_run_id() {
    make_installed_we
    CIRCLE_BUILD_NUM=77 bash "$SCRIPT" test >/dev/null 2>&1
    out="$(CIRCLE_BUILD_NUM=77 bash "$SCRIPT" perf 2>&1)" || fail "perf failed: $out"
    grep -qF -- "--run-id circleci-77 " "$TMP/perf-calls" || fail "record's run id is not the build's: $(cat "$TMP/perf-calls")"
    grep -qxF "check --job feed-scale --run-id circleci-77" "$TMP/perf-calls" || fail "check's run id is not the build's: $(cat "$TMP/perf-calls")"
}

test_perf_trend_page_never_fails_the_step() {
    make_installed_we
    bash "$SCRIPT" test >/dev/null 2>&1
    out="$(FAKE_PERF_REPORT_EXIT=2 bash "$SCRIPT" perf 2>&1)" || fail "a failed trend page failed the step: $out"
    grep -q "^check" "$TMP/perf-calls" || fail "check did not run after the trend page failed"
}

test_perf_regression_fails_the_step() {
    make_installed_we
    bash "$SCRIPT" test >/dev/null 2>&1
    local rc=0
    FAKE_PERF_CHECK_EXIT=7 bash "$SCRIPT" perf >/dev/null 2>&1 || rc=$?
    # check's own status: the failure is the rejected regression, not an earlier error.
    [ "$rc" = 7 ] || fail "perf exited $rc, not check's 7"
    grep -q "^check --job feed-scale" "$TMP/perf-calls" || fail "check did not run: $(cat "$TMP/perf-calls")"
}

# The artifacts dir outlives a job on a runner: results of another build are not this build's.
test_perf_ignores_results_of_another_build() {
    make_installed_we
    CIRCLE_BUILD_NUM=1 bash "$SCRIPT" test >/dev/null 2>&1
    out="$(CIRCLE_BUILD_NUM=2 bash "$SCRIPT" perf 2>&1)" || fail "perf failed: $out"
    grep -qF "nothing recorded" <<<"$out" || fail "the reason is not given: $out"
    [ ! -e "$TMP/perf-calls" ] || fail "perf recorded another build's results: $(cat "$TMP/perf-calls")"
    # Another commit in the same build number is another build too.
    out="$(CIRCLE_BUILD_NUM=1 AD4M_SHA=1111111111111111111111111111111111111111 bash "$SCRIPT" perf 2>&1)" || fail "perf failed: $out"
    [ ! -e "$TMP/perf-calls" ] || fail "perf recorded another commit's results: $(cat "$TMP/perf-calls")"
}

# A second attempt that writes results.json but no metrics.json must not inherit the first one's
# metrics.
test_a_new_attempt_clears_the_old_metrics() {
    make_installed_we
    bash "$SCRIPT" test >/dev/null 2>&1 || fail "the first attempt failed"
    [ -s "$FEED_SCALE_ARTIFACTS/metrics.json" ] || fail "the first attempt wrote no metrics"
    FAKE_SUITE=no-metrics bash "$SCRIPT" test >/dev/null 2>&1 || true
    [ ! -e "$FEED_SCALE_ARTIFACTS/metrics.json" ] || fail "the second attempt kept the first one's metrics.json"
    out="$(bash "$SCRIPT" perf 2>&1)" && fail "perf took the first attempt's metrics: $out"
    [ ! -e "$TMP/perf-calls" ] || fail "perf recorded: $(cat "$TMP/perf-calls")"
}

# `all` fails when a step inside perf fails, though perf's last command passes.
test_all_fails_when_the_record_fails() {
    make_we_remote
    make_installed_we
    printf '#!/usr/bin/env bash\nexit 0\n' >"$TMP/ad4m/scripts/coasys-spaces.sh"
    git -C "$TMP/ad4m" add -A
    git -C "$TMP/ad4m" -c user.name=t -c user.email=t@t commit -q -m fake-spaces
    echo "$WE_HEAD" >"$FEED_SCALE_WORK/we.sha"
    local rc=0
    FAKE_PERF_RECORD_EXIT=1 bash "$SCRIPT" all >/dev/null 2>&1 || rc=$?
    [ "$rc" != 0 ] || fail "all passed with a failed record"
    [ ! -e "$FEED_SCALE_STATE/last-success" ] || fail "all recorded the pair as measured after a failed perf step"
    # Without the fault it passes and records.
    rm -rf "$FEED_SCALE_ARTIFACTS" "$TMP/perf-calls"
    bash "$SCRIPT" all >/dev/null 2>&1 || fail "all failed on a clean run"
    [ -e "$FEED_SCALE_STATE/last-success" ] || fail "all did not record a passed run"
    grep -q "^check" "$TMP/perf-calls" || fail "all did not judge the run"
    # A failed suite still reaches perf, which records it.
    rm -f "$TMP/perf-calls" "$FEED_SCALE_STATE/last-success"
    FAKE_SUITE=fail bash "$SCRIPT" all >/dev/null 2>&1 && fail "all passed with a failed suite"
    grep -qF -- "--status failed" "$TMP/perf-calls" || fail "all did not record the failed run: $(cat "$TMP/perf-calls" 2>/dev/null)"
}

test_perf_records_a_failed_run_without_judging_it() {
    make_installed_we
    FAKE_SUITE=fail bash "$SCRIPT" test >/dev/null 2>&1
    out="$(bash "$SCRIPT" perf 2>&1)" || fail "perf failed on a failed run: $out"
    grep -qF -- "--status failed" "$TMP/perf-calls" || fail "the failed run was not recorded as failed: $(cat "$TMP/perf-calls")"
    grep -q "^check" "$TMP/perf-calls" && fail "a failed run was judged"
}

test_perf_without_results_records_nothing() {
    make_installed_we
    FAKE_SUITE=crash bash "$SCRIPT" test >/dev/null 2>&1
    out="$(bash "$SCRIPT" perf 2>&1)" || fail "perf failed with no results: $out"
    [ ! -e "$TMP/perf-calls" ] || fail "perf called the history with no results: $(cat "$TMP/perf-calls")"
}

test_perf_needs_the_history_tool() {
    make_installed_we
    bash "$SCRIPT" test >/dev/null 2>&1
    rm -r "$TMP/ad4m/scripts/perf"
    out="$(bash "$SCRIPT" perf 2>&1)" && fail "perf passed without scripts/perf"
    grep -qF "no scripts/perf/record" <<<"$out" || fail "the reason is not given: $out"
}

test_summary_is_the_suite_line() {
    make_installed_we
    [ "$(bash "$SCRIPT" summary)" = "feed-scale failed: no results ($FEED_SCALE_ARTIFACTS/results.json); the log of the step that stopped says why" ] ||
        fail "summary without results: $(bash "$SCRIPT" summary)"
    FEED_SCALE_MESSAGES=500 bash "$SCRIPT" test >/dev/null 2>&1
    [ "$(bash "$SCRIPT" summary)" = "feed-scale passed: messages=500" ] || fail "summary: $(bash "$SCRIPT" summary)"
}

# The executor step is coasys-spaces.sh's, run in this job's work dir.
test_executor_step_uses_its_own_work_dir() {
    local dest="$EXECUTOR_PUBLISH_DIR/$SHA"
    mkdir -p "$dest"
    head -c 10000 /dev/urandom >"$TMP/exe"
    gzip -c "$TMP/exe" >"$dest/ad4m-executor.gz"
    printf '{"commit": "%s", "file": "ad4m-executor.gz", "sha256": "%s", "size": %s}\n' \
        "$SHA" "$(sha256sum "$dest/ad4m-executor.gz" | cut -d' ' -f1)" "$(stat -c %s "$TMP/exe")" >"$dest/manifest.json"
    out="$(bash "$SCRIPT" executor 2>&1)" || fail "executor failed: $out"
    cmp -s "$TMP/exe" "$FEED_SCALE_WORK/ad4m-executor" || fail "the executor is not in the feed-scale work dir: $out"
    rm -r "$dest"
    out="$(bash "$SCRIPT" executor 2>&1)" && fail "executor passed with nothing published and FEED_SCALE_NO_BUILD=1"
    grep -qF "COASYS_SPACES_NO_BUILD=1" <<<"$out" || fail "FEED_SCALE_NO_BUILD did not reach coasys-spaces.sh: $out"
}

test_messages_must_be_a_number() {
    out="$(FEED_SCALE_MESSAGES=1e6 bash "$SCRIPT" summary 2>&1)" && fail "took FEED_SCALE_MESSAGES=1e6"
    grep -qF "is not a positive whole number" <<<"$out" || fail "the reason is not given: $out"
}

for t in $(declare -F | awk '$3 ~ /^test_/ {print $3}'); do
    echo "$t"
    setup
    "$t"
    rm -rf "$TMP"
done

if [ "$FAILED" = 0 ]; then
    echo "all passed"
else
    echo "FAILED"
    exit 1
fi
