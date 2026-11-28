# scripts/perf — agent guide

## Nightly performance history (`scripts/perf/`)

`integration-tests-sfu`, `integration-tests-sfu-capacity`, `llm-e2e` and `agent-scale` record their numbers on every
run (passed or failed), fail on a serious regression against their own history, and draw a
trend page. The full contract is the header of `scripts/perf/perf.mjs`.

- **The history is a git repository**: folder `nightly/` on branch `main` of
  `coasys/ad4m-wind-tunnel`. `nightly/<job>.jsonl` holds one line per metric per
  run, `nightly/<job>.checks.jsonl` one verdict per check. Both are append-only.
  `record` commits `perf(<job>): <sha9> <run_at>` in a fresh shallow clone
  (`$TMPDIR/ad4m-perf-history-<job>-*`, removed when the command ends), and
  pushes. When another run pushed first, it fetches, puts its lines on the new
  head and pushes again, up to 5 times. git runs with hooks off and its own ssh
  command (no `~/.ssh/config`): the runner also runs PR jobs, so nothing of the
  history, and not the key, stays on it. `scripts/perf/history-key` moves the
  key out of `~/.ssh` (`perf_history_key`) and deletes it, by fixed paths, with
  any clone left (`perf_history_cleanup`, the end of `perf_finish`). A job with
  its own record step calls those two commands. Env: `PERF_HISTORY_REPO`, `PERF_HISTORY_BRANCH`,
  `PERF_HISTORY_SUBDIR`, `PERF_HISTORY_SSH_KEY`.
- **The default repository is written only when `CI=true`.** For a local run,
  set `PERF_HISTORY_REPO` to a local bare repo. Never push test runs to the
  real history.
- **The push key.** CI pushes with a write deploy key on the wind-tunnel repo.
  Add it in Project Settings → SSH Keys → Additional SSH Keys (hostname
  `github.com`), and put its fingerprint in the pipeline parameter
  `perf-history-key-fingerprint`. If the key is missing or a push fails, the job
  does not fail. `record` prints `history not pushed: <reason>`, and the records
  stay in the job's artifacts (`perf/records.jsonl`). A `check` that cannot read
  the history still fails on a ceiling or floor; only the factor rule reports.
- **`check`** compares a value with the median of the last 7 passing runs with
  the same job, metric and params. It fails the job when a lower-is-better value
  is above `factor` × the median or above `ceiling`, or a higher-is-better value
  is below the median ÷ `factor` or below `floor`; the factor rule also needs the
  value at least `min_delta` worse than the median. `factor`
  (default 1.5), `min_delta` (default 0), `ceiling` and `floor` are per job and
  metric, and live only in `scripts/perf/thresholds.json`; a ceiling or floor
  fails at any history. With fewer than 3 earlier runs, the factor rule only
  reports. Values with different `params` (such as another LLM
  model) never compare. A regressed run stays out of later baselines.
  `scripts/perf/check --job J --accept <run_id>` takes it as the new normal: it
  resets the baseline, so later runs compare only with it and the runs after it.
- **What each job measures.**
  - `integration-tests-sfu`: every scenario writes `$PERF_RESULTS_DIR/sfu-<id>.json`
    (`tests/js/helpers/sfu/perf.ts`), and the step adds `sfu.suite.duration_ms`.
  - `integration-tests-sfu-capacity`: `tests/js/tests/sfu-capacity.test.ts` writes
    `$PERF_RESULTS_DIR/sfu-capacity.json`: per call size (params `{participants}`)
    `sfu_capacity.cpu_percent`, `.datagrams_per_s`, `.loop_iterations_per_s` and
    `.cpu_ms_per_kdatagram`. The datagram and pass counts are `sfu.status` counters.
    Their limits are a factor of 1.2 (see `thresholds.json`).
  - `llm-e2e`: raw logs go to `$PERF_RESULTS_DIR/raw`:
    - the Rust tests through `scripts/perf/libtest-timer.mjs`;
    - mocha through `PERF_MOCHA_LOG` (`tests/js/utils/perfMochaHooks.ts`);
    - each step's output as `<suite>.log`, where the executor logs every prompt
      (`✅ 🤖 prompt done ... latency=…ms tokens_out=…` at info, `❌ 🤖 prompt
      failed` at error, `rust_executor::ai_service`). The Rust step sets
      `RUST_LOG=warn,rust_executor::ai_service=info` (its tests default to warn).

    `scripts/perf/llm-e2e-results.mjs` turns them into metrics (calls, failures,
    latency, output tokens/s), with params `{model}`.
  - `agent-scale`: the test writes `$PERF_RESULTS_DIR/agent-scale.json`
    (`tests/js/helpers/agent-scale.ts`): every metric once per agent count (params
    `{agents: N, links}`), and its growth between the smallest and the largest count
    (`agent_scale.<metric>.growth_10_100`, params `{from, to, links}`). `links` is
    `AGENT_SCALE_LINKS`: another link count is another workload, so a series of its own.
- A new nightly measurement writes a results JSON file (`{"metrics": [...]}`)
  into `$PERF_RESULTS_DIR`. Wrap the job's tests in `perf_setup` and
  `perf_finish` (`.circleci/config.yml`).
- **Tests**: run `pnpm run test-perf-unit` in `tests/js`. It runs mocha for
  `sfu-perf.unit.test.ts` and `agent-scale.unit.test.ts`, and `node --test 'scripts/perf/*.test.mjs'`. The git
  tests push only to bare repos in a temp directory. The `js-unit-tests` CI job
  runs them.

