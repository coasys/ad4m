# .circleci — agent guide

CI config (`config.yml`) and its scripts.

## Nightly (`nightly-llm-e2e` workflow)

A pipeline with `run-nightly-llm: true` runs only this workflow, on `coasys/marvin`.
Each job has its own serial group, except `llm-e2e` and `coasys-spaces`, which
share `nightly-llm`. The jobs: `llm-e2e`,
`integration-tests-sfu`, `integration-tests-sfu-browsers` (see below),
`coasys-spaces`, `integration-tests-agent-harness` and `feed-scale`.

`llm-e2e` runs the ignored Rust interpretation suites and the JS suites gated
by `LLM_E2E=1`. It does not start a model server. It prompts staging's
llama-server at `http://127.0.0.1:12460/v1` (alias `qwen3.8-27b-ud-q4_k_m`,
kind `LLAMA_SERVER`, context budget 57344 under the server's 65536-token
slot) and embeds with Ollama's `nomic-embed-text` on `:11434`. A preflight
checks `/health` and that `/v1/models` lists the alias, and fails the job
when the unit is down. Requests share that one slot with staging's own AI
processing.

`coasys-spaces` runs WE's product suite (`pnpm --filter @we/app-web test:spaces`,
WE `apps/we-web/tests/live/coasys-spaces/`): a Coasys space, a call, its
transcript (executor Whisper), extraction on staging's llama-server
(`http://127.0.0.1:12460/v1`, model `qwen3.8-27b-ud-q4_k_m`), and flows.
That server is already running (`llama-qwen27b.service`, one 98304-token slot)
and already holds the GPU. The job does not start it and does not load a model.
`COASYS_SPACES_LLM_NUM_CTX` is empty, so `llm-check` does not make a context
variant. `/api/create` runs only when the server is Ollama (`/api/version`
returns a version) and `COASYS_SPACES_LLM_NUM_CTX` is set: a hand run against
Ollama can still make a variant such as `coasys-spaces-gemma3-12b:ctx16384`
when the model's own `num_ctx` is smaller. A model whose own `num_ctx` is
already that large is used as it is. The job also sets
`COASYS_SPACES_LLM_ENDPOINT_KIND=LLAMA_SERVER`,
`COASYS_SPACES_LLM_NATIVE_TOOLS=true`,
`COASYS_SPACES_LLM_REQUEST_TIMEOUT_SECS=900` and
`COASYS_SPACES_LLM_MAX_NUM_CTX=90112`. The script passes those through;
WE's real tier (we#468) registers them on the endpoint. The 900s limit
includes time queued on the one slot. It uses this commit's executor and SDK:

- The executor is the one the `build` job published for the commit
  (`$EXECUTOR_PUBLISH_DIR/<sha>/manifest.json`, sha256 checked). If there is none,
  the job builds it.
- `core` and `connect` are built and packed. WE's ad4m pin (root `pnpm.overrides`)
  points at the two tarballs.
- WE comes from the pipeline parameter `we-ref` (branch, tag or full commit;
  default `dev`). WE installs with its own pnpm (10).

The job halts green when the same ad4m commit, WE commit, tier and model already
passed on the host (`~/.local/state/ad4m-ci/coasys-spaces/last-success`). Set
`COASYS_SPACES_FORCE=1` to run anyway. A checkout with changes (a run by hand
with local edits) runs, but neither skips nor records a pass. It shares the serial group
`nightly-llm` with `llm-e2e`, so the two do not run at once: both prompt
staging's llama-server on :12460, and `llm-e2e`'s embeddings stay on Ollama.
A completion in this job may wait a few minutes on llama-server's one
slot, behind staging. Screenshots, traces and logs are under the
job's artifacts, `coasys-spaces/`.

Each step is a subcommand of `scripts/coasys-spaces.sh`. `bash
scripts/coasys-spaces.sh all` runs the whole job. The real tier needs the model
server named by `COASYS_SPACES_LLM_*` (the nightly job uses staging's
llama-server). `COASYS_SPACES_TIER=stub` needs no model. `bash
scripts/coasys-spaces.test.sh` tests the script.

`feed-scale` measures WE's Feed in a space with a million messages
(`pnpm --filter @we/app-web test:feed-scale`, WE `apps/we-web/e2e/feed-scale/`).
It needs no model and no GPU, and has its own serial group, `nightly-feed-scale`.

- The suite seeds `FEED_SCALE_MESSAGES` (1000000) TextBlock messages, 12 links each.
  It generates every link already signed by the message's author (one of 40 seed
  agents; the Feed shows its author and time) into one file, on all cores, and loads the
  file into the space's perspective with `runtime.importData` (`type: "perspective"`,
  Oxigraph's bulk loader, one commit; nothing goes to the link language). The import
  only adds links new to the perspective: it refuses the whole file (a 400) if a link is
  already stored, repeats in the file, or is ended by a tombstone in the file (a tombstone
  that ends a link of the file, or a link the store holds, refuses it too). A link that a
  tombstone already in the store ended is not loaded and counts as `covered`; the same
  link twice in the file is refused. The import takes the perspective's `persist_lock`,
  so it lands before or after a diff's write, never between. Then WE web opens the
  space in Chromium five times (cold, then warm) and loads the newest page and one
  older page. The links file (~6 GB at a million) lives in the data dir, beside the
  store.
- WE registers an auto-processor for the space's own collection when the space opens
  (`we-call-<collection>`, intended behaviour). The suite removes it for the seeding
  only, which is setup. The app registers it again for the loads, and the job measures
  its cost: `executor.cpu_seconds.ui_phase` (the executor's CPU over the loads) and
  `executor.idle_cpu_percent.after_ui` (idle with the watch on). A fix to the
  auto-processor's tick shows in these two.
- It writes `metrics.json` (metrics in the performance-history record format, job
  `feed-scale`, params `{"messages": N}`; the failure counts and
  `executor.cpu_seconds.ui_phase` add `"loads": L`, the number of loads, so a run with
  another number of loads makes its own series) and `results.json` (the same, with the
  per-call detail). The `perf` step gives `metrics.json` to `scripts/perf/record`,
  draws the trend page with `scripts/perf/report`, and for a passed run asks
  `scripts/perf/check` for the verdict, by the `feed-scale` rules in
  `scripts/perf/thresholds.json`. The records and the trend page are in the artifacts
  under `feed-scale/perf/`.
- The executor, SDK, WE and we-web steps are `coasys-spaces.sh`'s, run in
  `~/.cache/ad4m-ci/feed-scale`. The executor's data goes there too, not to /tmp
  (a shared tmpfs): a million messages take tens of GB.
- It halts green when the same ad4m commit, WE commit and size were already measured
  (`~/.local/state/ad4m-ci/feed-scale/last-success`; `FEED_SCALE_FORCE=1` runs anyway).
  Results, logs and failure screenshots are under the job's artifacts, `feed-scale/`. The
  job log ends with the suite's summary line.

`bash scripts/feed-scale.sh all` runs the whole job by hand
(`FEED_SCALE_MESSAGES=10000` for a short run). `bash scripts/feed-scale.test.sh` tests the
script.

To run the workflow by hand on a branch, with another WE ref (the definition id
is in Project Settings → Pipelines):

```bash
curl -X POST https://circleci.com/api/v2/project/circleci/VRkXSnZDyBKHhkHKBHTmLj/W84Q4MGGTeZwizersNwxB4/pipeline/run \
  -H "Circle-Token: $CIRCLECI_TOKEN" -H 'content-type: application/json' \
  -d '{"definition_id": "<definition id>",
       "config": {"branch": "<branch>"}, "checkout": {"branch": "<branch>"},
       "parameters": {"run-nightly-llm": true, "we-ref": "<WE branch>"}}'
```

This runs all six nightly jobs.


## K2 bootstrap server in CI (`install-hc-toolchain`)

`scripts/install-hc-toolchain.sh` installs `hc` and `kitsune2-bootstrap-srv` per
workspace, at the versions `Cargo.lock` pins. `tests/js/utils/utils.ts` starts
`kitsune2-bootstrap-srv` from `$PATH`. A host copy at another kitsune2 version
drifts on the wire protocol, so the script installs its own under `.hc-toolchain/bin`.
Test it with `scripts/install-hc-toolchain.test.sh`.

## CI looks stuck

Before you blame the disk, check these in order:

1. The runner units. A paused unit looks like a hang.
2. Unclaimed tasks per resource class:
   `runner.circleci.com/api/v3/runner/tasks?resource-class=...`.
3. Superseded or duplicate workflows. Cancel them. Each push, including a temp
   gate branch, starts a full pipeline, and CircleCI has no per-PR priority.

## Self-hosted runner failure signatures

- `checkout: branch already used by worktree`: a worktree under a runner's
  `ci-workdir*` pins its branch. Remove that worktree.
- `unresolved deltas` or `invalid index-pack` on checkout: the runner's
  `ci-workdir` clone lost its `blob:none` partial-clone config. Restore it.
- `tmpfile.exists()` panic in the v8 `build.rs`: `~/.cargo/.rusty_v8` is missing
  the lib `.a.gz` or the `src_binding` `.rs`. Put both there.
- `kitsune2-bootstrap-srv already exists in destination`: the runner's
  `.hc-toolchain` cargo records are out of sync. Reinstall with
  `cargo install --force`.
- `waiting for file lock on package cache`: low-priority builds on the same
  host hold the shared cargo lock. Wait for them or stop them.
