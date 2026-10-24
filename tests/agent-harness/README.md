# Agent Harness Integration Tests (A-series)

Pod-managed integration scenarios that test agent onboarding (**A2 Provision &
Connect**), waker subscriptions (**A4 Waker**), and A/V action loops (**A5 A/V
Loop**) across AI agent harnesses — OpenClaw, Hermes, and Sovereign — against a
real AD4M executor. Migrated from `coasys/ad4m-wind-tunnel`.

Each scenario is self-managed: it drives hardened verify scripts under
`interop/agents/`, each of which stands up its own Docker pod (AD4M node +
deterministic mock LLM + the real harness), runs the flow, asserts, and tears
the pod down. Ports are published on loopback only; containers run with
cap-drop, no-new-privileges, and resource limits.

> **CI:** these tests are **not** wired into CircleCI/GitLab. They are run
> manually — the executor image and harness images below are prerequisites the
> pipelines don't currently provide.

## Prerequisites

- Docker, Node.js ≥ 20, npm.
- **The AD4M executor test image** — every verify script defaults to
  `ad4m-test:latest`. Build it from the repo root `Dockerfile`:

  ```bash
  # from the ad4m repo root (slow first time — full Rust + JS build)
  docker build -t ad4m-test:latest .
  ```

  Override the tag per-route with `EXEC_IMG` / `A2_EXEC_IMAGE` /
  `A2H_EXEC_IMAGE` / `A2SV_EXEC_IMAGE` / `A4_EXEC_IMAGE` / `A4H_EXEC_IMAGE` /
  `A4SV_EXEC_IMAGE` / `A5H_EXEC_IMAGE` / `A5OC_EXEC_IMAGE` / `A5SV_EXEC_IMAGE`.
- `npm install` in this directory.

Routes whose prerequisites are missing **SKIP honestly** (recorded in metrics,
not failed) — except the A2 OpenClaw external-native route, which requires the
executor image and fails without it.

## Per-route prerequisites

| Route | Needs |
| --- | --- |
| A2 openclaw (external native) | `ad4m-test:latest`, pulls `ghcr.io/openclaw/openclaw:latest` |
| A2 external-plugin / managed, A4 openclaw, A5 openclaw | the above **plus** a buildable `plugins/ad4m` checkout — set `AD4M_PLUGIN_DIR` (or `AD4M_REPO`, from which `$AD4M_REPO/plugins/ad4m` is derived) |
| A2/A4/A5 hermes | the above plugin **plus** the Hermes image (`HERMES_IMAGE`, default `nousresearch/hermes-agent:latest` — note: **auto-pulled**, can be large) |
| A2/A4/A5 sovereign | a locally built Sovereign image — see `interop/agents/sovereign/Dockerfile` header for the build command |

The plugin build steps (`npm install`, `npm run build`, `npm pack`) run in your
`plugins/ad4m` checkout, so that working tree gets a `node_modules/` and
`dist/` as a side effect.

## Running

```bash
cd tests/agent-harness
npm install

npx tsx src/main.ts                 # all scenarios
npx tsx src/main.ts --scenario a4   # one scenario (a2 | a4 | a5)
```

Results land in `results/agent-harness/*.json`; the runner exits non-zero if
any scenario fails **or verifies nothing** (all routes skipped).

### Environment variables

| Variable | Default | Meaning |
| --- | --- | --- |
| `AD4M_PLUGIN_DIR` | `$AD4M_REPO/plugins/ad4m` if `AD4M_REPO` set | `plugins/ad4m` checkout used to build/pack the OpenClaw ad4m plugin |
| `HERMES_IMAGE` | `nousresearch/hermes-agent:latest` | Hermes harness image |
| `OC_IMG` | `ghcr.io/openclaw/openclaw:latest` | OpenClaw image |
| `KEEP=1` | unset | leave a verify script's pod running for debugging (skips teardown) |
| `AD4M_ADMIN_TOKEN` | `test123` | runner-level admin token (verify scripts use their own fixed test credentials) |
| `AD4M_WT_RESULTS_DIR` | `./results` | where reports are written |

Fixed host ports: each script uses its own loopback-only port block in the
14000–14500 / 18600–18800 range, so scripts don't collide with each other, but
two concurrent runs of the *same* script will.
