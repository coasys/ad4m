# Agent Harness Integration Tests (A-series)

Pod-managed integration scenarios that test agent onboarding (**A2 Provision &
Connect**), waker subscriptions (**A4 Waker**), and A/V action loops (**A5 A/V
Loop**) across AI agent harnesses — OpenClaw, Hermes, and Sovereign — against an
AD4M executor built from this checkout. Migrated from `coasys/ad4m-wind-tunnel`.

Each route is a verify script under `interop/agents/` that stands up its own
Docker pod (AD4M node + deterministic mock LLM + the real harness), runs the
flow, asserts, and tears the pod down. Ports are published on loopback only;
containers run with cap-drop, no-new-privileges, and resource limits. The
drivers run this checkout's `@coasys/ad4m` (`core/`), including the
`WakerSubscriptionManager` the OpenClaw plugin re-exports, and the OpenClaw
routes install this checkout's `plugins/ad4m`.

## Routes

| Scenario/route | Harness | Proves |
| --- | --- | --- |
| `a2/external-native` | OpenClaw | its native MCP client signs up, logs in and reads its DID |
| `a2/external-plugin` | OpenClaw + `plugins/ad4m` | `openclaw ad4m-setup` provisions a user on an external node |
| `a2/managed` | OpenClaw + `plugins/ad4m` | managed mode starts its own node and generates an agent, with no network |
| `a2/hermes` | Hermes | `mcp_servers.ad4m` with a user JWT: a turn reads the DID and creates a perspective |
| `a2/sovereign` | Sovereign | its injected ad4m MCP: a turn creates a perspective |
| `a4/openclaw` | OpenClaw | channel subscription (`generate_waker_query`) → 1 wake per message, debounce, liveness → `/hooks/wake` → a turn |
| `a4/hermes` | Hermes | the same subscription → signed `/webhooks/wake` → a turn |
| `a4/sovereign` | Sovereign | its native waker on a mention → presence turn → reply lands in the channel |
| `a5/hermes` | Hermes | mention subscription (`get_mention_waker_config`): call presence does not wake, a caller's transcript naming the agent wakes it once → the turn reads the transcript, then replies |
| `a5/openclaw` | OpenClaw | the same wake, through `/hooks/wake` → a turn (wake half only: the mock cannot drive OpenClaw's tool protocol) |
| `a5/sovereign` | Sovereign | its native waker on the transcript → presence turn → reply lands |

A5's transcript is written and signed by a second agent (a fresh did:key), as a
peer's links arrive through a neighbourhood: the executor's mention queries only
read verified links another agent wrote.

## Prerequisites

- Docker, Node.js ≥ 20, pnpm, npm (the plugin build uses npm, as its own CI does).
- `core/` built (`cd core && pnpm install && pnpm run build`), and the plugin's
  dependencies installable (`plugins/ad4m`, `npm install`; the scripts do it).
- **The executor image**, built from this checkout's binaries:

  ```bash
  # from the repo root
  cargo build --release -p ad4m --bin ad4m-executor --bin ad4m
  tests/agent-harness/interop/executor/build-image.sh   # -> ad4m-agent-harness-executor:local
  ```

  It wraps `target/release/{ad4m-executor,ad4m}` (or `AD4M_BIN_DIR`) with
  `docker-entrypoint.sh` and the `bootstrap-languages/local` seed. Every
  script fails if the image is missing.
- `pnpm install --ignore-workspace` in this directory (it is not a workspace
  package; `@coasys/ad4m` links to `../../core`).

Harness images are pinned by digest in `interop/agents/lib.sh` and pulled on
first use (Hermes is about 4 GB). A route whose harness image cannot be had
**SKIPs**. Sovereign's image is private: build it from a Sovereign checkout with
`interop/agents/sovereign/Dockerfile` and pass its tag as `SOVEREIGN_IMAGE`;
without it the Sovereign routes skip.

## Running

```bash
cd tests/agent-harness
pnpm install --ignore-workspace

pnpm start                                # all scenarios
pnpm start --scenario a4                  # one scenario (a2 | a4 | a5)
pnpm start --require a4/openclaw,a4/hermes
bash interop/agents/verify-a4-hermes.sh   # one route
pnpm run build                            # typecheck (src + interop drivers)
```

Results land in `results/agent-harness/<scenario>.json`, each route's output in
`results/agent-harness/logs/<scenario>-<route>.log`, and a failing route's
container logs next to it. The runner exits non-zero when a scenario fails (a
route failed, or none passed) or a required route did not pass: with
`--require` / `AGENT_HARNESS_REQUIRED`, a SKIP of a required route fails the run.

### Environment variables

| Variable | Default | Meaning |
| --- | --- | --- |
| `AD4M_EXEC_IMAGE` | `ad4m-agent-harness-executor:local` | executor image (`build-image.sh` builds this tag) |
| `AD4M_BIN_DIR` | `<repo>/target/release` | where `build-image.sh` takes the binaries from |
| `AD4M_PLUGIN_DIR` | `<repo>/plugins/ad4m` | the OpenClaw ad4m plugin to build and install |
| `OC_IMG` | `ghcr.io/openclaw/openclaw@sha256:…` (2026.7.1) | OpenClaw image |
| `HERMES_IMAGE` | `nousresearch/hermes-agent@sha256:…` | Hermes image |
| `SOVEREIGN_IMAGE` | `swwt-sovereign:latest` | locally built Sovereign image |
| `AGENT_HARNESS_REQUIRED` | unset | comma-separated `<scenario>/<route>` that must pass |
| `AD4M_WT_RESULTS_DIR` | `./results` | where reports are written (or `--results-dir`) |
| `KEEP=1` | unset | leave a verify script's pod running for debugging |

Fixed host ports: each script uses its own loopback-only port block in the
14000–14500 / 18600–18800 range, so scripts don't collide with each other, but
two concurrent runs of the *same* script will.

## CI

The CircleCI job `integration-tests-agent-harness` runs these scenarios nightly
in the `nightly-llm-e2e` workflow (serial group `nightly-agent-harness`, on
`coasys/marvin`), not on pushes. It builds the binaries from the commit
(`setup_integration_test_environment` with `build_binaries: true`), builds the
executor image with `build-image.sh`, and runs all scenarios with every
OpenClaw and Hermes route required:

```
a2/external-native a2/external-plugin a2/managed a2/hermes
a4/openclaw a4/hermes a5/hermes a5/openclaw
```

The Sovereign routes skip there (no private image) and are not required. On
failure the job stores the results JSON, the route logs and the pod logs as the
`agent-harness` artifacts.
