# Web executor spikes (S0a, S0b)

Two throw-away probes for an AD4M executor that runs in a browser.
They were run on 2026-11-19 against `origin/dev` at `425311398`.
This is spike code. It is not built by CI and it is not shipped.
The full results are in each `REPORT.md`. This file gives a summary only.

| Dir | Probe | Question |
|---|---|---|
| `s0a-worker/` | S0a (T3) | Can a published language bundle run in a browser Web Worker and sync with a native executor? |
| `s0b-wasm/` | S0b (T1 + T2) | Is Oxigraph fast and small enough in Wasm? How much of the query core compiles for `wasm32`? |

## S0a: a language in a browser Worker

### What it shows

- A headless Chrome tab runs the published, templated `server-link-language` bundle in a module Worker.
- `web/shim-src.js` (184 lines) replaces the `ad4m:host` module. The Worker rewrites the one `from "ad4m:host"` in the bundle text to the shim URL. No other byte of the bundle changes.
- No executor serves the tab. The tab talks only to `link-server`, through a test CORS proxy.
- Results (3 of 3 runs):
  - A link added in the native executor arrives in the tab as `emitPerspectiveDiff`: 29 / 29 / 27 ms.
  - A link committed from the tab arrives in the native executor: 35 / 37 / 37 ms. The native executor marks its signature `valid: true`.
  - A removal from the tab arrives in the native executor: 27 / 33 ms (runs 2 and 3).
- In run 3 the bundle made 182 host calls. Only 2 of them need the private key: `agentSignStringHex` and `agentOpenSealed`. Both are sync. `hash` is also sync, so the shim carries a JS SHA-256.
- The bundle read none of `Deno`, `Buffer`, `process`, `window`, `document` or the old executor globals. Its only I/O outside the host contract is `WebSocket`.

### Files

- `web/index.html`, `web/worker.js`, `web/shim-src.js`: the page, the Worker and the shim source.
- `scripts/drive.mjs`: drives headless Chrome with `playwright-core`, and the native side with the `ad4m` CLI.
- `scripts/start-executor.sh`: native executor with a temp data dir, no Holochain, egress through a deny proxy.
- `scripts/cors-proxy.mjs`, `scripts/egress-proxy.mjs`, `scripts/static-server.mjs`: test-only helpers.
- `selftest.mjs`: checks the shim in Node (sealed box, signature, `hash` vectors).
- `logs/browser-run3.json` (run 3: steps, events, every shim call), `logs/native-snapshot.txt` (the native view of the links).

### How to run

You need: Node 24, `/usr/bin/google-chrome` (or set `CHROME`), and `ad4m` + `ad4m-executor` built from this repo.
The ports are fixed: 47456 link-server, 47457 CORS proxy, 47458 page, 47459 egress proxy, 47460 executor.

```bash
cd spikes/web-executor/s0a-worker
pnpm install --ignore-workspace          # or npm install; this dir is not in the pnpm workspace
pnpm run build:shim                      # web/shim-src.js -> web/shim.js (esbuild)
node selftest.mjs

# Built from the repo root (see REPORT.md section 2 for the exact commands):
#   bootstrap-languages/server-link-language/build/bundle.js  (deno run --allow-all esbuild.ts)
#   link-server/dist/index.js                                  (npm ci && npx tsc)
node ../../../bootstrap-languages/local/generate-seed.mjs ../../../bootstrap-languages/local seed   # local seed, no network

node ../../../link-server/dist/index.js --port 47456 --host 127.0.0.1 --data "$(mktemp -d)" --auto-admit &
node scripts/cors-proxy.mjs 47457 47456 logs/cors.log &
node scripts/egress-proxy.mjs 47459 logs/egress.log &
node scripts/static-server.mjs 47458 "$PWD/web" &
EXECUTOR_BIN=/path/to/ad4m-executor scripts/start-executor.sh &
```

Then publish the language, make a neighbourhood and copy the templated bundle to `web/templated-bundle.js`.
The `ad4m` CLI commands are in `REPORT.md` section 2. Then:

```bash
AD4M_BIN=/path/to/ad4m LANG_ADDR=<templated language address> PERSP=<perspective uuid> OUT=browser-run node scripts/drive.mjs
```

Env vars: `S` (this dir), `AD4M_BIN`, `EXECUTOR_BIN`, `EXECUTOR_URL`, `PAGE_URL`, `CHROME`, `SEED`. Each has a default.

## S0b: Oxigraph and the query core in Wasm

### What it shows

Step 1 (T1): `crate/` is Oxigraph 0.5.13 (the executor's version) with a bench.
The bench writes the executor's quad shape: 8 quads per link, 50,000 links = 400,000 quads, in-memory store.
It runs 6 queries shaped after `model_query` and `sparql_store.rs` output.
Results with the `sparopt` patch (Node 24, rustc 1.88, median of 3 runs):

| Metric | Native | Wasm | Ratio |
|---|---|---|---|
| `.wasm` brotli q11 / gzip -9 | – | 0.60 MB / 0.87 MB | – |
| Load 50k links | 688 ms | 1064 ms | 1.55× |
| Q1–Q6 | 1.90–243.9 ms | 2.94–293.0 ms | 1.09–1.55× |
| Memory per link | 3,340 B (RSS) | 2,890 B (linear memory) | 0.87× |

- Headless Chrome 146, measured once: load 886.7 ms; Q1 / Q2 / Q3 = 119.6 / 3.6 / 41.9 ms.
- Scaling (10k, 50k, 100k links) is linear. The wasm/native ratios stay at 1.2–1.6×.
- **Oxigraph bug on 32-bit targets.** `sparopt` 0.3.9 computes join costs in `usize` with saturating math. On wasm32 the costs saturate, and OPTIONAL runs row by row. Stock Oxigraph makes Q2 (the hydration query of every paginated `model_query`) 85× slower than native (160.9 ms vs 1.90 ms). `bench/sparopt-u64.patch` changes the cost type to `u64` (11 lines). With the patch, Q2 is 1.55× native. The patch is a no-op on 64-bit. `crate/vendor/sparopt` is the patched copy, used through `[patch.crates-io]`.

Step 2 (T2): `probe/` type-checks a copy of the query core for `wasm32-unknown-unknown` and for native.
The copy is `sparql_store.rs`, `shacl_parser`, `model_query` and the modules they need: 63 files (55 `.rs`), verbatim from `rust-executor/src` at `425311398`.
`src/lib.rs` stubs the rest of the executor. Each stub marks a seam that a real core crate must cut.

- Raw copy: 87 errors on both targets. 60 of them come from `deno_core::anyhow`.
- After the mechanical changes (`anyhow` instead of `deno_core::anyhow`, tokio without `full`, `js` features for `uuid` and `oxigraph`): **4 errors on wasm32**. All 4 are the RocksDB-only Oxigraph API in `sparql_store.rs`. They need a `rocksdb` cfg gate.
- With the executor's own features, native has 0 errors, so the probe is complete. `tokio/full` is the only dependency that breaks wasm32 (in `mio`).
- `probe-deno/`: the real `deno_core` (coasys fork) fails on wasm32 in `v8`, `mio`, `sys_traits` and `uuid`. `deno_core` cannot be in a wasm32 core crate.

### Files

- `crate/`: the step-1 crate (`src/lib.rs` data + queries, `src/bin/native.rs`), `vendor/sparopt`.
- `bench/`: `node-bench.cjs`, `node-extra.cjs`, `browser.html`, `browser-run.mjs`, `probe-check.sh`, `sparopt-u64.patch`, and the outputs the report tables cite (`*-run*.txt`, `*-196.txt`, `scaling.txt`, `q2b.txt`, `q2-variants.txt`, `browser-run1.txt`, `runtime-hazards.txt`).
- `probe/`: `Cargo.toml`, `Cargo.lock`, `src/lib.rs` (stubs), `shim-deno-core/`, `fetch-sources.sh`. The copied executor files are not committed. `fetch-sources.sh` gets them from git.
- `probe-deno/`: the real-`deno_core` probe.

Each crate has its own `[workspace]` table, so the root workspace does not build it.

### How to run

You need: `rustup` with toolchain 1.96.1 and target `wasm32-unknown-unknown`, `wasm-pack` 0.14, Node 24.
The step-1 table used rustc 1.88.0. A cross-check with 1.96.1 is in `REPORT.md`.

```bash
cd spikes/web-executor/s0b-wasm
export CARGO_TARGET_DIR=$PWD/target

# Step 1
(cd crate && cargo build --release --bin native) && target/release/native 50000
(cd crate && wasm-pack build --release --target nodejs --out-dir ../pkg-u64)
node bench/node-bench.cjs pkg-u64 50000
(cd crate && wasm-pack build --release --target web --out-dir ../pkg-web)
node bench/browser-run.mjs                    # CHROME=/path/to/chrome to override
# Stock Oxigraph: remove [patch.crates-io] from crate/Cargo.toml and build to pkg/.

# Step 2 (cargo check only)
bash probe/fetch-sources.sh                   # AD4M_REPO=<ad4m checkout> if not run from this repo
bash bench/probe-check.sh r5 --features deno-shim                 # 4 errors on wasm32 and native
bash bench/probe-check.sh r6 --features deno-shim,executor-deps   # native 0; wasm32 stops in mio
(cd probe-deno && cargo +1.96.1 check --keep-going --target wasm32-unknown-unknown)
```

## What these probes do NOT show

- **The real executor does not build to Wasm yet.** S0b step 1 is Oxigraph with the executor's quad and query shapes. It is not the executor's `SparqlStore` or `model_query` code.
- **S0b step 2 is `cargo check` only.** Nothing in the query core was built to a `.wasm` or run. 4 wasm32 errors are left: the RocksDB gate.
- **The query watchdog is a design problem.** `sparql_store.rs` starts a thread per request query as a deadline watchdog. On wasm32 the thread spawn fails, and the code then cancels the query at once. So every request-gated query would fail. `spawn_blocking` (2 sites) also needs a Wasm path. This compiles, so `cargo check` does not catch it. It needs a deadline without a thread.
- The `types` split and the `LanguageController` call in `model_query/query.rs` are seams that a core crate must cut. The probe stubs them.
- The S0b ratios are from Node. Chrome was measured once, on the main thread, not in a Worker. No Firefox, Safari or mobile.
- The size numbers are Oxigraph + bench only, not the executor code on top.
- No persistence (IndexedDB/OPFS). In-memory store only.
- S0a ran on localhost only: no TLS, no `link.ad4m.dev`. Admission was `--auto-admit` only.
- S0a used a CORS proxy. `link-server` does not answer CORS preflights today (404 on `OPTIONS`).
- S0a tested one language (`server-link-language`). The shim KV is memory only: a page reload gives a new DID. No reconnect, no token refresh, no two tabs.
- `init` resolves even when the language cannot reach the server. A host needs other ready signals.
