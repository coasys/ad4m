# holograph-link

AD4M `LinkLanguage` backed by the holograph substrate
(sled `KvOpStore` + Kitsune2 transport + the substrate-agnostic
`perspective-diff-algorithm`).

This Language is the JS-facing shape of the holograph v1 spike (see
`.spike-docs/SPIKE.md` for the design). It implements the AD4M
LinkLanguage capability surface (`commit`, `sync`, `peers`,
`telepresence`) by delegating to host functions registered in
`ad4m:host` (`holographCreateNeighborhood`, `holographCommit`,
`holographRender`, `holographNextEmitted`, …). Those host functions
land in `rust-executor/src/js_core/host.js` and are backed by the
`HolographDelegate` trait in `rust-executor/src/holograph_wires.rs`.

## Step 5 status

- **JS module**: complete (`index.ts`), bundles via
  `pnpm run build` → `build/bundle.js`.
- **Host import surface**: exposed (the `holograph*` exports in
  `ad4m:host`); type-correct against `ad4m-ldk`.
- **Runtime delegate**: stubbed. Every call throws
  `[ad4m:host] holograph wire not yet implemented (Step 5 stub)`
  until Step 6 wires the real `HolographSpace` instance into the
  v8 isolate.

The Language address scheme uses the canonical content-address `hash()`
host function over `"@coasys/holograph-link@<version>"`. The version
string is part of `package.json`; bumping it produces a new address.

## Building

```sh
pnpm install      # at workspace root
cd bootstrap-languages/holograph-link
pnpm run build
```

Output: `build/bundle.js`. The bundle is consumed by the AD4M executor
(or `test-runner`) as a standard ES module.

## Testing

Two layers (Step 5d):

1. `pnpm run test` — Deno smoke test, asserts the exported method
   surface matches the AD4M LinkLanguage contract.
2. `pnpm run integration-test` — Step 7 territory; not exercised in
   Step 5.

## Architectural notes

Zero polling. Zero `setInterval`. Zero peer-revision walks.

The Step-3 `HolographIntegrationQueue` owns the watcher loop that
re-issues stalled fetches; K2 gossip + publish_ops own the propagation;
the JS subscriber drains `holographNextEmitted` (which awaits the
underlying mpsc receiver inside Rust — no JS-side delay timer).
`peers.remote()` is read from `DynPeerStore` synchronously at call
time.
