# @coasys/identity-wasm

`ad4m-identity-core`'s `wasm` feature built with wasm-bindgen for the `web` target. The Connect
frame (a browser page) uses it instead of its own TypeScript for HPKE, XChaCha20, JCS, base58,
the record, inbox and claim formats. There is no Rust and no crypto in this directory: the code
is `ad4m-identity-core/src/{device,wasm}.rs`.

## Build and test

```bash
cargo install wasm-bindgen-cli --version <the version in Cargo.lock> --locked   # once
pnpm --filter @coasys/identity-wasm build   # -> pkg/identity.js, identity.d.ts, identity_bg.wasm
pnpm --filter @coasys/identity-wasm test    # loads pkg/ with init() under Node and calls the exports
```

`scripts/build.mjs` refuses a wasm-bindgen CLI that differs from the locked crate (set
`WASM_BINDGEN_BIN` for one outside `PATH`). The behaviour is tested in Rust, on wasm32 under
Node: `ad4m-identity-core/tests/{device,wasm_device}.rs`.

## Rules

- The package never holds an Ed25519 key. A `*Bytes` export returns what to sign (WebCrypto signs
  it); the export after it takes the signature, checks it and returns the value to send. Only
  the X25519 key (`EncKey`) lives in WASM memory.
- `pkg/` is build output (git-ignored). Scripts are `.mjs` so the root `*.js` ignore does not
  hide them.
- Every export is one `device`/`KelChain` function of the core. A new format goes into the
  core first (with its vector), then here.
- Releases: `setVersion.js` sets this package's version with the others; the snapshot workflow
  (`publish_snapshot.yml`) builds and publishes it to pkg.pr.new.
