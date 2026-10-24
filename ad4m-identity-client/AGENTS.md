# ad4m-identity-client — agent guide

The master-key side of `did:scid:coasys:1` for apps (AR-03 4.2–4.5; identity plan PR 1.1).
The executor holds device keys only; an app (the desktop custody service, later the phone and
the auth domain) holds the words and master keys through this crate, builds the events, and
lets its executor co-sign and publish them. No custody key (decision 21.5).

| File | What |
|---|---|
| `src/lib.rs` | `Clock`, `Random` (the surface passes both: no clock or randomness of its own), `SystemClock` (not wasm32), `OsRandom`, `ClientError` |
| `src/secret.rs` | `Secret<T>`: zeroed on drop, `Debug` prints `<redacted>`, no `Serialize`, no `Clone` |
| `src/vault.rs` | `Vault`, `Unlocked` (one unlock per flow, 300 s), `MasterSigner` (`Signer` + `index`), `KeySigner`, `MasterUse` (`Link`, `Revoke`), `VaultError`; the trait has `store_master` and `unlock` only; `MemoryVault` (tests and feature `test-vault`, never in a release build) |
| `src/executor.rs` | `ExecutorRpc` (one RPC call), `ViaExecutor` (the typed `identity.*` calls the flows make: status, log, cosignIncept, publish, openOffer, offers; claiming is the joining executor's call, `ad4m-executor claim`); feature `ws`: `ExecutorRpc` for `ad4m-client`'s `WsRpcClient` |
| `src/flows/create.rs` | `CreateFlow` (words shown → two typed back → `finish`): master #0, `incept` listing the executor's key as `d1`, `identity.cosignIncept`, fold, `identity.publish`, then the vault keeps master #0 |
| `src/flows/link.rs` | `LinkFlow`: `open` (offer signed by the current master at the head, offer link), `wait_for_claim` (master-signed list requests; the match code), `confirm` (the `add` that spends the claim) |
| `src/flows/revoke.rs` | `revoke` (a `revoke` signed by the current master), `key_id_of` |
| `examples/holder.rs` | the flows over JSON lines against a real executor (features `ws,test-vault`), for `tests/js/tests/identity.test.ts` |
| `tests/flows.rs` | the flows against a real `identity-store` (router in process) through a test executor |

## Rules

- **The words and master keys never reach an executor or a log.** No `ViaExecutor` call takes
  or returns one. Master keys live in a `Vault`; a flow reads one only through `Unlocked`.
- **One unlock per flow** (`Vault::unlock`): linking signs the offer, the list requests and the
  `add` under one unlock. AR-03 4.3.2 writes the vault as `with_unlocked(callback)`; the flows
  are async across network calls, so the unlock is a guard the flow holds instead.
- The client re-checks what it reads: every log goes through `fold` here too.
- The core (no features) builds for `wasm32-unknown-unknown`; keep `tokio`, sockets and the
  system clock out of it (they sit behind `ws`, in examples and tests).
- `create` publishes no proof of work: slice 1's store admits a `#0` without one (identity plan
  PR 1.1); the brake and the `pow` step come in slice 3.

## Tests

```bash
cargo test -p ad4m-identity-client
cargo clippy -p ad4m-identity-client --all-targets --features test-vault -- -D warnings
cargo fmt -p ad4m-identity-client --check
cargo build -p ad4m-identity-client --target wasm32-unknown-unknown
cargo build -p ad4m-identity-client --example holder --features ws,test-vault
```

The flows against a real executor: `cd tests/js && pnpm run test-main-identity`.
