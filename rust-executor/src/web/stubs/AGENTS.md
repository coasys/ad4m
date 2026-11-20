# web/stubs/ — agent guide

The modules the web executor (wasm32) does not run, as hand-written stubs. `lib_wasm.rs`
declares each by path at the crate root (`crate::languages`, `crate::ai_service`, ...), so
ring 2 compiles unchanged. `perspectives/mod.rs` declares `interpretation.rs` and
`agent_session.rs` the same way, next to S7's native-only gates.

| File | Real (native file by `#[path]`) | Stubbed |
|---|---|---|
| `languages/` | `capability`, `error`, `literal` | `LanguageController` (every call a `LanguageError`), `Language` (empty capability set: answers as native does for a language without the capability) |
| `ai_service.rs` | — | `AIService` (never there), config AI keys as opaque JSON, `CreditGate`, `ProgressEvent`, `Backend`, three pure helpers |
| `ai_sessions.rs` | `types` | `ai_session_store()` (a database lookup, as native) |
| `identity.rs` | `types` | `service()` always 503, no `did:scid` |
| `runtime_service.rs` | `notification_access` | `RuntimeService` without a seed |
| `holochain_service.rs`, `email_service.rs` | — | errors |
| `sfu.rs`, `config_rpc.rs` | `sfu/types.rs` | payload types only |
| `mcp.rs` | `shacl` | — |
| `interpretation.rs`, `agent_session.rs` | `interpretation/types.rs` | every run and overlay call an error; two pure helpers copied |
| `mod.rs` | — | `NotAvailable`, the error every stub returns |

Rules:
- Only the items ring 2 names. Add an item when the wasm32 check asks for it, not before.
- Same signatures as native. A stub returns `NotAvailable` ("... is not available in the web
  executor") or the native "absent" answer; it never panics (no `unimplemented!`, `todo!`,
  `unwrap`).
- A copied helper says which native function it copies; keep the two the same.

Check: `bash scripts/web-executor/check.sh` (from the repo root).
