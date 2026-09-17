# holochain_service/ — agent guide

Embedded Holochain conductor run on a dedicated thread + tokio runtime, driven
through an actor channel. Split plan: spec item 10.

| File | Role |
|---|---|
| `mod.rs` | `HolochainService::init` (spawns the thread, builds the `StreamMap` signal fan-in, and the big `match` dispatch over `HolochainServiceRequest`), conductor construction, `install_app`, `call_zome_function`, agent infos, sign, pack/unpack dna+happ |
| `interface.rs` | `HolochainServiceInterface` (channel sender + signal receiver), `HolochainServiceRequest`/`Response` enums, one async method per request, global `get_holochain_service()` / `maybe_get_holochain_service()` |
| `holochain_service_extension.rs` + `.js` | 15 `#[op2]` ops exposed to Languages (`ad4m:host` holochain section) |

Adding a zome-level operation currently means three edits: request enum variant,
dispatch arm in `mod.rs::init`, method in `interface.rs`. Keep them in sync until
item 10 collapses them.

## Facts

- `HolochainService::init` is invoked from `api/agent_ws.rs` (agent generate/unlock),
  not from `lib.rs`. Signals are consumed by `lib.rs::holochain_signal_receiver` and
  routed to the language runtime registered for that cell.
- `get_holochain_service()` polls up to 120 s then **panics**; prefer
  `maybe_get_holochain_service()` in anything not on the critical boot path.
- DHT is full-arc: use `GetStrategy::Local`; cross-agent flakiness is gossip timing
  (see root `AGENTS.md`). K2 spaces exist only after `join`; `add_agent_infos` on a
  missing space returns `K2SpaceNotFound`.
- Local test networking: bootstrap and relay URLs must both be `http://` for a local
  bootstrap-srv, or cross-node traffic silently dies.
