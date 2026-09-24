# holochain_service/ — agent guide

Embedded Holochain conductor run on a dedicated thread + tokio runtime, driven
through an actor channel. Split plan: spec item 10.

| File | Role |
|---|---|
| `mod.rs` | `HolochainService::init` (spawns the thread, builds the `StreamMap` signal fan-in, spawns the dispatch loop), conductor construction, `install_app`, `call_zome_function`, agent infos, sign, pack/unpack dna+happ |
| `dispatch/mod.rs` | `run_dispatch_loop` (the request loop: lifecycle requests inline under a write lock, everything else spawned under a read guard + `ZOME_CALL_CONCURRENCY` permits, time-in-queue log, deadline refusal), the `ZomeDispatch` trait, `ConductorDispatch` (the big `match` over `HolochainServiceRequest`, one arm per variant with its timeout). `dispatch/tests.rs` checks the loop against a mock, no conductor. |
| `interface.rs` | `HolochainServiceInterface` (channel sender + signal receiver), `Envelope` (request + `queued_at` + `deadline`), `HolochainServiceRequest`/`Response` enums with `name`/`is_lifecycle`/`refuse`, one async method per request, the global service and its three accessors |
| `holochain_service_extension.rs` + `.js` | 15 `#[op2]` ops exposed to Languages (`ad4m:host` holochain section) |

Adding a zome-level operation currently means four edits: request enum variant
(plus its `name`/`refuse` arms), dispatch arm in `dispatch/mod.rs::ConductorDispatch`,
method in `interface.rs`. Keep them in sync until item 10 collapses them. A new
variant that mutates the conductor's app set must also be added to `is_lifecycle`,
or it will run concurrently with zome calls (#1133).

## Facts

- `HolochainService::init` is invoked from `agent/conductor_startup.rs`, in a task that
  agent generate/unlock spawn before replying, not from `lib.rs`. The reply may go out
  before the conductor is up, so nothing that runs during or straight after unlock may
  assume it is. The `start_holochain_conductor` op in `holochain_service_extension.rs`
  also calls `init`, bypassing both the in-flight claim and `ConductorStarting`; it has
  no in-repo JS caller, so don't add one.
- Signals are consumed by `lib.rs::holochain_signal_receiver` and routed to the language
  runtime registered for that cell.
- Three accessors, all in `interface.rs`: `maybe_get_holochain_service()` returns at once;
  `holochain_service_once_started()` waits while a start is in progress (the
  `ConductorStarting` guard), up to 120 s, then returns `None`; `get_holochain_service()`
  polls up to 120 s then **panics**, so prefer the other two in anything not on the
  critical boot path.
- DHT is full-arc: use `GetStrategy::Local`; cross-agent flakiness is gossip timing
  (see root `AGENTS.md`). K2 spaces exist only after `join`; `add_agent_infos` on a
  missing space returns `K2SpaceNotFound`.
- Local test networking: bootstrap and relay URLs must both be `http://` for a local
  bootstrap-srv, or cross-node traffic silently dies.
