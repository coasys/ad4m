# Holochain service

The **Holochain service** (`HolochainService`) runs an embedded Holochain **conductor** on its own thread and Tokio runtime. Rust code and language isolates reach it through one actor channel (`HolochainServiceInterface`). Languages see it as the `holochain*` functions of `ad4m:host`. The Unyt payment DNA also runs on it. The conductor has one agent key for the executor: `HolochainService::get_agent_key` takes the first key of the conductor keystore, so every agent on the executor uses the same cell per DNA.

**Why:** Holochain languages need a conductor: p-diff-sync, the Holochain agent language and the Holochain file-storage language. A conductor inside the executor removes a second daemon. The actor channel lets the conductor's app set change under a write lock while zome calls run concurrently.

**Parent:** [Executor services](README.md)

<details><summary>How it works</summary>

1. On agent generate or unlock, `conductor_startup` starts `HolochainService::init` in a background task and loads the system languages beside it. The RPC reply may go out before the conductor is up. `run_holochain: false` skips this step ([Executor startup](../startup.md)).
2. A language calls `holochainRegisterDnas([{nick, bundle}])`. The language installs a hApp (Holochain app) for itself and registers its cells in `HOLOCHAIN_SIGNAL_HANDLERS` (cell id → language address). Templating sets each copy's DNA network seed and properties ([Language trust and templates](../../../languages/runtime/installing/trust-and-templates.md)). The `install_app` op waits up to 45 s for the install (`APP_INSTALL_TIMEOUT_DURATION`); the dispatcher lets it run up to 120 s (`INSTALL_APP_TIMEOUT`), so it never cancels an install the op still waits for. An install over 30 s is logged, because every other Holochain request waits behind it.
3. `holochainCall(nick, zome, fn, params)` becomes a `HolochainServiceRequest`. `run_dispatch_loop` runs it in one of three lanes, each with a timeout and a queue deadline:

   | Request kind | Lane |
   |---|---|
   | lifecycle | inline, under a write lock |
   | local | a lane of 4 |
   | zome call | a lane of 32 |

4. Signals from all cells go into one `StreamMap`. `lib.rs::holochain_signal_receiver` routes each signal to its language's `__handleHolochainSignal__`, or to `unyt_service::handle_signal` for the alliance cell.

| Accessor | Behaviour |
|---|---|
| `maybe_get_holochain_service()` | answers now |
| `holochain_service_once_started()` | waits up to 120 s, then `None` |
| `get_holochain_service()` | waits up to 120 s, then panics; code off the boot path uses the other two |

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/holochain_service/AGENTS.md](../../../../rust-executor/src/holochain_service/AGENTS.md): rules for adding requests.
- [rust-executor/src/holochain_service/mod.rs](../../../../rust-executor/src/holochain_service/mod.rs): `init`, `install_app`, `call_zome_function`, `get_agent_key`, pack/unpack.
- [rust-executor/src/holochain_service/interface.rs](../../../../rust-executor/src/holochain_service/interface.rs): `HolochainServiceRequest`, `Envelope`, the three accessors. A request is an enum variant, `name`/`refuse` arms, a dispatch arm and an interface method; `is_lifecycle` puts it in the write-lock lane.
- [rust-executor/src/holochain_service/dispatch/mod.rs](../../../../rust-executor/src/holochain_service/dispatch/mod.rs): `run_dispatch_loop`, `ConductorDispatch`.
- [rust-executor/src/holochain_service/conductor_startup.rs](../../../../rust-executor/src/holochain_service/conductor_startup.rs): start on unlock.
- [rust-executor/src/holochain_service/holochain_service_extension.rs](../../../../rust-executor/src/holochain_service/holochain_service_extension.rs): the 14 ops.
- [rust-executor/src/js_core/language_bootstrap.js](../../../../rust-executor/src/js_core/language_bootstrap.js): `createHolochainDelegate`, signal registration.
- [rust-executor/Cargo.toml](../../../../rust-executor/Cargo.toml): the Holochain dependency, a coasys fork branch (`0.7.0-space-override-coasys`).

</details>
