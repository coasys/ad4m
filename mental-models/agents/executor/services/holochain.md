# Holochain service

The **Holochain service** (`HolochainService`) runs an embedded Holochain **conductor** on its own thread and Tokio runtime. Rust code and language isolates reach it through one actor channel (`HolochainServiceInterface`). Languages see it as the `holochain*` functions of `ad4m:host`. The Unyt payment DNA also runs on it. The conductor has one agent key per language and agent (`agent_key_for`, kept in the `settings` table as `language_agent_key:<address>` for the main agent and `language_agent_key:<address>:<did>` for a managed agent): two languages with the same DNA get two cells, and a managed agent's calls go to a **user cell** of its own. System languages use only the main agent's cells.

**Why:** Holochain languages need a conductor: p-diff-sync, the Holochain agent language and the Holochain file-storage language. With the conductor inside the executor, no second daemon runs. The actor channel lets the conductor's app set change under a write lock while zome calls run concurrently. A user cell has the same DNA hash as the main agent's cell, so it joins the same network and reads the same DHT, but it writes and signs as the managed agent's own Holochain agent.

**Parent:** [Executor services](README.md)

<details><summary>How it works</summary>

1. On agent generate or unlock, `conductor_startup` starts `HolochainService::init` in a background task and loads the system languages beside it. The RPC reply may go out before the conductor is up. `run_holochain: false` skips this step ([Executor startup](../startup.md)).
2. A language calls `holochainRegisterDnas([{nick, bundle}])` in its `init`, as the main agent. The language installs one hApp (Holochain app) per DNA, app id `<language address>-<nick>`, under the main agent's key for the language, and registers its cells in `HOLOCHAIN_SIGNAL_HANDLERS` (cell id → language address). Templating sets each copy's DNA network seed and properties ([Language trust and templates](../../../languages/runtime/installing/trust-and-templates.md)). The `install_app` op waits up to 45 s for the install (`APP_INSTALL_TIMEOUT_DURATION`); the dispatcher lets it run up to 120 s (`INSTALL_APP_TIMEOUT`), so it never cancels an install the op still waits for. An install over 30 s is logged, because every other Holochain request waits behind it.
3. The `installApp`, `getAppInfo` and `callZomeFunction` ops act as the agent of the call the code runs for (its call token, [Language isolates](../../../languages/runtime/isolates.md)); with no token they refuse. For a managed agent, `UserCells::app_for_call` maps the app id to the agent's own app, `<app id>@<did>`, and installs it from the main app's payload on the agent's first call. An app id with `@` is reserved to these apps: `installApp` refuses it from a language. `holochainAppInfo(nick)` answers the caller's own app, whose `agent_pub_key` is the caller's Holochain agent in the language.
4. `holochainCall(nick, zome, fn, params)` becomes a `HolochainServiceRequest`. `run_dispatch_loop` runs it in one of three lanes, each with a timeout and a queue deadline:

   | Request kind | Lane |
   |---|---|
   | lifecycle | inline, under a write lock |
   | local | a lane of 4 |
   | zome call | a lane of 32 |

5. Signals from all cells go into one `StreamMap`. `lib.rs::holochain_signal_receiver` routes each signal to its language's `__handleHolochainSignal__`, or to `unyt_service::handle_signal` for the alliance cell. A user cell's signal goes to its language as that managed agent (`UserCells::route_for_cell`).

| Accessor | Behaviour |
|---|---|
| `maybe_get_holochain_service()` | answers now |
| `holochain_service_once_started()` | the service at once if it is running; while a conductor start is in progress, waits up to 120 s; with no start in progress, `None` at once |
| `get_holochain_service()` | waits up to 120 s, then panics; code off the boot path uses the other two |

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/holochain_service/AGENTS.md](../../../../rust-executor/src/holochain_service/AGENTS.md): rules for adding requests.
- [rust-executor/src/holochain_service/mod.rs](../../../../rust-executor/src/holochain_service/mod.rs): `init`, `install_app`, `call_zome_function`, pack/unpack.
- [rust-executor/src/holochain_service/interface.rs](../../../../rust-executor/src/holochain_service/interface.rs): `HolochainServiceRequest`, `Envelope`, `agent_key_for`, `agent_key_for_language` (the main agent), the three accessors. A request is an enum variant, `name`/`refuse` arms, a dispatch arm and an interface method; `is_lifecycle` puts it in the write-lock lane.
- [rust-executor/src/holochain_service/dispatch/mod.rs](../../../../rust-executor/src/holochain_service/dispatch/mod.rs): `run_dispatch_loop`, `ConductorDispatch`.
- [rust-executor/src/holochain_service/user_cells.rs](../../../../rust-executor/src/holochain_service/user_cells.rs): `UserCells` (`app_for_call`, `route_for_cell`, `mark_system_language`), `user_app_id`.
- [rust-executor/src/holochain_service/conductor_startup.rs](../../../../rust-executor/src/holochain_service/conductor_startup.rs): start on unlock.
- [rust-executor/src/holochain_service/holochain_service_extension.rs](../../../../rust-executor/src/holochain_service/holochain_service_extension.rs): the 15 ops.
- [rust-executor/src/js_core/language_bootstrap.js](../../../../rust-executor/src/js_core/language_bootstrap.js): `createHolochainDelegate` (`registerDNAs`: the language's agent key, the installs), signal registration.
- [rust-executor/Cargo.toml](../../../../rust-executor/Cargo.toml): the Holochain dependency, a coasys fork branch (`0.7.1-space-override-coasys`).

</details>
