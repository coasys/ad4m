# Master key

The **master key** signs every change to a key log. It rests only where the person unlocks it: the desktop app's custody item behind the OS unlock, the phone's vault behind biometrics, or, for an email user, a record wrapped under a key that only the password produces. It never rests in an executor. When it travels to an app, it is sealed to that app's own device encryption key and sent through the device inbox. On the desktop that key belongs to the executor's wallet, so the executor opens the message on the custody service's call and returns the key in that one reply. The **identity client core** in this repository runs every flow that touches the words, the password or the master key, for every surface.

**Why:** An executor runs language code from the network and may serve other people, so it must not keep the key that adds and revokes keys. Every key step runs apart from every app (a separate OS process on the desktop and phone, a separate site on the web), so a bug in the chrome or a template cannot reach the key. A second "custody key" per app, which would let the executor store the message but never open it, lost: the device's own encryption key receives the master key, and the claim's receiving key could take a custody key with no format change. Losing one copy costs nothing while another remains, and the 12 words always make the next master key.

**Parent:** [Devices](README.md)

<details><summary>How it works</summary>

| Surface | Master key rests in | Opened by |
|---|---|---|
| Desktop app (custody service in the shell's main process) | a custody item in the OS keystore | Touch ID or the login password; a passphrase on Linux |
| Phone app (the shell's native side) | the vault plugin | biometrics or the device passcode |
| Browser, email user (the auth domain) | a non-extractable WebCrypto key; the durable copy is the wrapped record on the platform | the password |
| The desktop's executor | never at rest; in memory between opening a `Master` message and replying | the custody service's `identity.takeMaster` |
| Any other executor, a bot, the chrome, the platform in clear | never | — |

The hand-over on the desktop, executor side:

1. The shell starts the executor with `--custody-channel`. Without the flag, `identity.takeMaster` answers `403 no-custody-channel`.
2. After the computer's unlock, the custody service calls `identity.takeMaster {}` with the admin credential over the shell's loopback channel (`OperatorSource::Credential` only, local only).
3. The executor opens the oldest unacknowledged `Master` message with `WalletBackend::open_sealed`, runs `check_payload`, returns `{messageId, did, index, secret}`, and zeroes its copy. A `Stale` message is acknowledged and answers `null`.
4. The custody service stores the key, then calls `identity.inboxAck`. The executor writes the secret to no store, log, event or language.

The client core is a state machine over four interfaces, so the same code runs natively and as WASM: `Vault` (where keys rest; `with_unlocked` gives one prompt per flow), `StoreClient` (`DirectStore` over HTTP for the phone and web, `ViaExecutor` over executor RPC for the desktop), `AccountClient` (the wrapped-key store) and `Prompt`. Its only-copy check refuses to remove the master key from a device when no other device or current wrapped record holds it; the words do not count as a copy.

</details>

<details><summary>Where in the code</summary>

- [identity-client/](../../../../identity-client) (new): crate `ad4m-identity-client`, published as `@coasys/identity-client` for WASM. `src/vault.rs` (`Vault`, `MasterSigner`, `MasterReceiver`, `MasterUse`), `src/store.rs` (`DirectStore`, `ViaExecutor`), `src/account.rs` (wrap and unwrap), `src/copies.rs` (the only-copy check), `src/flows/` (create, login, take_master, rotate_custody, restore_custody, …), `src/secret.rs`.
- [rust-executor/src/identity/service.rs](../../../../rust-executor/src/identity/service.rs) (new): `identity.takeMaster`; the custody-channel flag from the run arguments.
- [rust-executor/src/wallet.rs](../../../../rust-executor/src/wallet.rs): `open_sealed`, which keeps the X25519 secret inside the wallet.
- The custody service, the identity window and the vault implementations live in the desktop and phone shells of the apps repository; the auth domain is its own site. They call this repository's client core and the executor's RPC.

</details>
