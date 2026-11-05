# Wallet and keys

The **wallet** stores every Ed25519 key of the executor by name: `main` for the main agent, one key per managed agent, and in hosted mode the **JWT signing key**. It has two backends:

- `LocalWallet` holds the keys in memory and writes them as an encrypted **keystore** into `agent.json`. It is **locked** until the operator gives the passphrase. A locked wallet cannot sign or check a token.
- `SharedWallet` fetches keys over HTTP from a **hosting service**. It is never locked.

**Why:** All keys of one executor sit in one keystore under one passphrase, so one unlock makes every hosted agent usable. A managed agent is an account on the executor: in a local wallet the operator's passphrase encrypts its key, and the code has no key export. A managed agent's key is named by its DID, so no lookup goes through an email. A shared wallet cannot rename keys, so there a managed agent's key keeps the user's email as its name.

**Parent:** [Identity](README.md)

<details><summary>How it works</summary>

| Key name | Owner | Created by |
|---|---|---|
| `main` (`KEY_NAME_MAIN`) | main agent | `AgentService::create_new_keys` on `agent.generate` |
| the managed agent's DID | managed agent | `AgentService::create_managed_agent_key` on `user.create` |
| the user's email (shared wallet only) | managed agent | same, through `get_or_create_keypair` |
| `platform` (`KEY_NAME_PLATFORM`) or `wallet_signing_key_name` | JWT signing (hosted mode) | `create_new_keys`, or `unlock` if missing |

Life of the local keystore:

1. `agent.generate { passphrase }`: create `main` and write `agent.json` (`AgentStore`: DID, DID document, signing key id, encrypted `keystore`, main profile).
2. Restart: `AgentService::load` reads `agent.json`. The wallet holds only the cipher text.
3. `agent.unlock { passphrase }`, or `AD4M_UNLOCK_PASSPHRASE_FILE` at startup (`unlock_agent_at_startup`): decrypt the keystore. A legacy-format keystore is rewritten in the current format. A copy stays in `agent.json.legacy` until an unlock opens the new file directly.
4. `agent.lock { passphrase }`: save `agent.json` under the unlock passphrase. Then encrypt the keys in memory under the given passphrase and drop the plain keys.

| Keystore detail | Value |
|---|---|
| Key derivation (KDF) | Argon2id over the whole passphrase, random salt; the parameters travel with the cipher text |
| Cipher | XChaCha20-Poly1305, fresh nonce per write |
| File write | atomic (`write_json_atomically`) |

DID documents leave the wallet with public keys only (`DID_DOCUMENT_CONFIG`). `AgentService::load` strips private keys that older executors wrote into `agent.json`.

A locked wallet makes `Session::open` fail with `AuthError::Locked` for every token except the admin credential. The WebSocket closes with 1013; HTTP answers 503 ([Sessions](../access/sessions.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/wallet.rs](../../../rust-executor/src/wallet.rs): `WalletBackend` trait, `LocalWallet`, `SharedWallet`, `KeystoreEnvelope`, `KEY_NAME_MAIN`, `KEY_NAME_PLATFORM`, `is_legacy_keystore`.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `AgentStore` (the `agent.json` shape), `AgentService::{create_new_keys, unlock, lock, try_save, load}`, `create_managed_agent_key`, `wallet_key_for_did`, `is_executor_key_name`, `write_json_atomically`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `agent.generate`, `agent.lock`, `agent.unlock`, `unlock_main_agent`.
- [rust-executor/src/lib.rs](../../../rust-executor/src/lib.rs): wallet backend choice in `run()`, `unlock_agent_at_startup`.
- [rust-executor/src/config.rs](../../../rust-executor/src/config.rs): `wallet_backend`, `wallet_backend_url`, `wallet_signing_key_name`, `signing_key_name()`.

</details>
