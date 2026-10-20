# Wallet and keys

The **wallet** stores every Ed25519 key of the executor by name: `main` for the main agent, one key per managed agent, and in hosted mode the **JWT signing key**. Each agent key is a **device key**: a key log lists it once the person links the executor. Its X25519 **encryption key** derives from the same key, so one secret serves signing and receiving sealed messages. The wallet has two backends:

- `LocalWallet` holds the keys in memory and writes them as an encrypted **keystore** into `agent.json`. Its passphrase is not typed: the executor derives it from the **storage secret** at each start.
- `SharedWallet` fetches keys over HTTP from a **hosting service**. It is never locked.

**Why:** Device keys come from random bytes, never from the 12 words, so an executor holds nothing that can change a key log. All keys of one executor sit in one keystore, so one unlock makes every hosted agent usable. Deriving the passphrase from the storage secret leaves one secret per executor off the data disk; a second, typed wallet password lost, because two secrets with the same rule double what an operator must keep. A device key never leaves its executor: a person moves by linking a new device, not by exporting a key. A managed agent's key is named by its DID, so no lookup goes through an email. A shared wallet cannot rename keys, so there a managed agent's key keeps the user's email as its name.

**Parent:** [Identity](README.md)

<details><summary>How it works</summary>

| Key name | Owner | Created by | Ends |
|---|---|---|---|
| `main` (`KEY_NAME_MAIN`) | main agent; `d1` or a later device key in the operator's log | `agent.generate` | `identity.rekey` makes a new one and keeps the old to read history |
| the managed agent's DID | a tenant, a password user or a guest | `host.join`, `user.create`, `guest.create` | destroyed when a tenant leaves or is revoked |
| the user's email (shared wallet only) | managed agent | the same, through `get_or_create_keypair` | — |
| `platform` (`KEY_NAME_PLATFORM`) or `wallet_signing_key_name` | JWT signing (hosted mode) | `create_new_keys`, or `unlock` if missing | — |

Life of the local keystore:

1. `storage.unlock` opens the keyring ([Keyring and storage secret](../executor/data/keyring.md)). The executor derives the agent passphrase with HKDF-SHA256 under its own tag `ad4m:agent-passphrase:v1`.
2. First start: `agent.generate` with no passphrase creates `main` and writes `agent.json` under the derived passphrase. Later starts: `agent.unlock` with no passphrase decrypts the keystore. Both are local only.
3. An existing wallet from before the storage secret moves once: `storage.migrateAgentPassphrase { oldPassphrase }` rewrites `agent.json` under the derived passphrase.
4. A legacy-format keystore is rewritten in the current format; a copy stays in `agent.json.legacy` until an unlock opens the new file directly.

| Keystore detail | Value |
|---|---|
| Key derivation (KDF) | Argon2id over the passphrase, random salt; the parameters travel with the cipher text |
| Cipher | XChaCha20-Poly1305, fresh nonce per write |
| File write | atomic (`write_json_atomically`) |
| Sealed messages | `open_sealed(name, sealed, info)` opens a message sealed to the key's X25519 half; the secret stays in the wallet |

DID documents leave the wallet with public keys only. A locked wallet makes `Session::open` fail with `AuthError::Locked` for every token except the admin credential ([Sessions](../access/sessions.md)).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/wallet.rs](../../../rust-executor/src/wallet.rs): `WalletBackend` (gains `open_sealed`), `LocalWallet`, `SharedWallet`, `KeystoreEnvelope`, `KEY_NAME_MAIN`, `KEY_NAME_PLATFORM`, `is_legacy_keystore`.
- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `AgentStore` (the `agent.json` shape), `AgentService::{create_new_keys, unlock, lock, try_save, load}`, `create_managed_agent_key`, `wallet_key_for_did`, `is_executor_key_name`, `write_json_atomically`.
- [rust-executor/src/at_rest/agent_passphrase.rs](../../../rust-executor/src/at_rest/agent_passphrase.rs) (new): `derive_agent_passphrase`, `AGENT_PASSPHRASE_TAG`.
- [rust-executor/src/api/agent_ws.rs](../../../rust-executor/src/api/agent_ws.rs): `agent.generate`, `agent.lock`, `agent.unlock` (local only; the passphrase is optional).
- [rust-executor/src/identity/service.rs](../../../rust-executor/src/identity/service.rs) (new): `identity.rekey`, `identity.status` (reports `deviceEnc`).
- [rust-executor/src/config.rs](../../../rust-executor/src/config.rs): `wallet_backend`, `wallet_backend_url`, `wallet_signing_key_name`, `signing_key_name()`.

</details>
