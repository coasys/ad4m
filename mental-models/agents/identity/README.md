# Identity

An agent's identity is a `did:key` DID that contains its Ed25519 public key. The executor keeps the secret key in its **wallet** and signs as the agent. Any executor verifies a signature from the DID alone. The agent's **agent profile** is published as an expression of the **agent language**.

**Why:** A `did:key` DID needs no registry. The public key is inside the identifier, so verification needs no network call. Profiles go through a language, so their storage can change without a change to the executor.

**Parent:** [Agents](../README.md)

## Parts
- [Wallet and keys](wallet-and-keys.md): the store of every agent's key, its file on disk, and how it is locked and unlocked.
- [Signing and verification](signing-and-verification.md): how the executor signs expressions and messages as an agent and checks them; entanglement proofs.
- [Agent profiles](profiles.md): the `Agent` record, where it is stored, how it is published and looked up.

<details><summary>How it works</summary>

```
AgentContext ──wallet_key_name()──► wallet key ("main", or the managed agent's DID)
     │                                  │
     │                                  ├── get_did_document() ──► DID ("did:key:z6Mk…")
     │                                  └── sign(payload) ──► Ed25519 signature
     ▼
create_signed_expression(data, ctx) ──► Expression { author: DID, timestamp, data, proof }
     │
     ▼
agent language ◄── publish_agent_to_language(ctx) ── Agent { did, perspective, directMessageLanguage }
```

1. `AgentContext::Main` uses the wallet key `main`. `AgentContext::Managed { did }` uses the key that `AgentService::wallet_key_for_did` finds for the DID. Each agent has one key, and its DID contains that key, so the code has no key rotation.
2. An agent has two code shapes. `AgentService`, a global singleton, holds the main agent (`did`, `agent`, `signing_key_id`). A managed agent is a `users` row with a wallet key, reached through static functions in `agent/mod.rs` ([Managed agents](../executor/managed-agents.md)).
3. Rust code passes the `AgentContext` explicitly. Language code signs as the agent that its runtime runs for at that moment (`get_runtime_agent_context()`); the ops `agent_did` and the signing ops follow it. The ops `agent_did_document` and `agent` return the main agent's DID document and profile ([agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs)).
4. Entanglement proofs are for the main agent only ([Signing and verification](signing-and-verification.md)).
5. The executor publishes the main agent's profile after `agent.generate`, `agent.unlock` and each `agent.updateProfile`. It publishes a managed agent's profile after `user.create` and after each `agent.updateProfile` in that agent's session.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/agent/mod.rs](../../../rust-executor/src/agent/mod.rs): `AgentContext`, `AgentService`, `did_for_context`, `sign_for_context`, `create_signed_expression`.
- [rust-executor/src/agent/signatures.rs](../../../rust-executor/src/agent/signatures.rs): verification.
- [rust-executor/src/wallet.rs](../../../rust-executor/src/wallet.rs): `WalletBackend`, `LocalWallet`, `SharedWallet`.
- [rust-executor/src/js_core/agent_extension.rs](../../../rust-executor/src/js_core/agent_extension.rs): the agent ops that language code calls.
- [bootstrap-languages/agent-language/](../../../bootstrap-languages/agent-language): the default agent language (see [Languages](../../languages/README.md)).
- [docs-src/pages/agents.mdx](../../../docs-src/pages/agents.mdx): user-facing description.

</details>
