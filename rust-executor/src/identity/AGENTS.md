# identity/ — agent guide

The executor's side of `did:scid:coasys:1`: which
identity each local agent's device key speaks for, the key logs it checked, and the
`identity.*` calls (registered with their guards in `api/identity_ws.rs`).

| File | What |
|---|---|
| `mod.rs` | `init` (at start, after the database), `service()`, `IdentitySettings` (`identity.storeUrl`, `identity.storeKeys`), `write_agent_language_settings`, the `identity-fork` publisher |
| `backend.rs` | `IdentityBackend`, `IdentityError`; `AgentLanguageBackend` calls the agent language through `agent_language_for` (a JS envelope reads refusals back as `IdentityErrorCode`) |
| `cache.rs` | `KelCache`: events in `SqliteChainStore` (`coasys:kel`) in `ad4m_db.sqlite`, the accepted head in `identity_heads`, `refresh`, `ingest`, `accept`, forks |
| `head.rs` | `verify_head` (`ad4m_identity_core::HeadStatement` under a key of `identity.storeKeys`) |
| `device.rs` | the agent's device key, its X25519 `enc` key (`did_key`, no second secret), `WalletSigner` |
| `operator.rs` | `admits_first_link`, `first_link_check`, `record_operator_at_init` (`ad4m-executor init --operator`) |
| `claim.rs` | `ClaimService::claim_for_main` (`already-linked`, first-link rule, claim with the main wallet key, never `canKeepMaster`), `join_status` (the main agent's claims only) |
| `service.rs` | `IdentityService`: status, log, cosignIncept, publish, openOffer, offers, devices; error mapping |
| `types.rs` | the RPC contracts (ts-rs) mirroring `ad4m_ldk::identity`'s JSON, `IdentityForkEvent`, `AgentWithIdentity` |

Rows in `db/identity.rs`: `operator_record`, `identity_agents` (agent `did:key` → SCID),
`identity_joins` (claims to follow).

## Invariants

- Logs arrive only through the agent language, so a deployment can swap the store behind it;
  nothing here speaks HTTP.
- The cache only extends a log. A different event at a held seq, or a signed head below the
  cached one (a shorter log never truncates a held one), records a fork, appends nothing and fires
  `identity-fork` once per fork, to the sessions of the identity's local agents.
- A head from the agent language counts only under `identity.storeKeys`; `ingest` takes events
  from any source with no head.
- Every claim for the main agent goes through `ClaimService::claim_for_main`, which runs the
  first-link rule itself: `already-linked` while the main key is valid in a log, `no-operator`
  without a record, `first-link` for another identity.
- An agent keeps the first identity it joined (`identity_agents`); `agent.status` and
  `agent.get` carry it as `identity`, `did` stays the device `did:key`.

## Do not

- Do not read the words or a master key here, or add a call that carries one: the executor
  holds device keys only.
- Do not trust a store's hash or head: `import`/`fold` and `verify_head` decide.
