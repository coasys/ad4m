# Key-log cache and resolver

The executor's identity module keeps a verified copy of every key log it meets, in `KelCache`, and answers every "who is this key?" question from it through **one resolver**. `author_of(did:key)` maps a link author to its person at a link's anchored position; `KeyResolver::resolve_key` gives a key's verdict at the head for sessions, and `subscribe` pushes each change. The module reaches logs only through the agent language (`IdentityBackend`), and serves the `identity.*` RPC calls that an agent uses to make and read its own identity.

**Why:** A link check and a session check read the same cache and the same key map, so they never disagree about one key at one position. The cache refuses a shorter log or a different hash at a position it holds, so a store that serves an old log to this executor cannot roll back what it saw. Heads from the agent language must carry the store's signature; events from any other source need only their own signatures, so a neighbourhood, an app or another executor can carry them with no change here. An outage fails fast: a 5-second timeout and a circuit breaker per backend give `Unavailable`, which counting reads treat as no person.

**Parent:** [Key log](README.md)

<details><summary>How it works</summary>

| Cache rule | Effect |
|---|---|
| An anchor the cache covers | no network call |
| One fetch per identity at a time | concurrent checks wait on it |
| Accept only events that continue the cached head and pass `KelChain::append` (through the module's `import`) | `Gap` fetches from `have + 1`; `Fork` records the fork, raises `identity-fork`, appends nothing |
| A head from the agent language | must carry a valid `coasys:head` signature by a key in `identity.storeKeys` |
| `ingest(events)` from a neighbourhood, an app or another executor | the same checks, no head signature |
| "No log" answers | cached 15 minutes, so a later claim shows |
| Refresh triggers | a link anchored beyond the cache; a signed head that arrives through a neighbourhood; every 15 minutes for identities active here in the last day |

| Cache state | `author_of` | `resolve_key` |
|---|---|---|
| key in a log, valid at the head | `Person` or `Bot` | `Valid` (with `bot`) |
| revoked at or before the head | `Person { revoked }` | `Revoked` |
| `Until` key or bot scope past `expires` | `Person` or `Bot` | `Expired` |
| no log lists it | `NoLog` | `NotInLog` |
| lookup failed, nothing cached within `max_age` | `Unavailable` | `StoreUnavailable` |

The verdicts that links get from these answers are on [Link verdicts](../../../perspectives/links/verdicts.md). Sessions end on `subscribe` ([Key login](../../access/key-login.md)).

`identity.*` calls of this module:

| Method | Does |
|---|---|
| `identity.status` | this agent's `did:scid` or none, its device `did:key`, its `enc` key, key id, `vouched`, head, fork record |
| `identity.log`, `identity.resolveKey`, `identity.powParams` | a log through the cache; `author_of`; the store's proof-of-work cost |
| `identity.cosignIncept` | adds this agent's device-key proof to an `incept` that lists its key, while the agent has no identity |
| `identity.publish` | appends through the agent language, ingests the events, records the `did:scid` for this agent context; takes the store's log when the same events were appended elsewhere |
| `identity.attachNote` | passes a vouching-note request that an app signed |

A **device-only identity** (a guest, a desktop that chose "Start now", every agent from before identities) has no log: `author_of` answers `NoLog` and its links verify as before. It becomes a full identity when an `incept` lists its existing device key and the key co-signs it.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/identity/backend.rs](../../../../rust-executor/src/identity/backend.rs) (new): `IdentityBackend`, `IdentityError`, `Refusal`; the one implementation calls the agent language.
- [rust-executor/src/identity/cache.rs](../../../../rust-executor/src/identity/cache.rs) (new): `KelCache` (`refresh`, `ingest`, `state`) on `SqliteChainStore` in the executor database, `CacheError`.
- [rust-executor/src/identity/resolver.rs](../../../../rust-executor/src/identity/resolver.rs) (new): `Author`, `author_of`, `keys_of`, `check_anchor`, `KeyResolver`, `KeyResolution`, `KeyVerdict`, `VerdictChange`.
- [rust-executor/src/identity/service.rs](../../../../rust-executor/src/identity/service.rs) (new): the `identity.*` handlers above; `IdentityStatus`.
- [rust-executor/src/languages/capability.rs](../../../../rust-executor/src/languages/capability.rs): gains `Capability::Identity*`; the executor calls them only on the language at `system_addresses.agent_language`.
- [rust-executor/src/agent/mod.rs](../../../../rust-executor/src/agent/mod.rs): `AgentService.scid`; the event `identity-fork` goes to that identity's sessions.

</details>
