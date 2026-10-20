# Identity interface

The **identity interface** (`IdentitySpec` in TypeScript, `IdentityCapability` in Rust) is the set of `identity*` calls that an agent language exports so that the executor can reach identities. It has two groups: the **key-log calls** (log, head, append, key owner, proof-of-work cost, vouching note, agent record) and the **device calls** (offers, the device inbox, the private record, the request box, bot pause records). The executor makes these calls only on the language at `system_addresses.agent_language`, through its `IdentityBackend`. What the key log is and how the executor checks it: [Key log](../../../agents/identity/key-log/README.md).

**Why:** One interface lets any store or network carry identities with the same calls and admission rules, and no change to the executor. Events cross it as **opaque JSON objects**, so the language needs neither the hash-chain module nor the key-log format; the executor and the store parse events with the identity core. The option that lost was typed `Event` values in the interface: the interface could not land before the key-log format.

**Parent:** [Agent language](README.md)

<details><summary>How it works</summary>

| Call (flat export) | Shared-store route (default agent language) |
|---|---|
| `identityLog(did, from)` | `GET /v1/logs/{scid}?from=` |
| `identityHead(did)` | `GET /v1/heads/{scid}` |
| `identityAppend(did, req)` | `POST /v1/logs/{scid}/events` |
| `identityKeyOwner(multikey, waitSec)` | `GET /v1/keys/{multikey}?wait=` (0–8 s) |
| `identityPowParams()` | `GET /v1/pow` |
| `identityAttachNote(did, token, req)` | `POST /v1/notes/{scid}` |
| `identityPutAgent(record, req?)` | `PUT /v1/agents/{did}` |
| `identityOpenOffer`, `…ListOffers`, `…CancelOffer`, `…ClaimOffer`, `…OfferState` | `/v1/offers/{scid}…` |
| `identityInboxPut`, `…InboxList`, `…InboxAck` | the device inbox, before and after a device joins |
| `identityGetRecord`, `…PutRecord`; `…PostRequest`, `…ListRequests`, `…DeleteRequest`; `…GetPause`, `…PutPause` | private record, request box, bot pause records |

Rules for every implementation:

1. Sign nothing, hold no key. Signed events and signed requests pass through byte for byte; the `X-Coasys-Request` header comes from the request.
2. Map HTTP statuses one to one onto `IdentityErrorCode`: `conflict` (with the store's head), `key-taken`, `forbidden` (with a reason), `invalid`, `rate-limited`, `gone`, `not-found`. A network failure or the host's 10-second `httpFetch` limit gives `unavailable`. A wait never passes 8 s, so it fits inside one call; the executor loops for longer waits.
3. Read the store's base URL from the language's settings, never from a constant.
4. Enforce the append rules of the interface (continuity at the head, a vouching note or the brake for a new log, replay, one key in one identity, no bot logs, an `add` spends a claim). For the default agent language, the shared store enforces them ([Shared store](shared-store.md)).
5. The wire types (`LogPage`, `SignedHead`, `AppendRequest`, `KeyIndexEntry`, `SignedRequest`, `AgentRecord`, `VouchNote`, `PowParams`, and the device types) live in the ALDK, in TypeScript and Rust.

Each call is one `Capability::Identity*` value ([Language interface](../../runtime/interface.md)).

</details>

<details><summary>Where in the code</summary>

- [ad4m-ldk/js/src/types.ts](../../../../ad4m-ldk/js/src/types.ts): `IdentitySpec`, `IdentityErrorCode`, the wire types; [ad4m-ldk/js/src/defineLanguage.ts](../../../../ad4m-ldk/js/src/defineLanguage.ts): maps `identity` to the flat exports.
- [ad4m-ldk/rust/src/traits.rs](../../../../ad4m-ldk/rust/src/traits.rs): `IdentityCapability`; [ad4m-ldk/rust/src/identity.rs](../../../../ad4m-ldk/rust/src/identity.rs) (new): the wire types.
- [rust-executor/src/languages/capability.rs](../../../../rust-executor/src/languages/capability.rs): `Capability::Identity*`.
- [rust-executor/src/identity/backend.rs](../../../../rust-executor/src/identity/backend.rs) (new): `IdentityBackend`, `IdentityError`, `Refusal`.
- [bootstrap-languages/centralized-agent-language/index.ts](../../../../bootstrap-languages/centralized-agent-language/index.ts), [rust-impl/src/lib.rs](../../../../bootstrap-languages/centralized-agent-language/rust-impl/src/lib.rs): the store-backed implementation; one shared test suite runs against both and a real `identity-store`.

</details>
