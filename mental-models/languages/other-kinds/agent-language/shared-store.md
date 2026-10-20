# Shared store

The **shared store** (`identity-store`) is a Rust service, separate from the executor, that the default agent language talks to. It keeps every key log, a **signed head** per log, the **key index** (which identity holds a key, and whether the key is still valid), vouching notes, agent records, and the device-linking data (offers, device inboxes, request boxes, private records, sign-up pairings). It runs on the identity core and the hash-chain module, with one SQLite file. It appends an event only when the event continues the current head and replays; it can withhold an event, never forge one.

**Why:** Every party needs one place to find a log by its `did:scid` and a key's owner. The store is trusted to stay up, not to be complete or honest: every event carries the identity's own signatures, and every executor checks them again and refuses a head that goes back. A new log with no email check passes a **brake** (rate limits and a proof of work) in place of an account, so a desktop identity needs no email. The development profile directory at `mock.ad4m.dev` retires after a one-time copy of its records into the store.

**Parent:** [Agent language](README.md)

<details><summary>How it works</summary>

| Route | Does |
|---|---|
| `POST /v1/logs/{scid}/events` | appends; `409` with the current head when the log moved, `409 key-taken` when a key already sits in any log |
| `GET /v1/logs/{scid}?from=`, `GET /v1/heads/{scid}` | a `LogPage` with its signed head; the head alone |
| `GET /v1/keys/{multikey}?wait=` | a `KeyIndexEntry`; waits up to 8 s for a new device's key |
| `GET /v1/pow`, `POST /v1/notes/{scid}` | the brake's proof-of-work cost; a vouching note beside a log |
| `GET` / `PUT /v1/agents/{did}` | agent records for `did:scid` (a signed request) and `did:key` (signed by that key) |
| `/v1/offers/…`, `/v1/inbox/…`, `/v1/private/…`, `/v1/requests/…`, `/v1/pauses/…`, `/v1/pair/…` | device linking and web sign-up ([Key log](../../../agents/identity/key-log/README.md)) |

1. Logs sit in the hash-chain module's tables (`chain_entries`, `chain_heads`, `chain_forks`, type `coasys:kel`); one transaction holds each append with its head, key-index rows and any claim it spends ([Hash-chain module](../../../agents/identity/key-log/hash-chain.md)).
2. Every head carries a `coasys:head` signature by the store's key. Executors check it against their config's `identity.storeKeys`; the link server against `STORE_HEAD_KEYS`.
3. The brake for a log with no note: 120 an hour store-wide, 3 a day per network address, and a 22-bit proof of work bound to the SCID, all configurable.
4. A signed request (for notes, agent records and the device routes) names a key valid at the store's head, with no bot mark, a nonce and an expiry at most 5 minutes ahead.
5. `identity-store keygen` makes the head key; `identity-store import-agents` copies the old profile directory's `did:key` records that verify.
6. Callers: the agent language and the link server. The auth domain appends to the store directly, not through a language. No executor code calls the store's routes directly; the executor goes through the agent language ([backend.rs](../../../../rust-executor/src/identity/backend.rs) (new)).

</details>

<details><summary>Where in the code</summary>

- [identity-store/](../../../../identity-store) (new): the binary, its SQLite schema on `SqliteChainStore`, routes, the brake, the pairing routes; `identity-store/tests` runs a real store on a temporary file.
- [ad4m-identity-core/](../../../../ad4m-identity-core) (new): event replay, `derive_scid`, proof of work, note and head checks.
- `ad4m-hash-chain`: `ChainStore`, `SqliteChainStore::append_in`.
- [ad4m-ldk/rust/src/identity.rs](../../../../ad4m-ldk/rust/src/identity.rs) (new): the wire types the store, the executor and the language share.

</details>
