# identity-store — agent guide

The shared store of `did:scid:coasys:1` key logs with the offers and claims of device linking.
One Rust binary, one SQLite file. Executors reach it only through the agent language
(`bootstrap-languages/centralized-agent-language`), so an executor never speaks HTTP to a
store and a deployment can swap the store behind the language; the auth domain calls it
directly.

It replays every event with the identity core before it stores it, so it can withhold an
event but never forge one, and every client checks the events' signatures again. It signs
heads with its own Ed25519 key (domain `coasys:head`).

HTTP stack: **axum 0.8** on tokio and hyper, with tower-http's CORS layer (all already in the
workspace's `Cargo.lock`); rusqlite 0.40 on the system `libsqlite3`.

| File | What |
|---|---|
| `src/main.rs` | `identity-store [serve]`, `identity-store keygen`; configuration from the environment |
| `src/lib.rs` | `Store` (database, head key, clock, settings, the change signal for waits), `Settings`, `Clock`, `Fault` |
| `src/db.rs` | the schema (the module's `chain_*` tables, type `coasys:kel`, and `logs`, `heads`, `keys`, `agents`, `nonces`, `feed`, `offers`, `claims`), log replay, key-index rows |
| `src/log.rs` | `POST …/events` with the admission checks (below), log pages, heads, the key index; `admit_new_log` (the brake's place) |
| `src/offers.rs` | the offer routes, the claim route, the device's state poll, the store's side of the claim check (`check_spend`, `spend`, `supersede`) |
| `src/request.rs` | signed requests (`X-Coasys-Request`): checks, nonce replay guard, `percent_decode` |
| `src/agents.rs` | agent records under a `did:key` or a `did:scid`; the store checks the record signature and never reads `profile` (the agent language writes a `did:key` agent's signed AD4M expression there) |
| `src/head.rs` | `StoreKey` (key file, `keygen`), `sign_head` (the core's `HeadStatement`; executors verify it with the same type) |
| `src/routes.rs` | the axum router, body limit, CORS, waits, the feed (token compared in constant time) |
| `src/error.rs` | `ApiError`: every refusal as a status and a JSON body with an `error` name |
| `tests/common/agent.rs` | a test person: fixed keys, a log built with the core's builders, offers, claims, signed requests, records |
| `tests/common/mod.rs` | `TestStore`: a real store on a temp file with an injected clock, called through the router |
| `tests/logs.rs`, `tests/offers.rs` | the store's acceptance tests: logs, heads, the key index, signed requests, limits and agent records; offers, claims, the state poll and the claim check |
| `examples/test_agent.rs` | the test person over JSON lines, for the agent language suites |
| `Dockerfile` | the container image (build from the repo root) |

## Routes (base `/v1`)

| Route | Answers |
|---|---|
| `POST /logs/{scid}/events` `AppendRequest` | `200 {head}` · `400 {error}` (`malformed`, `too-many-events`, `vouch-and-pow`, `expires`, a `ChainError` or `FoldError` name) · `403 pow` · `403 claim` · `404` a first event that is not #0 · `409 {head}` · `409 key-taken` · `413` · `503 vouch-unavailable` |
| `GET /logs/{scid}?from=` (public host profile) | `200 LogPage` (≤ 256 events; `vouched: false`, no note, no pauses) · `404` |
| `GET /heads/{scid}` (public host profile) | `200 SignedHead` · `404` |
| `GET /keys/{multikey}?wait=0..8` | `200 KeyIndexEntry` · `404` (at once, or at the timeout) · `400` wait > 8 |
| `GET /pow` | `200 {bits, previousBits, changedAt}` |
| `POST /notes/{scid}` `{token}` + signed request | `403 signer`/`store` · `503 vouch-unavailable` (no vouching service exists yet) |
| `GET /agents/{did}` | `200 AgentRecord` · `404` |
| `PUT /agents/{did}` | `200` · `400 malformed`/`anchor` · `403 signer`/`writer`/`store` · `409 {version}` |
| `POST /offers/{scid}` `SignedOffer` | `201 {offerId}` · `400` · `403 signer` · `404` · `409 {head}` · `409 offer-exists` · `429 identity` |
| `GET /offers/{scid}?wait=` master-signed | `200 [{offer, state}]` (open and claimed) · `403 signer`/`store` |
| `DELETE /offers/{scid}/{offerId}` master-signed | `204` · `403 signer`/`store` · `404` |
| `POST /offers/{scid}/{offerId}/claim` `Claim` | `201`/`200` repeat `{state, kind, expires}` · `403 secret`/`bound`/`cosig` · `404` · `409 key-taken` · `410 {state}` |
| `GET /offers/{scid}/{offerId}/state?key=&wait=` | `200 {state, seq?}` for the claiming key only · `404` |
| `GET /feed?after=` `Authorization: Bearer <FEED_TOKEN>` | `200 {items, next}` · `401` |
| `GET /health` | `200` |

### The public did:scid host profile

`GET /v1/logs/{scid}` and `GET /v1/heads/{scid}` make the store a public, read-only HTTPS
host of `did:scid` logs: a browser, `coasys.id`, another vendor's resolver or an executor
reads a log with no agent language, and a DID URL's `src` hint can name the store
(`did:scid:coasys:1:<scid>?src=<host>`; a resolver reads `{src}/v1/logs/{scid}?from=`). The
agent language never fetches a `src`: it talks only to its configured store.

- **CORS:** both routes answer `Access-Control-Allow-Origin: *` (GET only, no credentials),
  preflights included. Every other route, writes and the other reads, keeps `CORS_ORIGINS`.
- **`Cache-Control` on `GET /logs`:** `public, max-age=31536000, immutable` for a page whose
  last event is below the head (a log only grows, so those events and their `appendedAt`
  never change); `public, max-age=10` for the page that holds the head and for an empty page
  past it; `no-store` on `404`, so a log published a moment later is not hidden. A settled
  page's `head` is the head when it was served: a reader takes the current head from the last
  page or `/heads`. When vouching notes come, the `note` field must leave settled pages (or
  their cache header must change), since a note attached later would not reach a cached page.

## Admission checks

What the store checks before it commits a write. All but the bot-scope check guard
`POST /v1/logs/{scid}/events`; the bot-scope check guards `PUT /v1/agents/{did}`. Code and
tests name them by these names.

1. **The continuity check.** The request continues the stored head: its first event's
   `seq − 1` and `prev` are the head. It runs as `append_in` with `expect` taken from the
   request; a conflict answers `409` with the stored signed head.
2. **The new-log admission.** A log's first event is #0 (`404` otherwise) and names the route's
   SCID; `log::admit_new_log` decides whether a new log may start (proof of work, vouching
   note; see "No brake yet" below).
3. **The replay check.** Every event replays with the identity core (`KelPolicy`) on the stored
   state, so the store can withhold an event but never forge one. Each bot scope's `expires` in an
   `incept` or `add` lies in the future, by the store's clock, and at most 400 days ahead
   (`400 expires`). Only a bot key expires: a device key with `expires` and no bot mark is
   the core's `ExpiresWithoutBotMark`.
4. **The one-key check.** It looks across every log: a signing key (masters included) that sits
   in `keys`, revoked or not, is `409 key-taken`, in an `incept`, an `add`, a `rotate` and a
   claim. One key speaks for at most one identity, ever.
5. **The bot-scope check.** A bot key writes its own agent record only while its scope is in
   force (`agents.rs`).
6. **The one-transaction rule.** Events (`SqliteChainStore::append_in`), `logs`, `keys`, `feed`,
   spent claims, superseded offers and the signed head commit together or not at all.
   `Fault::AfterEvents` (tests only, `Store::inject_fault`) fails an append right after the
   event insert; the tests check that nothing of it stays.
7. **The claim check.** An `add` spends a claim. Its conditions:
   1. the `add` names an offer of the same SCID in state `claimed` (the store, on reading the
      offer);
   2. the offer's signer is still the current master key;
   3. the offer's confirm time (`expires` + `CONFIRM_GRACE_S`) has not passed;
   4. the `add` lists one key, and its `sign`, `enc` and `coSig` are the claim's;
   5. a bot offer adds a bot key with the offer's scope, and only a bot offer adds a bot key;
   6. the append spends the claim and closes the offer, in the append's transaction.

   Conditions 2–5 are the core's `link::check_add`. A failure is `403 {error:"claim"}`.

- A `rotate` supersedes every open and claimed offer. Offer time states (expired, 24-hour
  tombstones) apply lazily when a request touches the identity's offers.
- A wrong secret (`403 secret`) or a claim by another key on a bound offer (`403 bound`)
  changes nothing: no miss count, no lockout. The claim route is unauthenticated, so a count
  would let anyone who knows `(scid, offerId)` end the offer, and `s` is 256 bits, so a count
  protects nothing against guessing.

## Rules

- **Signed requests**: `req` is `{did, store, keyId, kelSeq, kelHash, method, path, bodyHash,
  nonce, expires}`, signed under `coasys:store-request`. `store` is this store key's `did:key`,
  as a signed head names its store (`403 store` otherwise, checked first), so a request for
  one store cannot be replayed to another that holds the same log prefix. No route publishes
  the key on its own: a client knows it from configuration (the executor's
  `identity.storeKeys`; `serve` prints it on start, `keygen` on creation), and the `store`
  of every signed head (`GET /v1/heads/{scid}`, the `head` of a log page) names it once
  checked against those keys. Then the key is valid at the head by the store's clock and has
  no bot mark;
  `kelSeq`/`kelHash` name a held position; `expires` lies at most 5 minutes ahead; method and
  **path** (percent-decoded, without the query) match; `bodyHash` is the multihash of the raw
  body bytes; the nonce is new in 10 minutes. Offer routes take only the current master key.
  The agent language sends every body as its canonical JSON, so a signer hashes `JCS(body)`.
- **Agent records**: `sig` is the `coasys:link` signature of `{profile, version, anchor}` by
  `signedBy`. A `did:key` record is signed by that key and its `anchor` is `null`. A
  `did:scid` record comes with a signed request of that identity, and its `anchor` is one
  number: the seq of the identity's log that the signer knew when it signed. The store accepts
  it only when the anchor is at or below the head (`400 anchor` otherwise) and `signedBy` is a
  bare `did:key` whose key the log lists as a device key (looked up among `state.keys`, never
  the masters: the master never signs for the person), without the bot mark and
  valid both at the anchor and at the log's current head (`key_valid_at_time(key, anchor,
  now)` and `key_valid_at_time(key, head, now)`, no bot mark at either; `403 signer`
  otherwise). The anchor places the signature in the log, so revocation needs no clock: a key
  revoked at seq N signs nothing valid anchored at or after N. The signer picks the anchor, so
  the head check is what stops a revoked key from backdating a new record below N: the store
  orders arrivals, a record it took before the revocation stays valid (readers check at the
  anchor), and one that arrives after it is refused whatever anchor it names. It carries no DID (the record is keyed by its did:scid), no key id (the log lists the
  key) and no hash (a hash only tells forked branches apart, and a fork freezes the identity).
  The old four-field anchor object does not parse: `400 malformed`. Offers' `by` and signed
  requests keep `kelSeq`/`kelHash`: they prove freshness at the head, once per action.
- **No brake yet**: a `#0` without a note is admitted with `admitted_by = 'brake'`; a `pow`
  that is sent must meet `POW_BITS` (`403 pow` otherwise); no address or global buckets, no
  `/metrics`. A vouching token answers `503 vouch-unavailable` and commits nothing. No pairing
  routes, no vouching notes. The brake (per-address buckets, a store-wide ceiling, a required
  proof of work) belongs in `log::admit_new_log`, before any replay.
- The store reads the clock only through `Store::now` (tests inject it). `wait` reads wake on
  every committed write (`Store::changed`).
- Never store or answer an offer's secret `s`: `tests/offers.rs` scans every table and response.

## Configuration

| Variable | Default | |
|---|---|---|
| `IDENTITY_STORE_DB` | `identity-store.sqlite` | the SQLite file |
| `IDENTITY_STORE_KEY` | `store-key` | the head key file (32-byte secret, base58btc); `serve` makes one when missing |
| `IDENTITY_STORE_LISTEN` | `0.0.0.0:8080` | port 0 picks a free port |
| `POW_BITS` | `22` | what `GET /v1/pow` publishes and a sent proof must meet |
| `CORS_ORIGINS` | none | comma-separated browser origins (all routes but the public host profile's, which allow any origin) |
| `FEED_TOKEN` | none | the mirror token; without one the feed answers `401` |

`serve` prints `identity-store listening on http://<addr> store-key <multikey>` once it listens.

## Tests

```bash
cargo test -p identity-store
cargo clippy -p identity-store --all-targets -- -D warnings
cargo fmt -p identity-store --check
# the agent language suites start this store and drive examples/test_agent.rs:
cd bootstrap-languages/centralized-agent-language && pnpm test
```
