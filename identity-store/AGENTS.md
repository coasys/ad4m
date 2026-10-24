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
| `src/agents.rs` | agent records under a `did:key` or a `did:scid` |
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
| `GET /logs/{scid}?from=` | `200 LogPage` (≤ 256 events; `vouched: false`, no note, no pauses) · `404` |
| `GET /heads/{scid}` | `200 SignedHead` · `404` |
| `GET /keys/{multikey}?wait=0..8` | `200 KeyIndexEntry` · `404` (at once, or at the timeout) · `400` wait > 8 |
| `GET /pow` | `200 {bits, previousBits, changedAt}` |
| `POST /notes/{scid}` `{token}` + signed request | `403 signer` · `503 vouch-unavailable` (no vouching service exists yet) |
| `GET /agents/{did}` | `200 AgentRecord` · `404` |
| `PUT /agents/{did}` | `200` · `403 signer`/`writer` · `409 {version}` |
| `POST /offers/{scid}` `SignedOffer` | `201 {offerId}` · `400` · `403 signer` · `404` · `409 {head}` · `409 offer-exists` · `429 identity` |
| `GET /offers/{scid}?wait=` master-signed | `200 [{offer, state}]` (open and claimed) |
| `DELETE /offers/{scid}/{offerId}` master-signed | `204` · `404` |
| `POST /offers/{scid}/{offerId}/claim` `Claim` | `201`/`200` repeat `{state, kind, expires}` · `403 secret`/`bound`/`cosig` · `404` · `409 key-taken` · `410 {state}` |
| `GET /offers/{scid}/{offerId}/state?key=&wait=` | `200 {state, seq?}` for the claiming key only · `404` |
| `GET /feed?after=` `Authorization: Bearer <FEED_TOKEN>` | `200 {items, next}` · `401` |
| `GET /health` | `200` |

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
   state, so the store can withhold an event but never forge one. Each key's `expires` in an
   `incept` or `add` lies in the future, by the store's clock, and at most 400 days ahead.
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

## Rules

- **Signed requests**: the key is valid at the head by the store's clock and has no bot mark;
  `kelSeq`/`kelHash` name a held position; `expires` lies at most 5 minutes ahead; method and
  **path** (percent-decoded, without the query) match; `bodyHash` is the multihash of the raw
  body bytes; the nonce is new in 10 minutes. Offer routes take only the current master key.
  The agent language sends every body as its canonical JSON, so a signer hashes `JCS(body)`.
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
| `CORS_ORIGINS` | none | comma-separated browser origins |
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
