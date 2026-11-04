# identity-store — agent guide

The shared store of `did:scid:coasys:1` key logs with the offers and claims of device linking,
the device inbox, the private details record and the request box.
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
| `src/db.rs` | the schema (the module's `chain_*` tables, type `coasys:kel`, and `logs`, `heads`, `keys`, `agents`, `nonces`, `feed`, `offers`, `claims`, `fork_signers`, `inbox`, `records`, `requests`, `buckets`), log replay, key-index rows |
| `src/held.rs` | `HeldLogs`: the folds of recently used logs at their stored heads, bounded, least recently used out; `Store::held_chain` |
| `src/log.rs` | `POST …/events` with the admission checks (below), log pages, heads, the key index; `admit_new_log` (the brake's place) |
| `src/offers.rs` | the offer routes, the claim route, the device's state poll, the store's side of the claim check (`check_spend`, `spend`, `supersede`) |
| `src/request.rs` | signed requests (`X-Coasys-Request`): checks, nonce replay guard (`take_nonce`, shared with pre-join reads), `percent_decode` |
| `src/inbox.rs` | the device inbox: pre-join and joined writes, reads (`X-Coasys-Inbox` for a pre-join read) and acknowledgements; `purge_ended` and `drop_closed_offer_messages`, which the append and the offer state changes call in their transactions |
| `src/private.rs` | the private details record: one per identity, versioned, wrapped to exactly the log's readers |
| `src/request_box.rs` | the request box: post, list, delete |
| `src/limits.rs` | the identity bucket (`take_identity_write`) |
| `src/agents.rs` | agent records under a `did:key` or a `did:scid`; the store checks the record signature and never reads `profile` (the agent language writes a `did:key` agent's signed AD4M expression there) |
| `src/head.rs` | `StoreKey` (key file, `keygen`), `sign_head` (the core's `HeadStatement`; executors verify it with the same type) |
| `src/routes.rs` | the axum router, body limit, CORS, waits, the feed (token compared in constant time) |
| `src/error.rs` | `ApiError`: every refusal as a status and a JSON body with an `error` name |
| `tests/common/agent.rs` | a test person: fixed keys, a log built with the core's builders, offers, claims, signed requests, records |
| `tests/common/mod.rs` | `TestStore`: a real store on a temp file with an injected clock, called through the router |
| `tests/logs.rs`, `tests/offers.rs`, `tests/devices.rs` | the store's acceptance tests: logs, heads, the key index, signed requests, limits and agent records; offers, claims, the state poll and the claim check; the inbox, the record, the request box, the purges, the sweeper and the identity bucket |
| `tests/held.rs` | the held folds: no replay per request (the inbox, record and request-box writes included), a forged request reads no log, an append's head in force at once (a revoke refuses the key's next write), the bound, rival events |
| `tests/serve.rs` | the `serve` process stops cleanly on SIGTERM and SIGINT |
| `examples/test_agent.rs` | the test person over JSON lines, for the agent language suites |
| `Dockerfile` | the container image (build from the repo root) |

## Routes (base `/v1`)

| Route | Answers |
|---|---|
| `POST /logs/{scid}/events` `AppendRequest` | `200 {head}` · `400 {error}` (`malformed`, `too-many-events`, `vouch-and-pow`, `expires`, a `ChainError` or `FoldError` name; an event that does not parse names its `FormatError`, e.g. `DuplicateMember`) · `403 pow` · `403 claim` · `404` a first event that is not #0 · `409 {head}` · `409 forked {head, fork}` · `409 key-taken` · `413` · `503 vouch-unavailable` |
| `GET /logs/{scid}?from=` (public host profile) | `200 LogPage` (≤ 256 events; `vouched: false`, no note, no pauses) · `404` |
| `GET /heads/{scid}` (public host profile) | `200 SignedHead` · `404` |
| `GET /keys/{multikey}?wait=0..8` | `200 KeyIndexEntry` · `404` (at once, or at the timeout) · `400` wait > 8 |
| `GET /pow` | `200 {bits, previousBits, changedAt}` |
| `POST /notes/{scid}` `{token}` + signed request | `403 signer`/`store` · `503 vouch-unavailable` (no vouching service exists yet) |
| `GET /agents/{did}` | `200 AgentRecord` · `404` |
| `PUT /agents/{did}` | `200` · `400 malformed`/`anchor`/`version` · `403 signer`/`writer`/`store` · `409 {version}` · `409 forked` |
| `POST /offers/{scid}` `SignedOffer` | `201 {offerId}` · `400` · `403 signer` · `404` · `409 {head}` · `409 offer-exists` · `429 identity` (5 open offers, or the identity bucket) |
| `GET /offers/{scid}?wait=` master-signed | `200 [{offer, state}]` (open and claimed) · `403 signer`/`store` |
| `DELETE /offers/{scid}/{offerId}` master-signed | `204` · `403 signer`/`store` · `404` |
| `POST /offers/{scid}/{offerId}/claim` `Claim` | `201`/`200` repeat `{state, kind, expires}` · `403 secret`/`bound`/`cosig` · `404` · `409 key-taken` · `410 {state}` |
| `GET /offers/{scid}/{offerId}/state?key=&wait=` | `200 {state, seq?}` for the claiming key only · `404` |
| `PUT /inbox/pre/{a}` `{sealed, bind:{scid, offerId, sign, r}}` + master-signed request | `201 {id, expires}` (expires = the offer's) · `400` · `403 writer` · `413` · `429 identity` (2 messages, or the bucket) |
| `GET /inbox/pre/{a}?wait=` `X-Coasys-Inbox` | `200 [InboxMessage]` oldest first · `403 signer`/`store` |
| `DELETE /inbox/pre/{a}/{id}` `X-Coasys-Inbox` | `204` · `403 signer`/`store` · `404` |
| `PUT /inbox/{scid}/{keyId}` `{sealed}` + master-signed request | `201 {id, expires}` (30 days) · `400` · `403 writer` · `404` no such key valid at the head without the bot mark · `413` · `429 identity` (16 messages, or the bucket) |
| `GET /inbox/{scid}/{keyId}?wait=` signed by `keyId` | `200 [InboxMessage]` · `403 signer` |
| `DELETE /inbox/{scid}/{keyId}/{id}` signed by `keyId` | `204` · `403 signer` · `404` |
| `GET /private/{scid}` signed (device key or master) | `200 SealedRecord` · `403` · `404` none yet |
| `PUT /private/{scid}` `SealedRecord` + signed request | `200 {version}` · `400 malformed`/`wrapped`/`recovery-key` · `403` · `409 {version}` · `413` · `429 identity` |
| `POST /requests/{scid}` `{body, expires}` signed by a device key | `201 {id}` · `400 malformed`/`expires` · `403 signer` · `429 identity` (16 open, or the bucket) |
| `GET /requests/{scid}?since=` signed (device key or master) | `200 [DeviceRequest]` oldest first |
| `DELETE /requests/{scid}/{id}` by the poster or the master | `204` · `403 signer` · `404` |
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
   spent claims, superseded offers, the purges (`inbox::purge_ended`: the joined inbox and the
   requests of every key the append ended, and every joined message written by a master that
   is no longer current; the pre-join messages of offers that left `open`) and the signed head
   commit together or not at all.
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
  checked against those keys. Then the signature verifies under the key the key index lists
  for `keyId` in that log (before the log is read: a forged request costs an indexed lookup
  and one signature check); then, on the log's fold at the stored head (`held.rs`), the
  identity is not frozen, the key is valid at the head by the store's clock and has
  no bot mark;
  `kelSeq`/`kelHash` name a held position; `expires` lies at most 5 minutes ahead; method and
  **path** (percent-decoded, without the query) match; `bodyHash` is the multihash of the raw
  body bytes; the nonce is new in 10 minutes. Offer routes take only the current master key.
  The agent language sends every body as its canonical JSON, so a signer hashes `JCS(body)`.
- **Agent records**: `version` is unix milliseconds; the store takes a record only when its
  version is above the stored one and at most `MAX_VERSION_AHEAD_S` (300 s) past its clock
  (`400 version` otherwise, before any other check). Without that ceiling one write at a huge
  version by a key valid at the time (a stolen device key before its revocation; for a
  `did:key`, any code its key signs for) would freeze the record for good; with it the owner
  writes `stored + 1` at the latest a second later, so revoking the key and republishing
  replaces the thief's record. The signer check, the version check and the write run in one
  immediate transaction (a revocation commits before the check or after the write).
- **Agent record signatures**: `sig` is the `coasys:agent-record` signature of `{profile, version, anchor}` by
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
  key) and no hash (a hash only tells forked branches apart, and the store keeps one branch).
  The old four-field anchor object does not parse: `400 malformed`. Offers' `by` and signed
  requests keep `kelSeq`/`kelHash`: they prove freshness at the head, once per action.
- **No brake yet**: a `#0` without a note is admitted with `admitted_by = 'brake'`; a `pow`
  that is sent must meet `POW_BITS` (`403 pow` otherwise); no address or global buckets, no
  `/metrics`. A vouching token answers `503 vouch-unavailable` and commits nothing. No pairing
  routes, no vouching notes. The brake (per-address buckets, a store-wide ceiling, a required
  proof of work) belongs in `log::admit_new_log`, which runs once event #0 has replayed and
  before the rest of the append replays.
- The store reads the clock only through `Store::now` (tests inject it). A `wait` read
  watches one `Topic`: the identity's offers (`GET /offers/{scid}`, master-signed), the
  identity's offers for the device's unauthenticated state poll (`OfferState`, its own
  places, so pollers never take the owner's), one key (`GET /keys/{k}`) or one inbox address
  (`GET /inbox/…`); a committed write wakes only the topics it concerns (`Store::changed`: an
  offer write its identity's offers and their state polls, an append those and every key it
  lists, an inbox write its address). At most
  `MAX_WAITERS_PER_TOPIC` (16) reads wait on one topic: a read that finds its topic full takes
  the oldest read's place, and that one answers at once, as with `wait=0`, so squatters who
  know an offer and its claiming key cannot shut the claimant's poll out, they only shorten
  each other's waits. At most `MAX_WAITERS` (4096) wait on the store; a read over that answers
  at once. The caps are per topic, not per caller address: behind a proxy every caller has one
  address.
- **Held folds** (`held.rs`): the store keeps each recently used log folded in memory, keyed
  by the stored head it ends at. `Store::held_chain` reads the head from `chain_heads` (one
  primary-key lookup) and uses the held fold only when it ends exactly there; otherwise the
  log folds once from `chain_entries` and is held. An append purges the inboxes and requests
  of the keys it ends inside its transaction, then puts its new fold in after that transaction
  commits, under the database lock, so the next request sees the new head's keys (a revoked
  key is refused at once) with no replay. The writes that check a signed request inside their
  own transaction (agent records, inbox, record and request-box writes) read the held fold
  there, so a revoke committed before the write refuses it. The folds are bounded by the events they
  cover together (`HELD_LOG_EVENTS`, default 1 048 576), least recently used out first; a log
  longer than the bound is folded on every use. `Store::held.folds()` counts replays (the
  tests count them instead of timing requests). A restart starts with nothing held: each log
  folds once on its first use. A rival (fork) event replays the prefix before it only when it
  carries a proof by the master that signs at its seq, read from the held fold, and only
  while that master has no recorded fork, so nobody without a master key of the log makes the
  store replay it.
- **A fresh database**: the store has never been deployed, and a store file from before the
  agent-record version ceiling and the `fork_signers` table is not upgraded: start such a
  store on an empty database. Records stored above the store's clock and forks recorded
  without their signing master would otherwise stay as they are. There are no migrations.
- **Forks**: an event at a seq the store holds, with another hash than the stored one, that
  replays on the stored events before it (every proof checked) is a fork. The store records it
  in `chain_forks` with the event's JSON (`SqliteChainStore::record_fork_in`) and its signing
  master (the last master of the state that replay gives) in `fork_signers` (`db::record_fork`,
  same transaction), and answers `409 {error:"forked", head, fork:{seq, ours, theirs}}`. It
  keeps **one fork per identity and signing master** (`fork_signers` primary key
  `(scid, signer)`): a further rival by a master that already has one is answered with the
  recorded fork and stores nothing, so the fork rows of an identity are bounded by the masters
  its log ever had, whatever a thief with a retired master files. Only the master signs events,
  so a fork means two master-signed events at one seq: duplicity, two wallets with the same
  words racing, or a retired (stolen) master signing on the prefix where it was current. A
  resent stored event, an event past the head and one that does not replay are plain
  `409 {head}`.
- **The freeze** (`db::freezing_fork`, derived from the fork rows and the held log each time,
  no stored flag): the identity is frozen only while a recorded fork's signing master is the
  current master at the held head (one primary-key lookup of `fork_signers`, no replay). A
  rival `rotate` is signed by the master it names: a twin rotating onto the committed next key
  freezes nothing until the owner rotates onto that same key, and then freezes until a second
  `rotate` retires it. A fork by a master the held log rotated away from at the
  fork's seq or later is kept and answered as evidence but freezes nothing, so a thief with a
  retired master cannot lock the identity out after the owner rotates. While frozen, every
  signed request and every append whose first event is not a `rotate` answers `409 forked`; a
  `rotate` (signed by the committed next key, which the current master alone does not give)
  is taken, retires the rival's signer and so ends the freeze. The log and head stay readable
  as held throughout.
- Never store or answer an offer's secret `s`: `tests/offers.rs` scans every table and response.
- **The device inbox, the record and the request box keep ciphertext and signed envelopes
  only.** The store reads the outer fields (who wrote, for whom, until when) and never a
  payload; `tests/devices.rs` scans every table for a master secret and a record's device name.
- **Inbox rules.** A pre-join write needs an `open` offer bound to `bind.sign`,
  `pre_address(sign, r)` equal to the route's address, and a request by the current master that
  signed the offer (`403 writer` otherwise); its message expires with the offer. A joined write
  needs the current master (`403 writer`) and a receiving device key valid at the head without
  the bot mark (`404`). The signed request (`Store::verify_request_in`) and each check run
  inside the write's transaction, on the log's fold at the head that transaction reads
  (`Store::held_chain`, no replay) and the offers as it reads them, so a revoke or an offer
  change that lands first is seen. A joined inbox is
  read and acknowledged only by its own key; a pre-join one only with an `X-Coasys-Inbox` by the
  device string's key for this store (`403 store` otherwise, the signed requests' check), that
  method and path, with a fresh nonce. Listing never deletes.
  Pre-join messages go whenever their offer leaves `open` (`offers::end`, the claim,
  `supersede`, `refresh`).
- **Record rules.** Readers and writers are the identity's device keys without the bot mark and
  the current master (`Signer::AnyKey`). The record must be wrapped to exactly the log's readers
  at the head (`record::check_sealed`, `400 wrapped`), its version must be the stored one plus
  one (`409 {version}`), and a recovery key once set is kept (`400 recovery-key`). The signed
  request is checked inside the write's transaction (`Store::verify_request_in`), against the
  held fold at the head it reads; the request box's post and delete do the same.
- **Request box rules.** Only a device key without the bot mark posts (a master gets
  `403 signer`); a new `send-master` replaces the poster's older one; a request lives at most 7
  days; the poster or the current master deletes. The stored `signed` is the post's own signed
  request, so a reader checks the poster without trusting the store.
- **The identity bucket** (`limits.rs`, `IDENTITY_BURST` and `IDENTITY_REFILL_S`): offer opens,
  inbox writes, record writes and request posts take one token, last, inside their
  transaction, so a refused write costs nothing. Reads and waits never count. The default
  burst of 10 lets a person link two devices that keep the master key back to back (one such
  link takes 5 writes: offer, ticket, master push, record, and the device's record update).
  Tests of another
  limit use `TestStore::with_identity_burst(n)`; the agent language suite starts its store with
  `IDENTITY_BURST=1000`.
- **Expiry.** Reads never answer an expired inbox message or request; `Store::sweep` deletes
  them (and old nonces), and `serve` runs it every `SWEEP_EVERY_S` (60 s). Per-address limits
  (long polls, writes per hour, failed claims) belong with the brake and do not exist yet.

## Configuration

| Variable | Default | |
|---|---|---|
| `IDENTITY_STORE_DB` | `identity-store.sqlite` | the SQLite file |
| `IDENTITY_STORE_KEY` | `store-key` | the head key file (32-byte secret, base58btc); `serve` makes one when missing |
| `IDENTITY_STORE_LISTEN` | `0.0.0.0:8080` | port 0 picks a free port |
| `POW_BITS` | `22` | what `GET /v1/pow` publishes and a sent proof must meet |
| `CORS_ORIGINS` | none | comma-separated browser origins (all routes but the public host profile's, which allow any origin) |
| `FEED_TOKEN` | none | the mirror token; without one the feed answers `401` |
| `IDENTITY_BURST` | `10` | writes per identity at once (the identity bucket) |
| `IDENTITY_REFILL_S` | `60` | seconds per write the identity bucket refills |
| `HELD_LOG_EVENTS` | `1048576` | the most events the logs held folded in memory cover together |

`serve` prints `identity-store listening on http://<addr> store-key <multikey>` once it listens.

## Tests

```bash
cargo test -p identity-store
cargo clippy -p identity-store --all-targets -- -D warnings
cargo fmt -p identity-store --check
# the agent language suites start this store and drive examples/test_agent.rs:
cd bootstrap-languages/centralized-agent-language && pnpm test
```
