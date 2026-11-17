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
| `src/main.rs` | `identity-store [serve]`, `identity-store keygen`; configuration from the environment (`Settings::from_config`); `serve` records each socket's peer address (`ConnectInfo`) |
| `src/lib.rs` | `Store` (database, head key, clock, settings, the published proof-of-work parameters, the HTTP client for the vouching service, the change signal for waits), `Settings` (`from_config` parses every key), `Clock`, `Fault` |
| `src/db.rs` | the schema (the module's `chain_*` tables, type `coasys:kel`, and `logs`, `heads`, `keys`, `agents`, `nonces`, `feed`, `offers`, `claims`, `fork_signers`, `inbox`, `records`, `requests`, `buckets`, `brake`, `pow`, `notes`), log replay, the key index (key rows with each master marked, the next-key commitment in `logs.next`) and its lookups |
| `src/held.rs` | `HeldLogs`: the folds of recently used logs at their stored heads, bounded, least recently used out; `Store::held_chain`; `fold_bytes_of` (a fold's whole heap and its live part, for the bound and the per-log budget); `state_before` (a log's state before a seq, rebuilt from a later state) |
| `src/log.rs` | `POST …/events` with the admission checks (below; `append` picks the brake or the vouch path, `append_admitted` runs the checks under an `Admission`), log pages (with the note and `vouched`), heads, the key index |
| `src/brake.rs` | the brake on no-note logs: the sender's address (`Sender`, `client_ip`, `TRUSTED_PROXIES`, IPv6 by /64), the token buckets in `brake` (failure, global, address), the proof-of-work check with the previous bits' grace, `pow_params` (table `pow`), pruning of refilled rows, the `/v1/metrics` text |
| `src/vouch.rs` | vouching notes: the redeem client of the platform's vouching service, the `notes` table (one note per log), `POST /v1/notes/{scid}`, `note_vouches` (the note under `VOUCH_KEYS`) |
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
| `tests/held.rs` | the held folds: no replay per request (the inbox, record and request-box writes included), a forged request reads no log, an append's head in force at once (a revoke refuses the key's next write), the bound, rival events (one that does not replay refused on the held fold with no replay), `state_before` against a fold at every seq |
| `tests/append_copies.rs` | an append copies the held fold once, taken or refused at its last event (a counting allocator) |
| `tests/authenticate.rs` | authenticated before any replay: junk and wrongly signed appends, offers and requests on an unheld log replay nothing; a valid append and a `rotate` replay once; the length cap |
| `tests/held_memory.rs` | the heap a held fold takes per event (a counting allocator; the bound stated under "Held folds") |
| `tests/miss.rs` | a miss in the held folds checks no stored signature (every stored proof replaced, the miss still folds; a new event is still checked); the per-log budget (`log-too-large`) on the live part and the whole fold, a `rotate` taken past either, the owner's recovery from a log a thief padded (rotate, revoke the thief's bot keys, add a device), and pad-revoke-pad stopped at the whole-fold bound |
| `tests/miss_cost.rs` | ignored: the wall time of one signed request on a miss, on a full rotation log and on logs padded with repeated scope actions; of a late-failing batch on a log held at its budget; of a rival that does not replay on a held full rotation log (`-- --ignored --nocapture`, release) |
| `tests/brake.rs` | the brake: the address bucket (a fourth no-note log from one address in a day, IPv6 by /64), the global ceiling, a full bucket before any signature check, the failure bucket, the previous bits for 10 minutes, trusted proxies, `/v1/metrics` (no addresses), pruning, the defaults |
| `tests/notes.rs` | vouching notes against `tests/common/vouch.rs` (a vouching service on 127.0.0.1 with the redeem contract): a note with #0, a note beside a log, who may ask for one, the service's answers, `vouched` under `VOUCH_KEYS`, no note on a settled page |
| `tests/serve.rs` | the `serve` process stops cleanly on SIGTERM and SIGINT |
| `examples/test_agent.rs` | the test person over JSON lines, for the agent language suites |
| `examples/vouch_service.rs` | `tests/common/vouch.rs` as a process over JSON lines (`give`, `spend`, `redeems`), for the executor's identity suite (`tests/js/tests/identity.test.ts`); CI builds it with the store |
| `Dockerfile` | the container image (build from the repo root) |

## Routes (base `/v1`)

| Route | Answers |
|---|---|
| `POST /logs/{scid}/events` `AppendRequest` | `200 {head}` · `400 {error}` (`malformed`, `too-many-events`, `log-full`, `log-too-large`, `vouch-and-pow`, `expires`, a `ChainError` or `FoldError` name; an event that does not parse names its `FormatError`, e.g. `DuplicateMember`) · `403 pow` · `403 claim` · `404` a first event that is not #0 · `409 {head}` · `409 forked {head, fork}` · `409 key-taken` · `413` · `403 vouch` (the service refused the token: spent, expired, or its email vouched another identity) · `409 {head}` a token for a log the store holds · `429 {error:"rate-limited", scope, retryAfter}` with `Retry-After` (`scope` `failure`, `global` or `address`) · `503 vouch-unavailable` |
| `GET /logs/{scid}?from=` (public host profile) | `200 LogPage` (≤ 256 events; `note` and `vouched` on the page that holds the head, none on a settled page; no pauses) · `404` |
| `GET /heads/{scid}` (public host profile) | `200 SignedHead` · `404` |
| `GET /keys/{multikey}?wait=0..8` | `200 KeyIndexEntry` · `404` (at once, or at the timeout) · `400` wait > 8 |
| `GET /pow` | `200 {bits, previousBits, changedAt}` (`POW_BITS`; the bits before the last change and when it was, kept in table `pow` across restarts) |
| `GET /metrics` | `200 text/plain`: the global bucket's level; for the address and the failure buckets, how many are in use (not full), how many are empty and the lowest level. Unauthenticated, so it names no address |
| `POST /notes/{scid}` `{token}` + signed request (a device key without the bot mark or the current master) | `201 VouchNote` · `400 malformed` · `403 signer`/`store` · `403 vouch` · `409 already-vouched` (checked before the redeem: no token is spent) · `503 vouch-unavailable` |
| `GET /agents/{did}` | `200 AgentRecord` · `404` |
| `PUT /agents/{did}` | `200` · `400 malformed`/`anchor`/`version` · `403 signer`/`writer`/`store` · `409 {version}` · `409 forked` |
| `POST /offers/{scid}` `SignedOffer` | `201 {offerId}` · `400` · `403 signer` · `404` · `409 {head}` · `409 offer-exists` · `429 identity` (5 open offers, or the identity bucket) |
| `GET /offers/{scid}?wait=` master-signed | `200 [{offer, state}]` (open and claimed) · `403 signer`/`store` |
| `DELETE /offers/{scid}/{offerId}` master-signed | `204` · `403 signer`/`store` · `404` |
| `POST /offers/{scid}/{offerId}/claim` `Claim` | `201`/`200` repeat `{state, kind, expires}` · `403 secret`/`bound`/`cosig` · `404` · `409 key-taken` · `410 {state}` |
| `GET /offers/{scid}/{offerId}/state?key=&wait=` | `200 {state, seq?}` for the claiming key only · `404` |
| `PUT /inbox/pre/{a}` `{sealed, bind:{scid, offerId, sign, r}}` + master-signed request | `201 {id, expires}` (expires = the offer's) · `400` · `403 writer` · `413` · `429 identity` (2 messages per writing identity at the address, 2 per offer over every address, or the bucket) |
| `GET /inbox/pre/{a}?wait=` `X-Coasys-Inbox` | `200 [InboxMessage]` oldest first · `403 signer`/`store` |
| `DELETE /inbox/pre/{a}/{id}` `X-Coasys-Inbox` | `204` · `403 signer`/`store` · `404` |
| `PUT /inbox/{scid}/{keyId}` `{sealed}` + master-signed request | `201 {id, expires}` (30 days) · `400` · `403 writer` · `404` no such key valid at the head without the bot mark · `413` · `429 identity` (16 messages, or the bucket) |
| `GET /inbox/{scid}/{keyId}?wait=` signed by `keyId` | `200 [InboxMessage]` · `403 signer` |
| `DELETE /inbox/{scid}/{keyId}/{id}` signed by `keyId` | `204` · `403 signer` · `404` |
| `GET /private/{scid}` signed (device key or master) | `200 {record: SealedRecord, receipt: RecordReceipt}` · `403` · `404` none yet |
| `PUT /private/{scid}` `SealedRecord` + signed request | `200 {version, receipt: RecordReceipt}` · `400 malformed`/`signer`/`signature`/`attestation`/`wrapped`/`recovery-key` · `403` · `409 {version}` · `413` · `429 identity` |
| `POST /requests/{scid}` `{body, expires}` signed by a device key | `201 {id}` · `400 malformed`/`expires`/`canonical` (the body not sent as its JCS) · `403 signer` · `429 identity` (16 open, 4 per posting key, or the bucket) |
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
  page or `/heads`. A settled page carries no `note` and `vouched: false`: a note attached
  later (or a voucher the store stops trusting) would never reach a cached copy, so a reader
  takes the note from the page that holds the head, as it takes the head.

## Admission checks

What the store checks before it commits a write. All but the bot-scope check guard
`POST /v1/logs/{scid}/events`; the bot-scope check guards `PUT /v1/agents/{did}`. Code and
tests name them by these names.

1. **The continuity check.** The request continues the stored head: its first event's
   `seq − 1` and `prev` are the head. It runs as `append_in` with `expect` taken from the
   request; a conflict answers `409` with the stored signed head.
2. **The new-log admission.** A log's first event is #0 (`404` otherwise) and names the route's
   SCID. Without a vouching token it passes the brake (below) before any signature of it is
   checked; with one, the token is redeemed only once #0 has replayed (see "The brake" and
   "Vouching notes" below).
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
  The ceiling follows the store's wall clock, so a clock step on a live store moves it: a
  **forward** jump of D seconds admits versions up to D + 300 s ahead of true time; while the
  clock is ahead the owner still writes `stored + 1` (the language does that on
  `409 {version}`), but once it is set right a record stored at such a version blocks every
  write, `stored + 1` included (`400 version`), until true time reaches it: up to D. A key
  that was valid during the jump (a stolen device key, any key for a `did:key`) can use that
  to freeze the record for up to D. A **backward** jump of D (more than 300 s) refuses every
  honest write, whose version is the writer's clock, with `400 version` until the store's
  clock is right again. Run the store on a disciplined clock (NTP slewing, not stepping) and
  never set it forward by hand.
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
- **The brake** (`brake.rs`): an append of #0 to a log the store does not hold, without a
  vouching token, checks in order, before any signature: the sender address's failure bucket
  (`FAILURES_PER_HOUR`, 30, refilled per hour; `429 failure`), the global bucket
  (`BRAKE_GLOBAL_PER_HOUR`, refilled over an hour; `429 global`, and a warning on stderr), the
  address bucket (`BRAKE_ADDR_BURST` at once, `BRAKE_ADDR_PER_DAY` per 24 h; `429 address`),
  and the proof of work at `POW_BITS` (`403 pow`; the previous bits pass for `POW_GRACE_S`,
  10 minutes, after a change). A full bucket answers `429` before any check of the events, so
  a broken event gets `429`, not `400`. Only an admitted log takes from the global and the
  address bucket, in the append's transaction (`admitted_by = 'brake'`). Any other `4xx` to a
  log the store does not hold, on a request without a token, takes one token from the failure
  bucket in a transaction of its own. Buckets refill continuously; table `brake` keeps each
  level at its `updated_at` (no row: full). An address is the IPv4 address or the IPv6 /64
  (an IPv4-mapped address counts as IPv4): the socket peer, or, when the peer is in
  `TRUSTED_PROXIES`, `CF-Connecting-IP`, else the nearest `X-Forwarded-For` entry that is no
  trusted proxy. Each admission or refusal deletes the rows of its kind's buckets that are full
  again, so `brake` holds only the addresses seen within one refill. On a log the store
  holds, a repeat of #0 with its `pow` or token gets the same `409 {head}` as any repeat: a
  client that resumes a create sends its #0 as it was. A `pow` or token on a request whose
  first event is not #0 is `400 malformed`.
- **Vouching notes** (`vouch.rs`): `POST {VOUCH_URL}/api/vouch/redeem {token, did}` with
  `Authorization: Bearer {VOUCH_CREDENTIAL}`, 10 s timeout. A `200` note for the identity that
  `verify_note` passes under `VOUCH_KEYS` is taken; `409`/`410` are `403 vouch`; no
  `VOUCH_URL`, an unreachable service, any other status, or a note that does not verify are
  `503 vouch-unavailable`. On `POST …/events` a token (with no `pow`: `400 vouch-and-pow`)
  takes nothing from the brake: #0 replays and every check runs without committing, then the
  token is redeemed outside the database lock, then the append runs again and commits with
  the note (`admitted_by = 'vouch'`); a log that appeared meanwhile is `409 {head}`. On
  `POST /v1/notes/{scid}` the store refuses a second note (`409 already-vouched`) before it
  redeems. `LogPage.vouched` is the stored note read under the current `VOUCH_KEYS`, so a
  store that drops a voucher shows the note with `vouched: false`. No pairing routes yet.
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
  there, so a revoke committed before the write refuses it. The folds are bounded by the heap they
  take together (`HELD_LOG_BYTES`, default 256 MiB), least recently used out first; the bound
  is at least the per-log whole-fold bound, twice `LOG_FOLD_BYTES` (`Settings::check`, at
  start), so every log within its budget can be held (a log past it by rotations alone, below, needs some room above it). `Store::held.folds()` counts replays (the tests count them instead of timing
  requests). A restart starts with nothing held: each log folds once on its first use.
  The bound counts bytes, not events, because events differ a hundredfold: measured with a
  counting allocator (`tests/held_memory.rs`), a held fold takes about 265 B per `scope`
  event and 395 B per `rotate` (the costliest common kind), but one `incept` listing 150 keys
  takes 55 kB and one `scope` naming an action 2 000 times 59 kB, both within the 64 kB body.
  Each fold is sized by `held::held_bytes_of` (key records, strings, scopes and hashes, the
  state's vector capacities), which the same test keeps between the measured heap and twice
  it. At the default a full log (16 384 rotations, about 6.5 MB) takes under 3 % of the
  bound, and the bound holds about 670 000 rotations' worth of folds.
- **A miss trusts the database** (`db::load_chain_before`): every event in `chain_entries`
  passed the full replay when the store appended it, so a miss rebuilds the fold with the
  core's `fold_stored`, which leaves out the proofs' signatures and the duplicate-key scan
  (rule K8, quadratic over a long log) and runs every other rule, giving the same state as
  `fold` (`ad4m-identity-core/tests/fold_stored.rs` pins that on a log of every op). A miss
  costs the parse and the hashing of the log, linear in its length: one signed request on a
  miss took 219 ms on a full rotation log (1 161 ms with every proof checked) and 77 ms on a
  log padded to the per-log budget (`tests/miss_cost.rs`, release, one machine). New events
  are always replayed in full. Forks replay the stored prefix the same way and the rival in
  full. `tests/miss.rs` replaces every stored signature and the miss still folds.
- **One copy per append**: an append stages its events on one copy of the held fold and
  steps each in place (`Chain::appended`); the held fold stays as it is until the commit,
  and a refused event drops the copy. So an append of 32 events allocates about one fold,
  taken or refused at its last event, never one per event (`tests/append_copies.rs`, a
  counting allocator). No state before an event is kept: the claim check of an `add` runs on
  the staged state just before the `add` steps, and its answer is given in the order of the
  admission checks. On a log held at the per-log budget, a batch of 31 valid small scopes and
  a 32nd event without a proof took 14 ms (790–990 ms with two copies of the state per event;
  `tests/miss_cost.rs`, release, one machine).
- **Authenticated before any replay**: nobody without a key of a log makes the store replay
  it, and for a log event only a master key does. A miss in the held folds (a log never used
  since the start, or one evicted by a flood of cheap logs) costs a replay under the database
  lock (no signature checks, bounded by the per-log budget), so every path that may fold a
  log first checks the sender against the key index, which describes the stored head and is written in the append's transaction: each
  key's row in `keys` (masters with `master = 1` and the seqs that added and retired them,
  device keys with their revocation and bot scope) and the next-key commitment in `logs.next`.
  - An append (`log::authenticate_first`): the first event's proof must verify under the key
    that signs at its seq. Only masters sign events after #0 (`KelPolicy`: `add`, `revoke`,
    `scope` by the current master, `rotate` by the master it reveals; a device key never signs
    one), so that key is the master current there, or for a `rotate` the key the stored
    next-key commitment names (at the head) or the master the log rotated onto (a rival). An
    event that continues the head without it is refused with the replay's own error (`400
    MissingProof`, `BadProof`, `NextMismatch`, `Ended`; an event with several faults may be
    named by its signature fault). An event past the head, a resend, and a rival without it
    are `409 {head}`; a rival by a master that already has a recorded fork is answered with
    that fork. The freeze is read from the index too. Only a rival with a valid proof and no
    recorded fork goes further. It is first checked on the state before its seq, rebuilt
    from the fold of the stored head with no replay (`held::state_before`: a key log only
    adds, and stamps every change to a key with its seq; the next-key commitment there is
    read from the one stored event that set it; `tests/held.rs` checks the rebuilt state
    against a fold at every seq of a log of every op): its `prev`, its chain id and the
    core's full `apply`. A rival that fails there is a plain `409 {head}` with no replay
    while the log is held (on a miss, the one fold of `held_chain`, which is then held): 4 ms
    on a held full rotation log, where the prefix replay took 220–240 ms. A rival that passes
    is replayed on the stored prefix before it is recorded, once per signing master, since a
    recorded fork answers the next rival by that master.
  - A signed request (`request.rs`): the signature under the indexed key, then the indexed
    standing (revoked, a bot key, or not the current master on a master route) before the
    log is read.
  - An offer (`offers.rs`, an unauthenticated route): `by.keyId` must be the indexed current
    master and its signature must verify under that key before the log is read.
  - An agent record's bot-scope check reads the index alone (`agents.rs`).

  `tests/authenticate.rs` counts the replays of each refusal (zero) and of each accepted write
  on an unheld log (one).
- **Log length cap** (`log::MAX_LOG_EVENTS`, 16 384 events, seqs 0 to 16 383): the readers'
  bound. The executor's resolver pages through a log at most 64 pages of 256 events deep, so
  the store refuses an append whose last event would lie past it (`400 log-full`, before the
  database is touched). The cap bounds a log's replay too: at most 16 384 events, each
  signature-checked once, when it is appended.
- **Per-log budget** (`LOG_FOLD_BYTES`, `Settings::log_fold_bytes`, default 16 MiB), two
  bounds from one walk of the state (`held::fold_bytes_of`, a `FoldBytes {total, live}`):
  the **live part** of one log's fold, the fold without the records of keys revoked at the
  head (`KeyRecord::revoked` set: device keys revoked, with their strings, bot scopes and map
  entries, and masters a `rotate` replaced, with their strings), at most `LOG_FOLD_BYTES`;
  the **whole fold** (`total`, what `held::held_bytes_of` gives and the held folds count;
  revoked records stay in it in full, as `held::state_before` rebuilds earlier states from
  them) at most twice it. The masters' vector, the event hashes and the fixed part count in
  both. The store measures the fold the append's replay gives, before the transaction writes
  anything, and refuses an append past either bound whole (`400 log-too-large`, the detail
  naming the bound; nothing stored, so a resend costs the replay of its own events and no
  more). A thief holding the current master can pad the live part to the budget (bot keys
  with repeated scope actions); after the owner rotates and revokes the thief's bot keys
  (one `revoke` of them all is fine), the live part drops and the owner appends again, up to
  the whole-fold bound. Without that bound any identity could pad, revoke and pad again up
  to the length cap with no bound on its fold. A log stolen and padded twice can reach the
  whole-fold bound and stays full (it takes `rotate`s only). Events differ a hundredfold in what they cost a fold, so the cap on
  events does not bound it: a `scope` may name its actions again and again (the format allows
  it, and refusing it would change the format), and 800 scopes of one full body each take
  296 MB. The budget bounds what a miss costs and keeps every log within it holdable. A full
  rotation log takes 4.6 MB; a log padded with repeated scope actions stops at 45 events of
  one full body each. `log-too-large` is its own name, not `log-full`: `log-full` follows
  from the seq alone, before the database is touched, while this one follows from what the
  log holds, and a client tells the two apart.
  **A `rotate` is never refused by the budget**: an append whose events are all `rotate`s is
  not measured. A `rotate` is the recovery from a stolen master, and a thief holding the
  current master could otherwise pad the log to within one action of the budget and lock the
  owner's `rotate` out. A `rotate` adds one master record (about 400 B) to the fold, and the
  length cap bounds how many there are, so a log is past its budget by its rotations alone
  (at most 16 384 × 400 B, about 6.5 MB); such a log is held while the bound has room for it.
  Any other append is measured, a `rotate` in it included, so the owner sends the recovery
  `rotate` on its own. The inbox, the private details record and the request box add nothing
  to a log: they are rows of their own tables, never events, so they neither grow a fold nor
  count toward the budget; their own caps bound them (2 pre-join messages per address and
  writing identity, at most 5 open offers each; 16 joined messages per address; one record
  per identity; 16 open requests per identity and 4 per posting key; the 64 kB body).
- **A fresh database**: the store has never been deployed, and a store file from before the
  agent-record version ceiling, the `fork_signers` table and the key index's `keys.master` and
  `logs.next` columns is not upgraded: start such a store on an empty database. Records
  stored above the store's clock, forks recorded without their signing master and key rows
  without their role would otherwise stay as they are. There are no migrations.
- **Forks**: an event at a seq the store holds, with another hash than the stored one, that
  replays on the stored events before it (its own proofs checked) is a fork. The store records
  it in `chain_forks` with the event's JSON (`SqliteChainStore::record_fork_in`) and its signing
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
- **The freeze** (`db::freezing_fork` on the held log for a signed request, the key index's
  current master for an append; derived from the fork rows each time, no stored flag): the
  identity is frozen only while a recorded fork's signing master is the current master at
  the stored head (one primary-key lookup of `fork_signers`, no replay). A
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
  the current master (`Signer::AnyKey`). The envelope must be signed by its writer, a device key
  without the bot mark valid at the head or the current master (`record::verify_envelope`,
  `400 signer`/`signature`; the writer need not be the request's signer), and wrapped to
  exactly the log's readers at the head (`record::check_sealed`, `400 wrapped`), its version must be the stored one plus
  one, an absent record counting as version 0 so the first write is version 1 (`409 {version}`). A `recoveryKey` needs the current master's
  attestation (`record::verify_attestation`, `400 attestation`). A device-signed write copies
  the stored record's counting recovery key and attestation unchanged (`record::recovery_of`),
  or carries none when that is none (`400 recovery-key`). Only a master-signed write installs,
  replaces or drops it. After a rotation the old master's attestation is refused, and a device
  write drops the key until the new master attests it again. A kept write gets a receipt
  signed by the store's head key (`StoreKey::sign_receipt`, `record::RecordReceipt`,
  `coasys:record-receipt` over `{scid, version, envelope, seq, store}`, `seq` the head the
  write was checked at), stored in `records.receipt` and returned on the PUT and every GET.
  Readers check the writer at the receipt's seq, so the record keeps opening after its writer
  is revoked or the master rotates; a new write is still checked at the head. The signed
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
  them (and old nonces), and `serve` runs it every `SWEEP_EVERY_S` (60 s). The brake's
  per-address buckets cover new no-note logs only; other per-address limits (long polls,
  writes per hour, failed claims) do not exist yet.

## Configuration

| Variable | Default | |
|---|---|---|
| `IDENTITY_STORE_DB` | `identity-store.sqlite` | the SQLite file |
| `IDENTITY_STORE_KEY` | `store-key` | the head key file (32-byte secret, base58btc); `serve` makes one when missing |
| `IDENTITY_STORE_LISTEN` | `0.0.0.0:8080` | port 0 picks a free port |
| `POW_BITS` | `22` | what `GET /v1/pow` publishes and a no-note #0 must meet; the previous bits pass for 10 minutes after a change |
| `BRAKE_GLOBAL_PER_HOUR` | `120` | no-note logs store-wide per hour |
| `BRAKE_ADDR_BURST` | `3` | no-note logs per address at once (IPv6 by /64) |
| `BRAKE_ADDR_PER_DAY` | `3` | what the address bucket refills per 24 h |
| `TRUSTED_PROXIES` | none | comma-separated proxy addresses or ranges (`10.0.0.0/8`) whose `CF-Connecting-IP` / `X-Forwarded-For` the brake reads |
| `VOUCH_URL` | none | the vouching service's base URL; unset: every token is `503 vouch-unavailable` |
| `VOUCH_CREDENTIAL` | none | the store's bearer credential at the vouching service |
| `VOUCH_KEYS` | none | comma-separated Ed25519 multikeys of the trusted vouching keys (`GET {VOUCH_URL}/api/vouch/key` answers the current one) |
| `CORS_ORIGINS` | none | comma-separated browser origins (all routes but the public host profile's, which allow any origin) |
| `FEED_TOKEN` | none | the mirror token; without one the feed answers `401` |
| `IDENTITY_BURST` | `10` | writes per identity at once (the identity bucket) |
| `IDENTITY_REFILL_S` | `60` | seconds per write the identity bucket refills |
| `HELD_LOG_BYTES` | `268435456` (256 MiB) | the most heap the logs held folded in memory take together (`held::held_bytes_of`); at least twice `LOG_FOLD_BYTES`, else `serve` refuses to start |
| `LOG_FOLD_BYTES` | `16777216` (16 MiB) | the per-log budget: the most heap one log's fold may take without the records of revoked keys, and half the most its whole fold may take; an append past either is `400 log-too-large`, unless all its events are `rotate`s |

`serve` prints `identity-store listening on http://<addr> store-key <multikey>` once it listens.

## Tests

```bash
cargo test -p identity-store
cargo clippy -p identity-store --all-targets -- -D warnings
cargo fmt -p identity-store --check
# the agent language suites start this store and drive examples/test_agent.rs:
cd bootstrap-languages/centralized-agent-language && pnpm test
# the same tests inside the image's build stage (what the image workflow runs first):
docker build -f identity-store/Dockerfile --target test .
```

Releases: pushing the tag `identity-store-v<version>` (the version in `Cargo.toml`) makes
`.github/workflows/publish-images.yml` run the tests, push `ghcr.io/coasys/identity-store:<version>`
and write the image's digest into the tag's GitHub release. Deploy by that digest.
