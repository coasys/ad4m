# identity/ — agent guide

The executor's side of `did:scid:coasys:1`: which
identity each local agent's device key speaks for, the key logs it checked, and the
`identity.*` calls (registered with their guards in `api/identity_ws.rs`).

| File | What |
|---|---|
| `mod.rs` | `init` (at start, after the database), `service()`, `IdentitySettings` (`identity.storeUrl`, `identity.storeKeys`, else the network seed's `identityStore`; `identity.allowPrivateSrc`), `write_agent_language_settings`, the `identity-fork` publisher; re-exports `verify_expression` |
| `resolver.rs` | The one place that decides what a log says. `key_valid_at_anchor` and `verify_expression` (both up to the log's trusted seq, `KelCache::trusted`) (an expression by a `did:scid` author with its `proof.record`, for `LanguageController::verify_expression_proof`; the record anchors the key); `Resolver::resolve` (`identity.resolve`): DID URL parsing, sources, the up-front `src` check, the negative cache, `did:key` → person, the DID Core result. Links: `check_signed_with` (the verdict and person of a signed link from the key map, no I/O), `Resolver::author_of` / `lookup_signer` (key map, then the key index confirmed against the log), `Resolver::prepare` (fills the key map before links are stored). Sessions: `Resolver::session_key` / `cached_session_key` (`SessionKey`: a device key's verdict and bot mark in a named log at its trusted seq, the log read from the store when older than the caller allows; `SessionKeyError::StoreUnavailable` fails closed) |
| `key_login.rs` | Login with a device key: `Challenges` (one use, 60 s, at most 10 000 open), `login` (challenge, executor DID, `audience` origin against `Audiences`, signature over `signing_bytes("coasys:session", statement)`, key, role: the operator record's identity gets the operator role, else the local agent that speaks for it, else `not-known`), `renew`, `session_info`, `end_all_sessions`, `sign_statement` (`agent.signSessionStatement`, `.local_only()`: a key-login operator is never "on this machine", on a loopback listener too, so it and every other local-only method refuse it; every other method admits it as the operator, `agent.generate`, `agent.import`, `agent.lock`, `runtime.exportData` and `runtime.importData` included), `run_key_session_watch` (ends a key's grants on its `VerdictChange`, and the 5-minute re-check) |
| `verdict.rs` | `LinkVerdict` (`Valid`, `Invalid(reason)`, `CantCheck`, `Flagged`) and `judge`, a pure function of the signature, the anchor, the signer's log and the space order; `KeyVerdict`, `VerdictChange`, `key_verdict` |
| `src_fetch.rs` | `SrcFetch`: the executor's reader of a foreign `src` over the did:scid host profile (`GET /v1/logs/{scid}?from=`), for `Resolver` only. `PublicOnly` (the client's DNS resolver: public addresses only), `refuse_private_host` (IP literals, `localhost`), no redirects, no proxy, HTTPS only, timeouts, body cap |
| `backend.rs` | `IdentityBackend` (`log(did, from)`, `key_owner`, …), `IdentityError`; `AgentLanguageBackend` calls the agent language through `agent_language_for` (a JS envelope reads refusals back as `IdentityErrorCode`) |
| `cache.rs` | `KelCache`: events in `SqliteChainStore` (`coasys:kel`) in `ad4m_db.sqlite`, the accepted head in `identity_heads`, `refresh` (at most `MAX_PAGES` pages; one fetch per log at a time, and a waiter takes the answer of a fetch that began after it called), `ingest`, `accept` (a lower store head: `Behind` or `Conflict`, a fork only with a valid rival), `trusted` (the log and its accepted head's seq), forks, `reset` (`identity.resetLog`), `refreshed_within` (the in-memory time of each log's last refresh), `store_behind`; fork checks `rival_signer`, `holding_rival` (one fork per signing master, `identity_fork_signers`); `KeyMap` (every trusted log by signing key, and the store's "no log" answers for 15 minutes; `global_key_map`), `subscribe` (a `VerdictChange` when an accepted head changes a key's verdict) |
| `head.rs` | `verify_head` (`ad4m_identity_core::HeadStatement` under a key of `identity.storeKeys`) |
| `device.rs` | the agent's device key, its X25519 `enc` key (`did_key`, no second secret), `WalletSigner`, `store_request` (a store request signed by a device key at a store-signed head) and `body_bytes` (the JCS bytes its `bodyHash` covers) |
| `operator.rs` | `admits_first_link`, `first_link_check`, `main_incept_check`, `record_operator_at_init` (`ad4m-executor init --operator`) |
| `custody.rs` | The custody channel's guard `admit` (the custody credential, `AD4M_CUSTODY_CREDENTIAL` = `OperatorSource::Custody`, on a loopback listener of an executor started with `--custody-channel`; the admin credential and every other caller get 403 `no-custody-channel`; `run` refuses the flag without the variable, exit 78), and `main_device_keys`; its calls are `identity.custodyStatus`, `identity.takeMaster` and a `identity.claimOffer` with `canKeepMaster` |
| `inbox.rs` | The device inbox: `Inbox` (the joined messages opened here, by id and type, never a body; one read of the joined inbox at a time; the open pre-join read; the pull's wake-up), `pull_inbox` (`identity.inbox`), `take_master` (`identity.takeMaster`), `inbox_ack`, `inbox_put`, `prepare_device` (`identity.prepareDevice`: the device string and the pre-join read; `take_ticket` claims and, once claimed, sends `link-ticket`), `run_pull` / `run_schedule` / `wake` (at start, at unlock and binding, every 15 minutes) |
| `record.rs` | `DevicePosition` (an agent's device key at its log's trusted seq: what a store request it signs names; `device_position`, `bound_position`), and the private record and request box calls (`identity.record`, `recordPut`, `unwrapRecordKey`, `requests`, `postRequest`, `deleteRequest`): the request the caller's master key signed, or one the agent's device key signs; a post is the device key's only |
| `rekey.rs` | `identity.rekey` (`.local_only()`): a new main wallet key, nothing in a log; the table of every place that holds the main agent's DID and how each moves. One re-key at a time, in its own task (a dropped caller does not stop it); owners move from every retired main key, so the next re-key repairs what an earlier one left |
| `person.rs` | the person's profile: `person_record` (the agent record under the `did:scid` and its signed request, by the device key at the trusted head), `IdentityService::publish_person_profile` |
| `claim.rs` | `ClaimService::claim_for_main` (`already-linked`, first-link rule, claim with the main wallet key; `canKeepMaster` as the caller says, which only the custody channel may: `custody::admit` on the RPC path, the channel itself on the ticket path), `join_status` (the main agent's claims only) |
| `service.rs` | `IdentityService`: status, log, resetLog, cosignIncept, publish, openOffer, offers, devices; error mapping |
| `types.rs` | the RPC contracts (ts-rs) mirroring `ad4m_ldk::identity`'s JSON, `IdentityForkEvent`, `AgentWithIdentity`; the key session contracts (`KeyStatement`, `SignedKeyStatement`, `SessionChallenge`, `KeySessionResult`, `SessionInfo`, ...) |

Rows in `db/identity.rs`, reached through `db_backend()` (`service::db`): `operator_record`, `identity_agents` (agent `did:key` → SCID),
`identity_joins` (claims to follow; written before the store call, so a claim the store
committed whose answer was lost is still followed by `joinStatus`, and a refused one reports
the offer's state).

## Invariants

- The configured store's logs arrive through the agent language, so a deployment can swap the
  store behind it. A foreign `src` (a host a DID URL names) is read by the executor itself
  over the did:scid host profile (`src_fetch.rs`), where the address policy can be enforced at
  connect time; its events are folded in memory for that one answer and never cached, its head
  is only a paging hint. Nothing else here speaks HTTP.
- **A key statement names the origin it was signed for.** `KeyStatement.audience` is the
  origin the client dialed (`scheme://host[:port]`), signed under `coasys:session` with the
  rest; `agent.signSessionStatement` signs it as given, so the person's app shows and signs
  the server it talks to. `login` and `renew` accept it only when, normalised
  (`normalise_origin`: `ws`/`wss` read as `http`/`https`, host lowercase, default port
  dropped), it is one of `Audiences`: the configured `public_origins` (`--public-origin`),
  else this executor's own loopback origins (`http://127.0.0.1:<port>`,
  `http://localhost:<port>`, `http://[::1]:<port>`). Without `public_origins` a non-loopback
  audience is refused as `no-public-origin`, a reason that names the setting; any other
  mismatch is `audience`. Nothing from the connection (`Host`, `Origin`, the listener) is
  read: the caller controls those. This is what stops a relay: a server the person added
  hands the app this executor's challenge and DID, but the app signs the server's own
  origin, and this executor refuses that. The executor-DID check stays as well. The
  challenge is spent before any of these checks, so a refused statement cannot be retried.
- The cache only extends a log. A different event at a held seq that holds (below) records a
  fork, appends nothing and fires `identity-fork` once per fork, to the sessions of the
  identity's local agents.
- **A store that answers less is behind, not a fork.** A shorter log never truncates a held
  one, and it is no evidence by itself (a store restored from a backup answers one). In
  `KelCache::accept`, a store-signed head below the cached head is compared with the cached
  event *at the head's own seq*:
  - the same hash: the store is behind on the cached branch. `CacheError::Behind`: nothing
    recorded, nobody alerted, the cached log and its accepted head stay (the trusted seq does
    not move back). Callers treat it as a soft miss: `refreshed_or_cached` and the resolver
    answer from the cache (`identity.resolve` lists the store in `coasys:skipped`, "the store
    is behind"), and the next refresh window reads the store again. `KelCache::refresh`
    notes it per log (`store_behind`, in memory): `identity.status` answers
    `storeBehind: { since, seq }` (the first refresh that saw it, the store's head seq or
    `null` for no log) until a refresh succeeds, or `resetLog`.
  - another hash: a signed head names a hash, not an event, so it is no fork either. The
    store's events at or below that seq go through the same import as `ingest`; `fetch`
    re-reads the store's log from seq 0 up to the head's seq once for them (page after page,
    at most `MAX_PAGES` and `MAX_STORE_LOG_BYTES` (32 MiB, one `httpFetch` reply) of events served, copies included:
    `store_events_to`; past it the store is `Unavailable`), since the page from the cached
    head holds none.
    A competing event that holds (`holding_rival`) records the fork, evidence at
    that event's seq (ours vs theirs there). Without one: `CacheError::Conflict`, evidence at
    the head's seq (our hash there vs `head.hash`), nothing recorded, nobody alerted, the
    cache answers (`coasys:skipped` with the conflict).
  - A head at the cached seq with another hash than the cached event is a `Conflict` too.
  - A store that answers no log (`null`) of one it signed a head of (under a key of the current
    `identity.storeKeys`) is `Behind` (`store: None`); without such a head it is `NotFound`
    (a log only a foreign `src` served, or a former store's head, says nothing about the store).
  `identityLog` answering `undefined` is refused (`backend::Undefined::Refused`), never read as
  no log.
- **A different event counts only when it is valid.** `import` compares hashes at a held seq
  and checks no proof there, so before a fork is recorded (or a `src` conflict reported as
  `coasys:fork`) the competing event goes through `cache::holding_rival`: first
  `rival_signer`, one proof check against the master of the log current before its seq (for a
  `rotate`, the key it names): the event must carry exactly that one proof (a later event has
  one signer, and `KelPolicy` refuses any other proof), so a body no master signed alone is
  refused before the prefix is read; then the event steps on the core's `fold` of the held
  prefix below it (`KelChain::appended`), every rule and proof. Every distinct copy with the
  rival's hash is tried, as `import` tries them (a copy with its proofs stripped hashes the
  same); exact repeats are dropped, the prefix folds once and each remaining copy steps on a
  copy of it, so a host serving many copies (repeats, or each with another junk proof) costs
  one fold and no copy of it per junk copy. One that does not hold records nothing and alerts nobody:
  `ingest` refuses it as `CacheError::Fold`, and a foreign `src` serving it is skipped
  (`coasys:skipped`, "does not hold") and not read again for `MISS_TTL`. Without this a store
  or host with no key could raise a persistent fork alert per body it makes up.
- **One fork per signing master**, as the identity store keeps: the cache records the rival's
  signer beside the fork (`identity_fork_signers`), and a further rival by a master with a
  recorded fork adds no row and no alert and is not read or folded again. A leaked retired
  master raises one alert, however many bodies it signs.
- **An event that names a member twice is refused where its text is.** A `Value` keeps only
  the last of the two, so `Event::from_json` cannot see it. A foreign `src` page is read as
  `Vec<Event>` by `Event`'s `Deserialize` (`src_fetch::HostPage`); the events of an
  `identity.publish` or `identity.cosignIncept` frame are read from the frame's text, and a
  frame that repeats `params`, `events` or `event` is refused too
  (`identity_ws::duplicate_member_refusal`, `invalid: DuplicateMember`), after the method's
  guard admits the caller (a caller it refuses gets the access answer); a store's
  page reaches the executor as values through the language runtime, so the agent language's
  `identityLog` refuses it (`invalid`, `DuplicateMember`).
- **A signed head proves authenticity, not freshness.** `verify_head` checks the store's
  signature and never reads `time` (the head's append time, not the read's): a store, or
  anyone replaying its answer, can serve an older signed head, and no signed statement proves
  a read is current. The cache never moves back (a lower head is `Behind` or a `Conflict`), and
  `ADMIT_FRESHNESS` bounds how long a positive verdict goes without a re-read; neither proves
  the store showed its latest head.
- **A fork has one way out: `identity.resetLog { did }`** (`Guard::Operator`). It drops this
  executor's copy of that log (its `coasys:kel` events, its `identity_heads` row, its
  `chain_forks` and `identity_fork_signers` rows, the log in memory, the resolver's confirmed
  keys and failed fetches of it), logs a warning naming the DID and answers `true`; the next
  read refreshes from the configured store and takes the store's branch. Bindings in
  `identity_agents` stay.
- A head from the agent language counts only under `identity.storeKeys`; `ingest` takes the
  configured store's events with no head (a store head no key of `identity.storeKeys` signed).
- **Authorship reads a log only up to its trusted seq.** A log listing a key does not prove its
  holder agreed: an `incept` carries every listed key's signature, an `add` only the master's
  (its claim never enters the log, so only the store checks it; spec section 11). So every read
  that maps a key to a person or decides authority goes through `KelCache::trusted` and takes
  the key as a device key only (`resolver::device_key_id`: a master signs the log, never for
  the person): the seq of the accepted head, while it still verifies under the current
  `identity.storeKeys` and the cached event at that seq has its hash. No such head: no key
  speaks for that person. The state counts at that seq: a key added past it does not speak yet, a revocation past it does not
  count yet. The readers: `key_speaks_for` (so `IdentityService::identity_of`),
  `key_valid_at_anchor` (so `verify_expression`), the `did:key` confirmation in `resolve_key`,
  `record_agent_if_listed` (binding a local agent to an identity; a bot key binds none) and `main_is_linked`
  (`already-linked`). Events past the trusted seq, and every event a foreign `src` served, are
  for resolution display only (`identity.resolve` shows them, `coasys:headTrusted: false`).
  `identity.status`, `identity.log` and `identity.devices` show the cached log as it is.
  Without a store key no `did:scid` author verifies, and `identity.publish` and
  `identity.claimOffer` refuse with `no-store-keys` (naming the cause, `no_store_keys_cause`:
  no store, a store URL configured without its keys, or a seed's store without keys) before
  the store commits anything: a
  log the executor can never accept would still take the agent's key for every identity.
- **The store is the network's.** A network is its bootstrap seed (`<data>/mainnet_seed.seed`,
  which `init` writes from the seed compiled in, `mainnet_seed.json`, or from
  `--network-bootstrap-seed`); the seed's optional `identityStore` (`{ url, keys }`,
  `runtime_service::IdentityStoreSeed`, carried from `cli/seed_proto.json` by
  `dev generate-bootstrap`) names its store. The compiled-in seed names
  `https://store.ad4m.dev`, the store of today's (staging) network; another network is
  another seed. A seed with no `identityStore` means the store of the network this executor
  was built for, the compiled-in seed's (`RuntimeService::identity_store`, parsed once): a
  seed file written before the field existed keeps working on upgrade without re-running
  `init` (which can wipe agent keys); a seed for another network names its own store. The
  `identityStore` object ignores fields it does not know, so a newer seed stays readable;
  `url` and `keys` are required. There is no built-in default store in code beyond the
  compiled-in seed (none in the agent language).
  `IdentitySettings::from_config(config, seed_store)` is the one place that picks the store
  and keys, the config winning: URL → `identity.storeUrl`, else the seed's, else none; keys →
  `identity.storeKeys` when set (exactly those, `[]` too), else none when `identity.storeUrl`
  is set (the seed's keys never apply to a configured URL), else the seed's, else none. Each
  value records where it came from (`StoreSource`: `store_url_source`, `store_keys_source`),
  so the start-up checks (`IdentitySettings::check`: `check_store_keys`, `check_store_url`),
  the warning and the `no-store-keys` refusal name `identity.storeUrl`/`identity.storeKeys`
  or the seed's `identityStore`, whichever the operator has to fix.
  `lib.rs` loads the seed (`RuntimeService`) before `identity::init`, and
  `settings_from_config()` reads it for the agent language's settings. Without a key `init`
  logs `missing_store_keys_warning` at start: "this executor has no identity store" without a
  URL (only when no seed is loaded, or the compiled-in seed names none), else the store's URL
  and where its keys were missing (`identity.storeKeys` or the seed's `identityStore.keys`).
  `identity.status.store` (`StoreInfo { url, trusted }`, `null` without a store; typed in
  `ad4m-identity-client` as `IdentityStatus::store`) tells apps which store this executor uses and
  whether it trusts a key for it. Without a store the agent language answers every identity
  call `invalid` (`store-url`), which reaches callers as an error, never a panic.
- Every claim for the main agent goes through `ClaimService::claim_for_main`, which runs the
  first-link rule itself: `already-linked` while the main key is valid in its identity's log at
  the trusted seq, `no-operator` without a record, `first-link` for another identity. One
  exception (`FirstLink::FirstRun`): a `--custody-channel` executor (a desktop app, which never
  runs `init --operator`) with no record and no identity for its main agent claims the first
  offer, and once the store accepts that claim records the offer's identity as operator
  (`OperatorSetBy::FirstRun`, `set_operator_record_if_empty`). From then on the rule applies as
  above: a claim for any other identity is `first-link`. Both the ticket path and
  `identity.claimOffer` (a pasted link) go through `claim_for_main`, so both get it.
- With an operator record the main key may speak only for the operator's identity:
  `cosignIncept` for the main agent runs `main_incept_check` and refuses (`first-link`) an
  incept of any other identity, and `publish` of an incept by the main agent runs it again
  before the store commits (an incept co-signed before the record existed). Managed users are
  not bound by the record.
- `cosignIncept` and `publish` bind the caller's agent for good, and every app holds
  `agent:SIGN` and `agent:UPDATE` by default, so their guard is `Guard::UserOrOperator`: for
  the main agent only the operator (the admin or the custody credential), never an
  app token on the main agent; a user's session (or an app the user approved) for its own
  managed agent.
- The store's URL and keys (`IdentitySettings`) reach the agent language only through
  `write_agent_language_settings` (atomic: temp file + rename), as its `storeUrl` and
  `storeKeys` settings, on every start: no store or no keys removes the setting, so an earlier
  run's store never stays in force. The language keeps a `did:scid` log only under a head
  signed by a key of `storeKeys`. An empty store URL, configured or from the seed, stops
  startup (`check_store_url`: the language would read it as unset). When the write fails,
  `LanguageController::install_agent_language` does not load the agent language, so no
  profile or identity call goes to an older store; identity calls answer `unavailable`.
- **A `did:scid` author counts only on the agent language's profile.** An expression's author
  is its signing device's `did:key`; the person is derived from the signer's log, never
  written as author. The one exception is the profile the agent language serves with the
  agent record its store admitted. So `LanguageController::verify_expression_proof` takes a
  `did:scid`-authored expression as valid only when the agent language served it
  (`LanguageController::honours_record`) and it carries a non-null `proof.record`; any other
  (another language, a literal, the language language, or no record) is not valid, whatever
  its signature. The store takes a `did:scid` record only with an anchor, and the agent
  language answers a `did:scid` profile only with its `proof.record`, so no legitimate
  profile lacks one.
- Such a profile verifies only under its `proof.key` (a bare `did:key`) and only when its own
  signature holds under that key, at the record's anchor (`verify_expression`):
  - `proof.record` is `{version, anchor, sig}`, which the agent language adds to a
    `did:scid` profile from its agent record. `sig` must be the key's
    `coasys:agent-record` signature over `{profile, version, anchor}`, `profile` the expression without
    `proof.record` (and without `proof.valid`/`proof.invalid`), as `AgentRecord::signed_value`
    builds it; the anchor must be at or below the trusted seq (not merely the cached head);
    the log must list the key as a device key, valid at the anchor by the clock now
    (`key_valid_at_time`) and without the bot mark there. A profile signed before a revocation
    stays valid after it. A `proof.record` that does not hold (malformed, an unknown member, a
    bad signature, an anchor past the trusted seq, a key not valid there) makes the expression
    invalid. The anchor counts only through the record signature.
  - The record counts only from the agent language: its store refuses a record anchored
    before a revocation of its key, while anyone else can wrap a fresh signature by a revoked
    key in a record naming an earlier anchor.
  - `verify_expression` takes the record: there is no rule for a `did:scid` expression
    without one.

  The expression's own timestamp never counts. When the cached log does not admit the key (no
  log or no store-signed head yet, a trusted seq below the anchor, or a device added since),
  the read refreshes that one log from the configured store (which advances the accepted
  head) and checks again; a store that cannot answer leaves it `valid: false`. That refresh
  too happens at most once per `ADMIT_FRESHNESS` window per log, so a key that does not speak,
  or an author the store lacks, never costs a store call (up to its 15 s timeout) per read.
  The in-memory refresh times are bounded (4096 logs): past the bound the oldest one goes.
- **A positive verdict is fresh for `resolver::ADMIT_FRESHNESS` (60 s).** When the cached log
  admits the key but last refreshed from the configured store longer ago than that (the time is
  kept in memory only, so after a restart every log is stale), the read refreshes it once and
  checks again, so the cached log follows the store within the window. The anchor fixes a
  profile's verdict: a revocation the refresh brings in does not touch a profile anchored
  before it. Offline policy: when that refresh fails (the store unreachable or behind, a
  conflict, a fork) the cached trusted head
  answers, and the failed attempt counts as the window's read, so an unreachable store is tried
  once per window per log, not on every read.
- An agent keeps the first identity it joined (`identity_agents`); `agent.status` and
  `agent.get` carry it as `identity` only while the agent speaks for it
  (`IdentityService::identity_of`: `key_speaks_for` at the trusted seq, no store read), so after
  a revocation of its key reaches the trusted seq they answer `identity: null` while
  `identity.status` still shows the binding with `revoked`. `did` stays the device `did:key` (and
  `agent.status().didDocument` the device's did:key document; `identity.resolve` serves the
  person's).
- **An agent that speaks for a person publishes the person's profile** under the `did:scid`
  (`person.rs`, `identityPutAgent`), on the first binding (`record_agent_if_listed`: a claim
  spent, or `publish` of a log listing the key; main agent or managed user) and on every
  profile publish after the did:key one (`AgentService::publish_profile`). It speaks for the
  person while `identity_agents` binds it and its device key is listed, valid and without the
  bot mark at the trusted seq. The record: the agent's profile with `did` the `did:scid`, as an
  expression authored by the `did:scid` and signed by the device key (`proof.key` the bare
  `did:key`), `anchor` the trusted seq, `version` `person::next_version`: the time in ms, or
  one past the version the store holds (`IdentityBackend::agent_version`, the agent language's
  `expressionGet` of the `did:scid`, `proof.record.version`) when that is not below it, so a
  device whose clock runs ahead never blocks the others; the signed request: the same
  key at the accepted head, for the store that signed that head. A failure is logged, never
  fails the did:key publish, and the next profile publish tries again. The record is signed
  under `coasys:agent-record` (`AGENT_RECORD_DOMAIN`), which the store and the agent language
  check; the store refuses a `version` more than 300 s past its clock.
- **After unlock every profile is published again**: the main agent's, then each managed
  user's with a stored profile (`AgentService::publish_managed_profiles`, from
  `unlock_main_agent`), so profiles an earlier agent language held reach the configured store.
  A failure is logged and the next user is tried; so is a publish that takes longer than
  `MANAGED_PROFILE_PUBLISH_TIMEOUT` (60 s), which is dropped and so frees the publish lock.

## Links: anchor, verdict, person

- **The anchor.** A link that a bound agent signs (`agent::create_signed_link`,
  `identity::anchor_for`) carries `proof.anchor`: the trusted seq of its identity's log, when
  that log lists the agent's key as a device key. Its signature covers
  `coasys:link` `{data, timestamp, anchor}` (`signatures::anchored_signing_bytes`), the
  timestamp as the link carries it. A link without an anchor signs the old bytes. The anchor
  is in the proof on the wire and in the link store (`ad4m://ontology/proofAnchor`).
  Signing is synchronous: the anchor comes from the key map, with no I/O. The logs of local
  identities (rows in `identity_agents`) are loaded at start (`load_local_identities`, before
  the API and perspectives) and on each binding (`KelCache::keep_local`), and are pinned
  (`KeyMap::pin`), so the map's bound never drops them; a link signed after a newer head is
  accepted carries the new anchor.
- **The key map follows the trusted seq only.** `KelCache` puts a log in it from `trusted` and
  from `accept` when a head is accepted (`publish_trusted`), and nowhere else. A store
  `Behind`, a `Conflict` or a fork leaves the accepted head, so the map keeps answering the
  cached trusted log: `lookup_signer` reads the cache after a failed refresh (only
  `Unavailable` answers `Unknown` and trips `BREAKER`). A fork is decided only by the cache's
  and the resolver's `holding_rival`; the key lookups never judge a rival themselves.
  `resetLog` drops the log from the map (`KeyMap::forget`) with the rest of the copy, and the
  store-behind note with it.
- **One check.** `signatures::verify`, `LinkExpression::check` and
  `DecoratedLinkExpression::check` all go to `identity::check_signed`: the signature over the
  bytes the proof names, then `verdict::judge` on what the key map holds. The check does no
  I/O, so it runs the same in the store, in flows and in every recompute.
  `identity::prepare_link_signers` fills the key map first (`persist_link_diff`, the
  re-check).
- **The rules** (`judge`). A bad signature: `invalid`. No anchor and no log that lists the
  key: `valid`, as before key logs. No anchor and a log lists the key: judged as anchored at
  the seq that added the key. With an anchor: no log lists the key, `invalid`; nothing known
  or the anchor past the trusted seq, `cantCheck`; a master key, `invalid`; the key added
  after the anchor or revoked at or before it, `invalid`; a bot key whose scope is past its
  `expires` by the clock now, `invalid`. A key revoked after the anchor: `flagged` when the
  link entered the space's ordered history after the `revoke`, else `valid`. No link language
  gives that order today, so `check_signed` passes none.
- **Unknown is not "no log".** A failed key-index call answers `Unavailable` and is never
  kept; the index is not asked again for 30 s (`BREAKER`). An unanchored link by an unknown
  key stays `valid` with no person; an anchored one is `cantCheck`.
- **The person** is derived, never on the wire: the `did:scid` of the signer's log (a bot
  key: its owner), only for a `valid` link. The link store keeps it beside the verdict
  (`ad4m://ontology/person`, `ad4m://ontology/linkVerdict`) for SPARQL; `proofValid` is `true`
  exactly for `valid`. A found person stays for good; "no log" holds for 15 minutes.
- **The re-check** (`run_recheck`, started by `init`): on each `VerdictChange` the links of
  that key, and every 60 s the `cantCheck` links, are judged again and rewritten when the
  verdict or person changed (`PerspectiveInstance::recheck_links`, at most 10 000 links per
  perspective per pass).
- **Reads.** Logic reads (model queries, flows, SHACL, SDNA facts) count only links with
  `proofValid` `true`. The display reads `perspective.queryLinks` and `perspective.snapshot`
  (`get_links_shown`) leave out `invalid` links and return `cantCheck` and `flagged` ones with
  `verdict`; with `includeUnverified: true` on the `LinkQuery` they return `invalid` links too
  (so an owner can find and remove them; removal does not check the verdict).
  `PerspectiveInstance::get_links` returns every stored link with its verdict.

## DID resolution (`identity.resolve`)

Spec: `docs-src/identity/did-scid-coasys-1.md`, sections 8 (Resolution), 9 (DID document),
10 (Where `src` goes). Guard `Cap(agent:READ)` like `identity.log`: logs are public.

- **Input.** `did:scid:coasys:1:<scid>[?src=…&versionId=…][#<keyId>]`, values and fragment
  percent-decoded, or a bare device `did:key:z6Mk…`. `versionTime` is `invalidDidUrl` (the log
  holds no time); a parameter given twice, a bad `%` escape, a path, a non-decimal `versionId`
  are `invalidDidUrl`; a malformed DID is `invalidDid`; another method is `methodNotSupported`;
  an `accept` other than `application/did+ld+json` is `representationNotSupported`.
- **Answer.** Always a result, never an RPC error: `{didDocument, didDocumentMetadata,
  didResolutionMetadata, verificationMethod?}` (`types::DidResolutionResult`). `didDocument`
  is the core's JSON-LD (`did_document(chain, versionId, alsoKnownAs)`, `alsoKnownAs` =
  `[<did>?src=<identity.storeUrl host[:port][/path]>]` when a store is configured). A fragment
  dereferences one method: `didDocument` null, `verificationMethod` the method with an absolute
  `id` (`<did>#d1`). Errors: `didResolutionMetadata.error` (DID Core code) and `coasys:message`.
  Metadata: `coasys:source` (the host that answered, or `cache`), `coasys:headTrusted` (the
  answered log's head is the one `KelCache::trusted` gives: the accepted head, verifying under
  the current `identity.storeKeys`), `coasys:fork` (the first recorded fork, else a conflict
  between this answer's `src` and the cache whose competing event holds), `coasys:skipped`
  (sources not used, with why).
- **Sources.** The cache answers unless `fresh`, or it lacks the log or the asked
  `versionId`. Then: the URL's `src`, then the configured store. A `src` naming the configured
  store (same host, port, path; scheme ignored) is the configured store. The configured store
  goes through `KelCache::refresh` (its head counts; a head no key of `identity.storeKeys`
  signed: `fetch_store_unsigned`, events via `ingest`; a log past `MAX_PAGES` pages there is
  unavailable, as on the signed path, so a prefix never answers as the store's log). A foreign
  `src` goes through
  `fetch_foreign` and `SrcFetch` (`GET https://<src>/v1/logs/<scid>?from=<n>`, from the cached
  head, at most `MAX_PAGES` pages and `MAX_SRC_LOG_BYTES` (1 MiB) in all; head only a paging
  hint, read as `{events, head: {seq}}` since a foreign host's head need not be signed). Its
  events are folded in memory on the cached log for this answer only: nothing of a foreign
  `src` is cached, so `identity.resolve` from any `agent:READ` session cannot fill the shared
  cache. The configured store is read on in any case (into the cache), so a `src` serving a
  prefix cannot hide an event the store holds (a revocation): the answer is the `src`'s events
  on the cache's log, `coasys:source` the `src`. When the two differ at a seq both hold, the
  cache's log answers with the conflict as `coasys:fork` when the `src`'s event holds on the
  prefix (`Resolver::verified_conflict`, `holding_rival`), and no fork is recorded; when it does
  not, the `src` is skipped with why, no `coasys:fork` is set, and the (host, SCID) is skipped
  for `MISS_TTL` whether the conflict showed while fetching or while merging. The cache
  only extends; a fork with the configured store is recorded and the held log answers. A
  failed foreign fetch is remembered per (host, SCID) for 5 minutes (`MISS_TTL`), `fresh` or
  not.
- **`did:key`.** The store's key index (`IdentityBackend::key_owner`, the language's
  `identityKeyOwner`, `GET /v1/keys/{multikey}`) is a hint: the named log (cache, then store;
  re-read once from the store when the cached log does not list the key up to its trusted seq)
  must list the key as a device key added at or below the trusted seq. Answer: the person's
  document at the head with `coasys:keyId` and `coasys:keyValid` (`key_valid_at_time` at the
  trusted seq by the clock now, as `key_speaks_for`; a revoked key, or a bot key past its scope's
  `expires`, resolves `false`). Unknown key: `notFound`. A log that does not list it:
  `notFound` with `coasys:keyIndexMismatch: true` and a warning in the log. A log that lists it
  only past its trusted seq (or has no store-signed head, e.g. only a foreign `src` served it):
  `notFound`, no mismatch flag. A confirmed key → SCID sits in a bounded in-memory map.
- **`src` and SSRF.** A `src` comes from untrusted input and executors run on servers. Public
  means `resolver::is_public`: no loopback, RFC 1918, fc00::/7, link-local, CGNAT 100.64/10,
  multicast, unspecified, 0/8, 240/4, 192.0.0/24, and IPv4-mapped/compatible/NAT64
  (`64:ff9b::/96`)/6to4 forms of those; no local-use NAT64 `64:ff9b:1::/48` at all. Up front, without a connection (`coasys:skipped` says why): HTTPS only (another
  scheme is skipped), no user info, query or fragment; an IP literal must be public and a
  `localhost` name is refused (`src_fetch::refuse_private_host`).
- **The transport enforces the address policy.** `SrcFetch`'s reqwest client resolves names
  only through `PublicOnly` (`ClientBuilder::dns_resolver`): the lookup made for the
  connection itself, filtered to public addresses; with none left the lookup fails and no
  connection is made. So a name that answers a public address once and a private one later
  (DNS rebinding) never reaches the private one. An IP literal skips the resolver, so
  `SrcFetch::page` checks it again before the request. Redirects are not followed
  (`redirect::Policy::none()`: a 3xx is a failed fetch), no proxy is used (a proxy would
  resolve the name itself), `https_only`, a 5 s connect and 15 s request timeout, and the body
  is read in chunks and refused past `MAX_LOG_PAGE_BYTES`. `identity.allowPrivateSrc`
  (`--identity-allow-private-src`, `AD4M_IDENTITY_ALLOW_PRIVATE_SRC`; off by default, warns at
  start) drops the public filter and the up-front IP check for tests and local development;
  redirects stay unfollowed.

## The device inbox (`inbox.rs`, spec section 17)

- A message is sealed to the device's `enc` key, the X25519 form of its wallet key: only
  `WalletBackend::open_sealed` opens it (the secret and its X25519 form stay in that call), and
  every opened payload (`Opened`) wipes its secret (a master key, an offer's `s`) on drop. A
  locked wallet opens nothing and deletes nothing.
- **The joined inbox** of the main agent is read at start, at each `inbox::wake` (the wallet
  unlocked: `agent.generate`/`agent.unlock`; the main agent bound to an identity) and every
  15 minutes (`run_pull`), and by `identity.inbox`; one read at a time (`Inbox::pulling`).
  **With the custody channel** each new message is opened for its body type, wiped at once,
  left in the store, and announced once (`inbox-message {id, kind}`, audience
  `Audience::LocalOperator`); one that does not open is deleted. **Without it** (a server, a
  host, Coasys cloud) the executor keeps no master key, and a master key is the only body a
  joined inbox carries, so each message is deleted at the pull unopened, with an error in the
  log, and nothing is listed or announced.
- **`identity.takeMaster`** (custody channel, `custody::admit`) refreshes the log, opens the
  joined messages oldest first and runs `check_payload` against the cached log: `KeepMaster`
  answers `TakenMaster {messageId, did, index, secret}` (wiped when dropped, never cloned,
  `Debug` without the secret) and leaves the message for `identity.inboxAck`; `Stale` and a
  failed check are deleted and the next one is tried; none answers `null`; a locked wallet is
  503 `unavailable`. The secret goes into the RPC answer only: no store, log line, event or
  language sees it.
- **`identity.prepareDevice`** makes the main key's device string (`sign`, `enc`, a fresh
  `r`, `exp` 5 minutes ahead, `identity.storeUrl`) and reads `Pre(a)` with pre-join reads
  (`sign_pre_auth` by the device key, for the first key of `identity.storeKeys`) until `exp`,
  at most one read a second; a new call ends the last read. `claim_for_main` (the first-link
  rule) claims a ticket that passes `check_payload` (its signed offer checked against the
  identity's log, refreshed from the store) with `canKeepMaster` exactly when the custody
  channel is on; a claim sends `link-ticket {did, offerId, matchCode}` to the operator on this
  machine, a refused one sends nothing; the message is deleted either way. A ticket whose log
  cannot be read now waits for the next round; a store that is behind (`Behind`, `Conflict`)
  is no such case: the cached log at its trusted seq checks the ticket. A master key in a pre-join inbox is dropped
  with an error in the log.
- The record and the request box pass a request the caller signed with the identity's master
  key (the custody service, a restoring app), or sign one with the agent's device key at its
  `DevicePosition`; `identity.postRequest` always signs with the device key (the store refuses
  a master key's post). The executor never decrypts the record, so `identity.devices` has no
  names: the app joins the opened record's entries by key id. The record key is wrapped to
  this executor's `enc` key, and an app whose device is this executor (the desktop's custody
  service) has no `enc` key of its own: **`identity.unwrapRecordKey`** (operator, local only)
  opens the record key of the main agent's entry in the wallet (`WalletBackend::unwrap_record_key`,
  the secret stays in that call) and answers it (`RecordKey`, wiped on drop); the app decrypts
  the record with it.

## Do not

- Do not read the words here, or a master key outside `identity.takeMaster`: the executor
  holds device keys only; a master key opens here only inside that call and is never kept.
- Do not trust a store's hash or head: `import`/`fold` and `verify_head` decide.
- Do not pass a `src` to the backend (the agent language reads only its store), and do not
  read a foreign host with another client than `SrcFetch`.
