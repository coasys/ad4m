# identity/ — agent guide

The executor's side of `did:scid:coasys:1`: which
identity each local agent's device key speaks for, the key logs it checked, and the
`identity.*` calls (registered with their guards in `api/identity_ws.rs`).

| File | What |
|---|---|
| `mod.rs` | `init` (at start, after the database), `service()`, `IdentitySettings` (`identity.storeUrl`, `identity.storeKeys`, `identity.allowPrivateSrc`), `write_agent_language_settings`, the `identity-fork` publisher; re-exports `verify_expression` |
| `resolver.rs` | The one place that decides what a log says. `key_valid_at_anchor` and `verify_expression` (both up to the log's trusted seq, `KelCache::trusted`) (an expression by a `did:scid` author with its `proof.record`, for `LanguageController::verify_expression_proof`; the record anchors the key); `Resolver::resolve` (`identity.resolve`): DID URL parsing, sources, the up-front `src` check, the negative cache, `did:key` → person, the DID Core result. Links: `check_signed_with` (the verdict and person of a signed link from the key map, no I/O), `Resolver::author_of` / `lookup_signer` (key map, then the key index confirmed against the log), `Resolver::prepare` (fills the key map before links are stored). Sessions: `Resolver::session_key` / `cached_session_key` (`SessionKey`: a device key's verdict and bot mark in a named log at its trusted seq, the log read from the store when older than the caller allows; `SessionKeyError::StoreUnavailable` fails closed) |
| `key_login.rs` | Login with a device key: `Challenges` (one use, 60 s, at most 10 000 open), `login` (challenge, audience, signature over `signing_bytes("coasys:session", statement)`, key, role: the operator record's identity gets the operator role, else the local agent that speaks for it, else `not-known`), `renew`, `session_info`, `end_all_sessions`, `sign_statement` (`agent.signSessionStatement`: the admin credential or loopback only), `run_key_session_watch` (ends a key's grants on its `VerdictChange`, and the 5-minute re-check) |
| `verdict.rs` | `LinkVerdict` (`Valid`, `Invalid(reason)`, `CantCheck`, `Flagged`) and `judge`, a pure function of the signature, the anchor, the signer's log and the space order; `KeyVerdict`, `VerdictChange`, `key_verdict` |
| `src_fetch.rs` | `SrcFetch`: the executor's reader of a foreign `src` over the did:scid host profile (`GET /v1/logs/{scid}?from=`), for `Resolver` only. `PublicOnly` (the client's DNS resolver: public addresses only), `refuse_private_host` (IP literals, `localhost`), no redirects, no proxy, HTTPS only, timeouts, body cap |
| `backend.rs` | `IdentityBackend` (`log(did, from)`, `key_owner`, …), `IdentityError`; `AgentLanguageBackend` calls the agent language through `agent_language_for` (a JS envelope reads refusals back as `IdentityErrorCode`) |
| `cache.rs` | `KelCache`: events in `SqliteChainStore` (`coasys:kel`) in `ad4m_db.sqlite`, the accepted head in `identity_heads`, `refresh` (at most `MAX_PAGES` pages), `ingest`, `accept`, `trusted` (the log and its accepted head's seq), forks, `reset` (`identity.resetLog`), `refreshed_within` (the in-memory time of each log's last refresh); `KeyMap` (every trusted log by signing key, and the store's "no log" answers for 15 minutes; `global_key_map`), `subscribe` (a `VerdictChange` when an accepted head changes a key's verdict) |
| `head.rs` | `verify_head` (`ad4m_identity_core::HeadStatement` under a key of `identity.storeKeys`) |
| `device.rs` | the agent's device key, its X25519 `enc` key (`did_key`, no second secret), `WalletSigner` |
| `operator.rs` | `admits_first_link`, `first_link_check`, `main_incept_check`, `record_operator_at_init` (`ad4m-executor init --operator`) |
| `person.rs` | the person's profile: `person_record` (the agent record under the `did:scid` and its signed request, by the device key at the trusted head), `IdentityService::publish_person_profile` |
| `claim.rs` | `ClaimService::claim_for_main` (`already-linked`, first-link rule, claim with the main wallet key, never `canKeepMaster`), `join_status` (the main agent's claims only) |
| `service.rs` | `IdentityService`: status, log, resetLog, cosignIncept, publish, openOffer, offers, devices; error mapping |
| `types.rs` | the RPC contracts (ts-rs) mirroring `ad4m_ldk::identity`'s JSON, `IdentityForkEvent`, `AgentWithIdentity`; the key session contracts (`KeyStatement`, `SignedKeyStatement`, `SessionChallenge`, `KeySessionResult`, `SessionInfo`, ...) |

Rows in `db/identity.rs`: `operator_record`, `identity_agents` (agent `did:key` → SCID),
`identity_joins` (claims to follow).

## Invariants

- The configured store's logs arrive through the agent language, so a deployment can swap the
  store behind it. A foreign `src` (a host a DID URL names) is read by the executor itself
  over the did:scid host profile (`src_fetch.rs`), where the address policy can be enforced at
  connect time; its events are folded in memory for that one answer and never cached, its head
  is only a paging hint. Nothing else here speaks HTTP.
- The cache only extends a log. A different event at a held seq, or a signed head below the
  cached one (a shorter log never truncates a held one), records a fork, appends nothing and fires
  `identity-fork` once per fork, to the sessions of the identity's local agents. A store that
  answers no log records a fork only when the cache holds a head that store signed; a log only
  a foreign `src` served is no evidence against the store.
- **A fork has one way out: `identity.resetLog { did }`** (`Guard::Operator`). It drops this
  executor's copy of that log (its `coasys:kel` events, its `identity_heads` row, its
  `chain_forks` rows, the log in memory, the resolver's confirmed keys and failed fetches of
  it), logs a warning naming the DID and answers `true`; the next read refreshes from the
  configured store and takes the store's branch. Bindings in `identity_agents` stay.
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
  count yet. The readers: `key_valid_at_anchor` (so `verify_expression`), the `did:key`
  confirmation in `resolve_key`,
  `record_agent_if_listed` (binding a local agent to an identity) and `main_is_linked`
  (`already-linked`). Events past the trusted seq, and every event a foreign `src` served, are
  for resolution display only (`identity.resolve` shows them, `coasys:headTrusted: false`).
  `identity.status`, `identity.log` and `identity.devices` show the cached log as it is.
  Without `identity.storeKeys` no `did:scid` author verifies, and `identity.publish` and
  `identity.claimOffer` refuse with `no-store-keys` before the store commits anything: a log
  the executor can never accept would still take the agent's key for every identity.
- Every claim for the main agent goes through `ClaimService::claim_for_main`, which runs the
  first-link rule itself: `already-linked` while the main key is valid in its identity's log at
  the trusted seq, `no-operator` without a record, `first-link` for another identity.
- With an operator record the main key may speak only for the operator's identity:
  `cosignIncept` for the main agent runs `main_incept_check` and refuses (`first-link`) an
  incept of any other identity. Managed users are not bound by the record.
- `cosignIncept` and `publish` bind the caller's agent for good, and every app holds
  `agent:SIGN` and `agent:UPDATE` by default, so their guard is `Guard::UserOrOperator`: for
  the main agent only the operator (the admin credential the custody service holds), never an
  app token on the main agent; a user's session (or an app the user approved) for its own
  managed agent.
- `identity.storeUrl` reaches the agent language only through `write_agent_language_settings`
  (atomic: temp file + rename). When it fails, `LanguageController::install_agent_language`
  does not load the agent language, so no profile or identity call goes to an older or the
  default store; identity calls answer `unavailable`.
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
    `coasys:link` signature over `{profile, version, anchor}`, `profile` the expression without
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
  the read refreshes that one log from the configured store once (which advances the accepted
  head) and checks again; a store that cannot answer leaves it `valid: false`.
- **A positive verdict is fresh for `resolver::ADMIT_FRESHNESS` (60 s).** When the cached log
  admits the key but last refreshed from the configured store longer ago than that (the time is
  kept in memory only, so after a restart every log is stale), the read refreshes it once and
  checks again, so the cached log follows the store within the window. The anchor fixes a
  profile's verdict: a revocation the refresh brings in does not touch a profile anchored
  before it. Offline
  policy: when that refresh fails (the store unreachable, or a fork) the cached trusted head
  answers, and the failed attempt counts as the window's read, so an unreachable store is tried
  once per window per log, not on every read.
- An agent keeps the first identity it joined (`identity_agents`); `agent.status` and
  `agent.get` carry it as `identity`, `did` stays the device `did:key` (and
  `agent.status().didDocument` the device's did:key document; `identity.resolve` serves the
  person's).
- **An agent that speaks for a person publishes the person's profile** under the `did:scid`
  (`person.rs`, `identityPutAgent`), on the first binding (`record_agent_if_listed`: a claim
  spent, or `publish` of a log listing the key; main agent or managed user) and on every
  profile publish after the did:key one (`AgentService::publish_profile`). It speaks for the
  person while `identity_agents` binds it and its device key is listed, valid and without the
  bot mark at the trusted seq. The record: the agent's profile with `did` the `did:scid`, as an
  expression authored by the `did:scid` and signed by the device key (`proof.key` the bare
  `did:key`), `anchor` the trusted seq, `version` the time in ms; the signed request: the same
  key at the accepted head, for the store that signed that head. A failure is logged, never
  fails the did:key publish, and the next profile publish tries again.

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
  between this answer's `src` and the cache), `coasys:skipped` (sources not used, with why).
- **Sources.** The cache answers unless `fresh`, or it lacks the log or the asked
  `versionId`. Then: the URL's `src`, then the configured store. A `src` naming the configured
  store (same host, port, path; scheme ignored) is the configured store. The configured store
  goes through `KelCache::refresh` (its head counts; a head no key of `identity.storeKeys`
  signed: `fetch_store_unsigned`, events via `ingest`). A foreign `src` goes through
  `fetch_foreign` and `SrcFetch` (`GET https://<src>/v1/logs/<scid>?from=<n>`, from the cached
  head, at most `MAX_PAGES` pages and `MAX_SRC_LOG_BYTES` (1 MiB) in all; head only a paging
  hint, read as `{events, head: {seq}}` since a foreign host's head need not be signed). Its
  events are folded in memory on the cached log for this answer only: nothing of a foreign
  `src` is cached, so `identity.resolve` from any `agent:READ` session cannot fill the shared
  cache. The configured store is read on in any case (into the cache), so a `src` serving a
  prefix cannot hide an event the store holds (a revocation): the answer is the `src`'s events
  on the cache's log, `coasys:source` the `src`. When the two differ at a seq both hold, the
  cache's log answers with the conflict as `coasys:fork`, and no fork is recorded. The cache
  only extends; a fork with the configured store is recorded and the held log answers. A
  failed foreign fetch is remembered per (host, SCID) for 5 minutes (`MISS_TTL`), `fresh` or
  not.
- **`did:key`.** The store's key index (`IdentityBackend::key_owner`, the language's
  `identityKeyOwner`, `GET /v1/keys/{multikey}`) is a hint: the named log (cache, then store;
  re-read once from the store when the cached log does not list the key up to its trusted seq)
  must list the key as a device key added at or below the trusted seq. Answer: the person's
  document at the head with `coasys:keyId` and `coasys:keyValid` (`key_valid_at` the trusted
  seq; a revoked key resolves, `false`). Unknown key: `notFound`. A log that does not list it:
  `notFound` with `coasys:keyIndexMismatch: true` and a warning in the log. A log that lists it
  only past its trusted seq (or has no store-signed head, e.g. only a foreign `src` served it):
  `notFound`, no mismatch flag. A confirmed key → SCID sits in a bounded in-memory map.
- **`src` and SSRF.** A `src` comes from untrusted input and executors run on servers. Public
  means `resolver::is_public`: no loopback, RFC 1918, fc00::/7, link-local, CGNAT 100.64/10,
  multicast, unspecified, 0/8, 240/4, 192.0.0/24, and IPv4-mapped/compatible/NAT64/6to4 forms
  of those. Up front, without a connection (`coasys:skipped` says why): HTTPS only (another
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

## Do not

- Do not read the words or a master key here, or add a call that carries one: the executor
  holds device keys only.
- Do not trust a store's hash or head: `import`/`fold` and `verify_head` decide.
- Do not pass a `src` to the backend (the agent language reads only its store), and do not
  read a foreign host with another client than `SrcFetch`.
