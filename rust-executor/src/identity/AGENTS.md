# identity/ — agent guide

The executor's side of `did:scid:coasys:1`: which
identity each local agent's device key speaks for, the key logs it checked, and the
`identity.*` calls (registered with their guards in `api/identity_ws.rs`).

| File | What |
|---|---|
| `mod.rs` | `init` (at start, after the database), `service()`, `IdentitySettings` (`identity.storeUrl`, `identity.storeKeys`, `identity.allowPrivateSrc`), `write_agent_language_settings`, the `identity-fork` publisher; re-exports `verify_expression` |
| `resolver.rs` | The one place that decides what a log says. `key_speaks_for`, `key_valid_at_anchor` and `verify_expression` (all up to the log's trusted seq, `KelCache::trusted`) (an expression by a `did:scid` author, for `LanguageController::verify_expression_proof`; `proof.record` anchors the key); `Resolver::resolve` (`identity.resolve`): DID URL parsing, sources, the up-front `src` check, the negative cache, `did:key` → person, the DID Core result. Later checks (link anchors, verdicts, key login) belong here |
| `src_fetch.rs` | `SrcFetch`: the executor's reader of a foreign `src` over the did:scid host profile (`GET /v1/logs/{scid}?from=`), for `Resolver` only. `PublicOnly` (the client's DNS resolver: public addresses only), `refuse_private_host` (IP literals, `localhost`), no redirects, no proxy, HTTPS only, timeouts, body cap |
| `backend.rs` | `IdentityBackend` (`log(did, from)`, `key_owner`, …), `IdentityError`; `AgentLanguageBackend` calls the agent language through `agent_language_for` (a JS envelope reads refusals back as `IdentityErrorCode`) |
| `cache.rs` | `KelCache`: events in `SqliteChainStore` (`coasys:kel`) in `ad4m_db.sqlite`, the accepted head in `identity_heads`, `refresh` (at most `MAX_PAGES` pages), `ingest`, `accept`, `trusted` (the log and its accepted head's seq), forks, `reset` (`identity.resetLog`), `refreshed_within` (the in-memory time of each log's last refresh) |
| `head.rs` | `verify_head` (`ad4m_identity_core::HeadStatement` under a key of `identity.storeKeys`) |
| `device.rs` | the agent's device key, its X25519 `enc` key (`did_key`, no second secret), `WalletSigner` |
| `operator.rs` | `admits_first_link`, `first_link_check`, `main_incept_check`, `record_operator_at_init` (`ad4m-executor init --operator`) |
| `custody.rs` | The custody channel's guard `admit` (the admin credential on a loopback listener of an executor started with `--custody-channel`; else 403 `no-custody-channel`) and `main_device_keys`; `identity.custodyStatus` is its one call so far |
| `rekey.rs` | `identity.rekey` (`.local_only()`): a new main wallet key, nothing in a log; the table of every place that holds the main agent's DID and how each moves |
| `claim.rs` | `ClaimService::claim_for_main` (`already-linked`, first-link rule, claim with the main wallet key, never `canKeepMaster`), `join_status` (the main agent's claims only) |
| `service.rs` | `IdentityService`: status, log, resetLog, cosignIncept, publish, openOffer, offers, devices; error mapping |
| `types.rs` | the RPC contracts (ts-rs) mirroring `ad4m_ldk::identity`'s JSON, `IdentityForkEvent`, `AgentWithIdentity` |

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
  count yet. The readers: `key_speaks_for` and `key_valid_at_anchor` (so `verify_expression`,
  with and without `proof.record`), the `did:key` confirmation in `resolve_key`,
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
- An expression by a `did:scid` author verifies only under its `proof.key` (a bare `did:key`)
  and only when its own signature holds under that key. Which seq of the log counts:
  - With `proof.record` (`{version, anchor, sig}`, which the agent language adds to a
    `did:scid` profile from its agent record): the record's anchor. `sig` must be the key's
    `coasys:link` signature over `{profile, version, anchor}`, `profile` the expression without
    `proof.record` (and without `proof.valid`/`proof.invalid`), as `AgentRecord::signed_value`
    builds it; the anchor must be at or below the trusted seq (not merely the cached head);
    the log must list the key as a device key, valid at the anchor by the clock now
    (`key_valid_at_time`) and without the bot mark there. A profile signed before a revocation
    stays valid after it. A `proof.record` that does not hold (malformed, an unknown member, a
    bad signature, an anchor past the trusted seq, a key not valid there) makes the expression
    invalid: the head rule is never tried instead. The anchor counts only through the record
    signature.
  - Without it: the trusted seq, by the clock now, without the bot mark.
  - `proof.record` counts only on an expression the agent language served
    (`LanguageController::honours_record`): its store refuses a record anchored before a
    revocation of its key. From any other language, a literal or the language language, the
    record is ignored and the head rule judges.

  The expression's own timestamp never counts. When the cached log does not admit the key (no
  log or no store-signed head yet, a trusted seq below the anchor, or a device added since),
  the read refreshes that one log from the configured store once (which advances the accepted
  head) and checks again; a store that cannot answer leaves it `valid: false`.
- **A positive verdict is fresh for `resolver::ADMIT_FRESHNESS` (60 s).** When the cached log
  admits the key but last refreshed from the configured store longer ago than that (the time is
  kept in memory only, so after a restart every log is stale), the read refreshes it once and
  checks again, so a revocation at the store reaches every reader within the window. Offline
  policy: when that refresh fails (the store unreachable, or a fork) the cached trusted head
  answers, and the failed attempt counts as the window's read, so an unreachable store is tried
  once per window per log, not on every read.
- An agent keeps the first identity it joined (`identity_agents`); `agent.status` and
  `agent.get` carry it as `identity`, `did` stays the device `did:key` (and
  `agent.status().didDocument` the device's did:key document; `identity.resolve` serves the
  person's).

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
