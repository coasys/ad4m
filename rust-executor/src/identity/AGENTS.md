# identity/ — agent guide

The executor's side of `did:scid:coasys:1`: which
identity each local agent's device key speaks for, the key logs it checked, and the
`identity.*` calls (registered with their guards in `api/identity_ws.rs`).

| File | What |
|---|---|
| `mod.rs` | `init` (at start, after the database), `service()`, `IdentitySettings` (`identity.storeUrl`, `identity.storeKeys`, `identity.allowPrivateSrc`), `write_agent_language_settings`, the `identity-fork` publisher; re-exports `verify_expression` |
| `resolver.rs` | The one place that decides what a log says. `key_speaks_for` and `verify_expression` (an expression by a `did:scid` author, for `LanguageController::verify_expression_proof`); `Resolver::resolve` (`identity.resolve`): DID URL parsing, sources, the `src` address check, the negative cache, `did:key` → person, the DID Core result. Later checks (link anchors, verdicts, key login) belong here |
| `backend.rs` | `IdentityBackend` (`log(did, from, src)`, `key_owner`, …), `IdentityError`; `AgentLanguageBackend` calls the agent language through `agent_language_for` (a JS envelope reads refusals back as `IdentityErrorCode`) |
| `cache.rs` | `KelCache`: events in `SqliteChainStore` (`coasys:kel`) in `ad4m_db.sqlite`, the accepted head in `identity_heads`, `refresh`, `ingest`, `accept`, forks |
| `head.rs` | `verify_head` (`ad4m_identity_core::HeadStatement` under a key of `identity.storeKeys`) |
| `device.rs` | the agent's device key, its X25519 `enc` key (`did_key`, no second secret), `WalletSigner` |
| `operator.rs` | `admits_first_link`, `first_link_check`, `main_incept_check`, `record_operator_at_init` (`ad4m-executor init --operator`) |
| `claim.rs` | `ClaimService::claim_for_main` (`already-linked`, first-link rule, claim with the main wallet key, never `canKeepMaster`), `join_status` (the main agent's claims only) |
| `service.rs` | `IdentityService`: status, log, cosignIncept, publish, openOffer, offers, devices; error mapping |
| `types.rs` | the RPC contracts (ts-rs) mirroring `ad4m_ldk::identity`'s JSON, `IdentityForkEvent`, `AgentWithIdentity` |

Rows in `db/identity.rs`: `operator_record`, `identity_agents` (agent `did:key` → SCID),
`identity_joins` (claims to follow).

## Invariants

- Logs arrive only through the agent language, so a deployment can swap the store behind it;
  nothing here speaks HTTP (the resolver only looks up a `src` host's addresses).
- The cache only extends a log. A different event at a held seq, or a signed head below the
  cached one (a shorter log never truncates a held one), records a fork, appends nothing and fires
  `identity-fork` once per fork, to the sessions of the identity's local agents.
- A head from the agent language counts only under `identity.storeKeys`; `ingest` takes events
  from any source with no head.
- Every claim for the main agent goes through `ClaimService::claim_for_main`, which runs the
  first-link rule itself: `already-linked` while the main key is valid in a log, `no-operator`
  without a record, `first-link` for another identity.
- With an operator record the main key may speak only for the operator's identity:
  `cosignIncept` for the main agent runs `main_incept_check` and refuses (`first-link`) an
  incept of any other identity. Managed users are not bound by the record.
- `identity.storeUrl` reaches the agent language only through `write_agent_language_settings`
  (atomic: temp file + rename). When it fails, `LanguageController::install_agent_language`
  does not load the agent language, so no profile or identity call goes to an older or the
  default store; identity calls answer `unavailable`.
- An expression by a `did:scid` author verifies only under its `proof.key` (a bare `did:key`)
  when the cached log lists that key valid at the cached head by the clock now, without the
  bot mark; the expression's own timestamp does not count. When the cached log does not admit
  the key (no log yet, or a device added since), the read refreshes that one log from the store
  once and checks again; a store that cannot answer leaves it `valid: false`.
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
  cache's accepted head, signed under `identity.storeKeys`, is the log's head), `coasys:fork`
  (the first recorded fork), `coasys:skipped` (sources not used, with why).
- **Sources.** The cache answers unless `fresh`, or it lacks the log or the asked
  `versionId`. Then: the URL's `src`, then the configured store. A `src` naming the configured
  store (same host, port, path; scheme ignored) is the configured store. The configured store
  goes through `KelCache::refresh` (its head counts); a foreign `src` through
  `fetch_untrusted` (events via `ingest`, head only a paging hint). Either way the cache only
  extends; a fork is recorded and the held log answers. A failed foreign fetch is remembered
  per (host, SCID) for 5 minutes (`MISS_TTL`), `fresh` or not.
- **`did:key`.** The store's key index (`IdentityBackend::key_owner`, the language's
  `identityKeyOwner`, `GET /v1/keys/{multikey}`) is a hint: the named log (cache, then store;
  re-read once from the store when the cached log does not list the key) must list the key as a
  device key. Answer: the person's document at the head with `coasys:keyId` and
  `coasys:keyValid` (`key_valid_at` the head; a revoked key resolves, `false`). Unknown key:
  `notFound`. A log that does not list it: `notFound` with `coasys:keyIndexMismatch: true` and
  a warning in the log. A confirmed key → SCID sits in a bounded in-memory map.
- **`src` and SSRF.** A `src` comes from untrusted input and executors run on servers. Before a
  foreign `src` reaches the agent language: HTTPS only (another scheme is skipped; the language
  refuses non-HTTPS too), no user info, query or fragment; an IP literal must be public, and a
  name is looked up here and every address must be public (`resolver::is_public`: no loopback,
  RFC 1918, fc00::/7, link-local, CGNAT 100.64/10, multicast, unspecified, 0/8, 240/4,
  192.0.0/24, and IPv4-mapped/compatible/NAT64/6to4 forms of those; `localhost` names refused
  without a lookup). `identity.allowPrivateSrc` (`--identity-allow-private-src`,
  `AD4M_IDENTITY_ALLOW_PRIVATE_SRC`; off by default, warns at start) skips the address check
  for tests and local development.
- **Residual window.** The check is a pre-check. The language fetches with Deno's `fetch`
  (`httpFetch` in `js_core/host.js`), which resolves the name again and follows redirects; no
  hook there takes this call's policy. So a name that answers a public address to the check and
  a private one to the fetch (DNS rebinding), or a public host that redirects to a private
  address, is not stopped here. Closing it needs the check at the fetch point: a connector
  that refuses non-public peer addresses (and redirects to them) for identity reads.

## Do not

- Do not read the words or a master key here, or add a call that carries one: the executor
  holds device keys only.
- Do not trust a store's hash or head: `import`/`fold` and `verify_head` decide.
- Do not pass a `src` to the backend without `Resolver`'s check.
