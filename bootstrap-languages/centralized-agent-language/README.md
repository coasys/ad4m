# centralized-agent-language

The closed beta's AD4M agent language (the identity interface of
`docs-src/language-interface-spec.md`, section 5.8). It keeps agent profiles and
serves the identity interface, both on the shared identity store (`identity-store/`):

- `expressionCreate` / `expressionGet`: agent records at `PUT`/`GET /v1/agents/{did}` (no
  call to mock.ad4m.dev any more). For a `did:key` agent the host signs the Agent as an AD4M
  expression (`createSignedExpression`), that signed expression is the record's `profile`, and
  the agent's key signs the record through the host; a read keeps a record only when its
  signature holds. For a `did:scid` the record's `anchor` is one number, the seq of that log
  the signer knew when it signed; the read keeps the record only when the anchor is at or below
  the log's head and the log lists `signedBy` as a device key (never a master), without the
  bot mark and valid at the anchor (a
  key revoked at seq N signs nothing valid anchored at or after N). The signer picks the
  anchor, so the store orders arrivals: it takes a `did:scid` record only when `signedBy` is
  also valid at the log's head when the record arrives, so a revoked key cannot backdate a new
  record to an anchor before its revocation, and a record the store took before the revocation
  stays valid. The read relies on that admission and checks at the anchor. A record's profile is the agent's signed AD4M expression, answered
  only when its author is the record's did and its data an Agent naming that did. A `did:key`
  read answers it unchanged, so the executor's signature check on every expression passes. A
  `did:scid` read answers it exactly as stored plus `proof.record` = `{version, anchor, sig}`
  of the record, so the executor checks the record itself at its anchor: removing
  `proof.record` gives back the stored profile, which `sig` covers, by `proof.key` (the
  record's `signedBy`). A `did:scid` profile with `proof.valid`, `proof.invalid` or
  `proof.record`, another `proof.key`, or a field the expression type does not carry is
  ignored.
- `identityLog`, `identityHead`, `identityAppend`, `identityKeyOwner`, `identityPowParams`,
  `identityAttachNote`, `identityPutAgent`, `identityOpenOffer`, `identityListOffers`,
  `identityCancelOffer`, `identityClaimOffer`, `identityOfferState`, `identityInboxPut`,
  `identityInboxList`, `identityInboxAck`, `identityGetRecord`, `identityPutRecord`,
  `identityPostRequest`, `identityListRequests`, `identityDeleteRequest`: one store route each.
  Inbox messages and private records are sealed; the language passes the ciphertext through
  and opens nothing. A pre-join inbox read's `PreInboxAuth` travels in `X-Coasys-Inbox`.
  The language signs nothing; a `SignedRequest` (whose `req.store` names the store it is
  for; another store refuses it as `forbidden` `store`) travels in `X-Coasys-Request`; every body goes
  out as the argument's canonical JSON (so a signer hashes `JCS(body)`); HTTP statuses map one
  to one onto `IdentityErrorCode`, and a network failure is `unavailable`.
- No token interface: the executor answers `disabled`.

The store's base URL is the `storeUrl` language setting (`{"storeUrl": "https://…"}`), read on
every call; without it, `https://store.coasys.org`. It must be `https`, or plain `http` to a
loopback address (a store on the same machine); any other URL is `invalid` (`store-url`) and
nothing is sent.

The `storeKeys` setting (`{"storeKeys": ["z6Mk…"]}`, the executor's `identity.storeKeys`)
lists the store keys the language trusts. To check a `did:scid` record it reads the person's
log and keeps it only when the last page's head is signed (`coasys:head`) by one of those keys,
names that log, and is exactly the head the replayed events end at; without `storeKeys` no
`did:scid` record verifies. A signed head carries no freshness: the store can still serve an
older head with its prefix. A log read stops at 64 pages and two pages' worth of bytes
(`invalid` `too-large`), whatever head the store names. Each event is parsed from the page's
text, so an event that names a member twice (I-JSON, RFC 7493) gives no log.

Record signatures use their own domain, `coasys:agent-record`, and a record's `version` is
unix milliseconds: the store refuses one more than 5 minutes past its clock, so no write can
push the version out of the owner's reach.

Every call goes to that store. A did:scid `src` hint names another host of a log; the executor
fetches it itself, never through the language. A reply over 257 × 64 KiB (a full log page),
on any route and with any status, and a log page over 256 events are `invalid` (`too-large`).

The language is `rust-impl/` (the Rust ALDK port), which `pnpm build` bundles into
`build/bundle.js`. The older TypeScript `index.ts` is not built and does not serve the
identity interface.

```bash
pnpm build      # icons, expression UI, the Rust port to wasm
pnpm test       # rust-impl/test.sh, against a real identity-store
```

The suites build `identity-store` and its `test_agent` example with cargo in the repo root and
start the store on a temporary SQLite file.
