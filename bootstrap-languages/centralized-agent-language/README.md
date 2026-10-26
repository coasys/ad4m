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
  the log's head and the log lists `signedBy`, without the bot mark and valid at the anchor (a
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
  `identityCancelOffer`, `identityClaimOffer`, `identityOfferState`: one store route each.
  The language signs nothing; a `SignedRequest` travels in `X-Coasys-Request`; every body goes
  out as the argument's canonical JSON (so a signer hashes `JCS(body)`); HTTP statuses map one
  to one onto `IdentityErrorCode`, and a network failure is `unavailable`.
- No token interface: the executor answers `disabled`.

The store's base URL is the `storeUrl` language setting (`{"storeUrl": "https://…"}`), read on
every call; without it, `https://store.coasys.org`.

Every call goes to that store. A did:scid `src` hint names another host of a log; the executor
fetches it itself, never through the language. A log page over 256 events or 257 × 64 KiB is
`invalid` (`too-large`).

The language is `rust-impl/` (the Rust ALDK port), which `pnpm build` bundles into
`build/bundle.js`. The older TypeScript `index.ts` is not built and does not serve the
identity interface.

```bash
pnpm build      # icons, expression UI, the Rust port to wasm
pnpm test       # rust-impl/test.sh, against a real identity-store
```

The suites build `identity-store` and its `test_agent` example with cargo in the repo root and
start the store on a temporary SQLite file.
