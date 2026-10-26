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
  key revoked at seq N signs nothing valid anchored at or after N; a record signed before the
  revocation stays valid). A `did:key` read answers the stored signed expression unchanged, so the
  executor's signature check on every expression passes, and only when its author is the
  record's did and its data an Agent naming that did. A `did:scid` record's profile is a
  plain Agent, answered as an expression built from the record; the executor cannot verify
  that one yet (it resolves an expression's author as a `did:key`).
- `identityLog`, `identityHead`, `identityAppend`, `identityKeyOwner`, `identityPowParams`,
  `identityAttachNote`, `identityPutAgent`, `identityOpenOffer`, `identityListOffers`,
  `identityCancelOffer`, `identityClaimOffer`, `identityOfferState`: one store route each.
  The language signs nothing; a `SignedRequest` travels in `X-Coasys-Request`; every body goes
  out as the argument's canonical JSON (so a signer hashes `JCS(body)`); HTTP statuses map one
  to one onto `IdentityErrorCode`, and a network failure is `unavailable`.
- No token interface: the executor answers `disabled`.

The store's base URL is the `storeUrl` language setting (`{"storeUrl": "https://…"}`), read on
every call; without it, `https://store.coasys.org`.

`identityLog(did, from, src?)` alone may read elsewhere: given a did:scid `src` hint that is an
HTTPS base (`store.example`, `https://store.example`, `https://store.example/prefix`; a
scheme-less one is HTTPS), it fetches `GET {src}/v1/logs/{scid}?from=` from that host. Any
other `src` (`http`, userinfo, a query or fragment, a host that is not a DNS name or IP
literal, an AD4M language address, a DID method) is refused as `invalid` (`src`) before any
fetch. The language only reads from a `src` and passes its head on as data; the executor
decides whether to trust it. Refusing hosts on private or loopback addresses is the
executor's job (it resolves DNS and runs the fetch); the language checks only the form. A
page over 256 events or 257 × 64 KiB is `invalid` (`too-large`), from a `src` or the store.

The language is `rust-impl/` (the Rust ALDK port), which `pnpm build` bundles into
`build/bundle.js`. The older TypeScript `index.ts` is not built and does not serve the
identity interface.

```bash
pnpm build      # icons, expression UI, the Rust port to wasm
pnpm test       # rust-impl/test.sh, against a real identity-store
```

The suites build `identity-store` and its `test_agent` example with cargo in the repo root and
start the store on a temporary SQLite file.
