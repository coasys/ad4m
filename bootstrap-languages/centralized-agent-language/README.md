# centralized-agent-language

The closed beta's AD4M agent language (the identity interface of
`docs-src/language-interface-spec.md`, section 5.8). It keeps agent profiles and
serves the identity interface, both on the shared identity store (`identity-store/`):

- `expressionCreate` / `expressionGet`: agent records at `PUT`/`GET /v1/agents/{did}` (no
  call to mock.ad4m.dev any more). For a `did:key` agent the host signs the Agent as an AD4M
  expression (`createSignedExpression`), that signed expression is the record's `profile`, and
  the agent's key signs the record through the host; a read keeps a record only when its
  signature holds (for a `did:scid`, by an unrevoked, non-bot key of that log at an anchor the
  log holds). A `did:key` read answers the stored signed expression unchanged, so the
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

The language is `rust-impl/` (the Rust ALDK port), which `pnpm build` bundles into
`build/bundle.js`. The older TypeScript `index.ts` is not built and does not serve the
identity interface.

```bash
pnpm build      # icons, expression UI, the Rust port to wasm
pnpm test       # rust-impl/test.sh, against a real identity-store
```

The suites build `identity-store` and its `test_agent` example with cargo in the repo root and
start the store on a temporary SQLite file.
