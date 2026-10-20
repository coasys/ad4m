# Link verdicts

The executor gives every stored link one of four **link verdicts**: `Valid`, `Invalid(reason)`, `CantCheck` or `Flagged`. It computes the verdict when the link enters the store, through one **resolver** over its cache of verified key logs (`KelCache`), and stores it with the link. **Logic reads** (values, counts, votes, roles, ordering) use only `Valid` links. **Display reads** hide only `Invalid` links and carry each link's verdict. The same resolver answers `author_of`: which person a device key belongs to.

**Why:** One yes-or-no verdict, as before identities, would mark honest links as forged while the shared store is down. Four verdicts keep "forged" apart from "not checkable yet", and fail closed only where the answer decides something. `Flagged` covers a link with a valid signature and an old anchor that reached this executor only after its key was revoked, so the reader cannot tell when it was really signed. The resolver is the only path to the key logs, so a link check and a session check never disagree about one key at one position.

**Parent:** [Links and store](README.md)

<details><summary>How it works</summary>

| Verdict | When | Logic reads | Display reads |
|---|---|---|---|
| `Valid` | every check passes | count | shown |
| `Invalid(reason)` | bad signature; key never valid; revoked at or before the anchor; past its expiry; head hash disagrees (fork) | excluded | hidden |
| `CantCheck` | the agent language gives no answer and the cache does not cover the anchor | excluded until a re-check | shown with a note |
| `Flagged` | the signature holds, but this executor first saw the link more than 10 minutes after the store appended the key's revocation | excluded | shown with "signed before removal, first seen after" |

1. `signatures::verify` dispatches. No anchor: the `did:key` check as before identities (`Valid` or `Invalid`). Anchor: resolve the log (cache, then the agent language), check that the log lists the key at `key_id`, that the key is valid at `kel_seq` by the executor's clock (this covers a bot's scope and a key with an expiry), that the log's hash at `kel_seq` equals `kel_hash`, then the signature, then the first-seen rule.
2. A check whose anchor the cache covers makes no network call. One fetch per identity runs at a time; each call times out after 5 s behind a circuit breaker, so an outage gives `CantCheck` fast.
3. A re-check queue runs `CantCheck` links again when the agent language answers, and replaces the stored verdict.
4. `author_of(did_key)` answers `Person { did, key_id, revoked }`, `Bot { owner, … }`, `NoLog` (a device-only key) or `Unavailable`. Every read that counts or filters by person treats `Unavailable` as no person.
5. `KeyResolver::subscribe` sends a `VerdictChange` when a key's state changes. Sessions close on it ([Sessions](../../agents/access/sessions.md)); neighbourhoods record the reach of a revocation ([Counts by person](../neighbourhoods/membership/counts-by-person.md)).
6. Migration: a stored `true` becomes `Valid`, `false` becomes `Invalid(Legacy)`. Old links carry no anchor, so their verdict never changes.

The key log itself is on [Identity](../../agents/identity/README.md).

</details>

<details><summary>Where in the code</summary>

- [identity/verdict.rs](../../../rust-executor/src/identity/verdict.rs) (new): `LinkVerdict`, `KeyVerdict`, the first-seen rule.
- [identity/resolver.rs](../../../rust-executor/src/identity/resolver.rs) (new): `author_of`, `keys_of`, `check_anchor`, `KeyResolver`.
- [identity/cache.rs](../../../rust-executor/src/identity/cache.rs) (new): `KelCache` (`refresh`, `ingest`), on the hash-chain module's SQLite store.
- [agent/signatures.rs](../../../rust-executor/src/agent/signatures.rs): `verify`, the dispatch point.
- [perspectives/sparql_store.rs](../../../rust-executor/src/perspectives/sparql_store.rs): the verdict annotation on the reifier ([Oxigraph store](oxigraph-store.md)).

</details>
