# Appendix B — Worked Example (Informative)

This appendix walks an end-to-end scenario across §§ 2–8 so a reader can see how the wire pieces compose. Two agents (Alice and Bob) start cold, join the same Neighbourhood, post a typed message, and query it.

This is illustrative rather than normative — every step is implemented by RPCs documented in [Appendix A](./appendix-a-rpc-reference.md) and behaviours specified in §§ 2–8.

## C.1 Setup

Both Alice and Bob have just installed a conforming executor. Each runs locally on `ws://localhost:12000`.

```
Alice's executor                          Bob's executor
   wallet locked                              wallet locked
   admin credential off                       admin credential off
   RPC server running                         RPC server running
```

## C.2 Alice generates an identity

Alice opens a setup UI that holds the admin credential (so it can talk to the executor before any app token exists).

```
→ ws://localhost:12000/api/v1/ws?token=<admin-credential>

→ { "id": "1", "type": "agent.generate",
    "params": { "passphrase": "correct-horse-battery-staple" } }

← { "id": "1",
    "result": { "did": "did:key:z6MkAlice...",
                "didDocument": "{...}",
                "perspective": null } }
```

The executor:

- Generated an Ed25519 keypair.
- Persisted it encrypted under Argon2id+XSalsa20-Poly1305 (§3.2, §10.6).
- Unlocked the wallet in-memory for the current session.

`runtime.info` now returns:

```json
{ "ad4mExecutorVersion": "0.13.0-test-2",
  "isInitialized": true,
  "isUnlocked": true }
```

## C.3 Alice grants an app a capability token

Alice runs a chat app that wants to read/write the `perspective` and `agent` domains. The app calls the consent flow (§4.8):

```
→ { "id": "2", "type": "agent.requestCapability",
    "params": { "authInfo": {
      "appName": "Chat",
      "appDesc": "Cross-Neighbourhood chat",
      "capabilities": [
        { "with": { "domain": "perspective", "pointers": ["*"] },
          "can":  ["READ", "CREATE", "UPDATE", "SUBSCRIBE"] },
        { "with": { "domain": "agent", "pointers": ["*"] },
          "can":  ["READ"] }
      ]
    } } }

← { "id": "2", "result": { "requestId": "abc-123" } }
```

Alice approves in the executor's UI (`agent.permitCapability(abc-123)`). The app then collects the token:

```
→ { "id": "3", "type": "agent.generateJwt",
    "params": { "requestId": "abc-123", "rand": "...", ... } }

← { "id": "3", "result": "<JWT string>" }
```

The JWT carries (decoded for illustration):

```json
{
  "iss": "did:key:z6MkAlice...",
  "sub": null,
  "aud": "Chat",
  "exp": 1779379200,
  "iat": 1779292800,
  "nonce": "550e8400-e29b-41d4-a716-446655440000",
  "capabilities": {
    "appName": "Chat",
    "appDesc": "Cross-Neighbourhood chat",
    "capabilities": [
      { "with": { "domain": "perspective", "pointers": ["*"] },
        "can":  ["READ", "CREATE", "UPDATE", "SUBSCRIBE"] },
      { "with": { "domain": "agent", "pointers": ["*"] },
        "can":  ["READ"] }
    ]
  }
}
```

The Chat app now opens its own WebSocket — `ws://localhost:12000/api/v1/ws?token=<JWT>` — and does everything else under this token. The admin-credential connection is no longer needed.

## C.4 Alice publishes a Neighbourhood

The Chat app picks one of the executor's `knownLinkLanguages` to use as the Link Language for the new Neighbourhood. It first templates a per-Neighbourhood instance:

```
→ { "id": "4", "type": "language.applyTemplate",
    "params": { "sourceAddress": "Qm...link-language-template-address",
                "templateData": "{\"name\":\"AliceBobChat\"}" } }

← { "id": "4", "result": { "address": "Qm...AliceBobChat-LL" } }
```

The executor fetched the template, ran its parameter substitution, then went through the §8.5 install check (signature + trustedAgents) on the new bundle before exposing it.

Now create a Perspective backed by this Link Language and publish it as a Neighbourhood:

```
→ { "id": "5", "type": "perspective.create",
    "params": { "name": "AliceBobChat" } }

← { "id": "5", "result": { "uuid": "p-1", "name": "AliceBobChat",
                            "state": "Private" } }

→ { "id": "6", "type": "neighbourhood.publish",
    "params": { "perspectiveUuid": "p-1",
                "linkLanguage": "Qm...AliceBobChat-LL",
                "meta": { "links": [] } } }

← { "id": "6", "result": "neighbourhood://Qm...nh-address" } }
```

Alice sends Bob the URL `neighbourhood://Qm...nh-address` out-of-band.

## C.5 Bob joins

Bob's executor is already initialized. The Chat app on Bob's side has its own token (same consent flow). Bob calls:

```
→ { "id": "7", "type": "neighbourhood.join",
    "params": { "url": "neighbourhood://Qm...nh-address" } }
```

Bob's executor:

1. Resolved the `NeighbourhoodExpression` via the Neighbourhood Language (§8.2.3).
2. Verified its signature (§3.4) — author is Alice.
3. Installed the Link Language at `Qm...AliceBobChat-LL` — applying the §8.5 trust check (the template chain's original author must be in Bob's `trustedAgents`).
4. Created a new local PerspectiveHandle backed by the Link Language.
5. Began syncing.

```
← { "id": "7", "result": { "uuid": "p-bob-1",
                            "state": "LinkLanguageInstalledButNotSynced" } }
```

After sync completes, Bob receives an event:

```
{ "type": "sync-state-change",
  "data": { "perspectiveUuid": "p-bob-1", "state": "Synced" } }
```

## C.6 Alice registers a Subject Class

Both apps need to agree on what a "Message" is. Alice writes a SHACL shape (in the JSON form of §5.1):

```json
{
  "node_shape_uri": "shacl://Message",
  "target_class": "chat://Message",
  "properties": [
    { "path": "chat://body",      "name": "body",
      "datatype": "xsd:string",   "min_count": 1, "max_count": 1 },
    { "path": "chat://timestamp", "name": "timestamp",
      "datatype": "xsd:dateTime", "min_count": 1, "max_count": 1 }
  ],
  "constructor_actions": [
    { "action": "addLink", "source": "this",
      "predicate": "rdf://type", "target": "chat://Message" }
  ]
}
```

Alice's app sends this once:

```
→ { "id": "8", "type": "perspective.addSubjectClass",
    "params": { "uuid": "p-1", "name": "Message",
                "shaclJson": "<the JSON above, stringified>" } }
```

The executor encodes the shape as the link sub-graph in §5.2 — `ad4m://has_subject_class → literal:string:Message`, plus the `sh://NodeShape`, `sh://property`, etc. links. Because `p-1` is the local handle for the shared `AliceBobChat` Neighbourhood, these SHACL links get sync'd to Bob's executor by the Link Language. Bob does not need to register the same Subject Class — it appears on his side automatically.

## C.7 Alice posts a message

```
→ { "id": "9", "type": "perspective.createSubject",
    "params": { "uuid": "p-1", "className": "Message",
                "baseExpression": "chat://msg-001" } }

→ { "id": "10", "type": "perspective.addLink",
    "params": { "uuid": "p-1",
                "link": { "source":    "chat://msg-001",
                          "predicate": "chat://body",
                          "target":    "literal:string:Hello%20Bob" } } }

→ { "id": "11", "type": "perspective.addLink",
    "params": { "uuid": "p-1",
                "link": { "source":    "chat://msg-001",
                          "predicate": "chat://timestamp",
                          "target":    "literal:string:2026-05-20T10%3A30%3A00.000Z" } } }
```

For each `addLink`, the executor:

- Constructed a `Link`, wrapped it as a `LinkExpression`, signed it (§3.3) using Alice's wallet — `author` = Alice's DID, `proof.key` = `did:key:z6MkAlice...#z6MkAlice...`.
- Projected it into the SPARQL store as 8 quads (§2.10).
- Sent it through the Link Language's `perspectiveCommit` (§6.5) so Bob's executor will pull it.

## C.8 Bob queries the messages

Bob's executor has received the synced links. Bob's app runs a SPARQL query:

```
→ { "id": "12", "type": "perspective.querySparql",
    "params": { "uuid": "p-bob-1",
                "query": "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                          SELECT ?msg ?body ?ts ?author WHERE {
                            ?msg rdf:type <chat://Message> .
                            ?msg <chat://body>      ?bodyRaw .
                            ?msg <chat://timestamp> ?tsRaw .
                            BIND(<ad4m://fn/parse_literal>(?bodyRaw) AS ?body)
                            BIND(<ad4m://fn/parse_literal>(?tsRaw)   AS ?ts)
                            ?r rdf:reifies <<( ?msg <chat://body> ?bodyRaw )>> .
                            ?r <ad4m://ontology/author> ?author .
                          }" } }

← { "id": "12",
    "result": [
      { "msg":    "chat://msg-001",
        "body":   "Hello Bob",
        "ts":     "2026-05-20T10:30:00.000Z",
        "author": "did:key:z6MkAlice..." }
    ] }
```

This query exercises:

- The reifier storage model (§2.10) to pull `?author` out of the per-link metadata.
- The custom `<ad4m://fn/parse_literal>` SPARQL function (§5.4.4) to decode `literal:string:...` URIs into native SPARQL strings.
- Cross-implementation parity: since reifier IRIs are computed identically (§2.10.1), Bob's executor sees the same `?r` IRI Alice's executor saw when it created the link.

## C.9 Bob verifies signatures (optional)

If Bob wants to independently verify Alice signed the message body link:

```
→ { "id": "13", "type": "runtime.verifySignature",
    "params": { "did":              "did:key:z6MkAlice...",
                "didSigningKeyId":  "did:key:z6MkAlice...#z6MkAlice...",
                "data":             "<the SHA-256 hash from §3.3>",
                "signedData":       "<hex signature>" } }

← { "id": "13", "result": true }
```

Or Bob's app can just trust the `proofValid` metadata on the reifier — the executor already ran verification at receive time (§3.4) and exposes the result as `<ad4m://ontology/proofValid>`.

## C.10 What this example does NOT show

- **Telepresence** — Bob and Alice's online-status broadcast, signal handling, presence subscriptions. Requires a Link Language exporting the `telepresence` capability (§6.5).
- **Templating with private parameters** — for templates that bake secrets (e.g. DM inboxes), the consent flow on the recipient side.
- **Multi-key agents** — DID rotation via `proof.key` selection (§3.4.1, §10.5.1).
- **Error paths** — install failure on §8.5 signature mismatch, token revocation mid-session, sync failure when peers are offline.
- **Multi-user mode** — multiple agents on a single executor, sub-account flows.

These are exercised in the cross-implementation test suite (TBD per §11.5) and in the unit tests under [`rust-executor/src/api/tests/`](../rust-executor/src/api/tests/).
