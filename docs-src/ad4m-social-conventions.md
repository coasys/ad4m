# AD4M Social Conventions

**Version:** 0.1-draft
**Date:** 2026-04-10
**Status:** Draft — companion to the Language Interface spec

> This document is a **companion** to [`language-interface-spec.md`](./language-interface-spec.md)
> and [`ad4m-lang.wit`](./ad4m-lang.wit). Those define what Languages are
> and how they talk to a runtime. This document defines **social-layer
> conventions** that sit on top of the Language interface — things like
> how agents discover each other's inboxes, how friendships are
> represented, how direct messages work.
>
> The Language Interface spec is a **technical protocol spec** (WIT +
> prose). This spec is **vocabulary and patterns** — reserved `ad4m://`
> predicates and the idioms that use them. An AD4M implementation
> MAY choose to honor only the Language Interface spec and leave these
> conventions to applications; but applications that want to
> interoperate across AD4M implementations SHOULD use the vocabulary
> defined here.
>
> Where these conventions depend on behaviors from the Language
> Interface spec, we cite `§N` into that document.

---

## 1. Motivation

Earlier drafts of AD4M baked three pieces of social-layer concern
directly into the core:

1. **Agents had a `directMessageLanguage` field** on their shape,
   alongside `did` and `perspective`.
2. **The Language interface had a `direct-message` capability**
   with `recipient` / `sendP2P` / `sendInbox` / `setStatus` / etc.
3. **The runtime carried a local `friends` table** in SQLite,
   used only to gate DM sends.

All three are special cases of things AD4M already has. An "inbox"
is a Language that accepts commits; the `direct-message` capability
is just `perspective-commit` with a recipient DID baked into a
template clone; and a "friends list" is a set of links in a
perspective. Keeping them as first-class concerns in the core meant
that:

- Adding new social-layer concepts (follows, block-lists, contacts
  with metadata) would require more capabilities or more fields.
- The friend-list couldn't be queried with SPARQL alongside the rest
  of a user's graph.
- DM assumptions (one inbox per agent, recipient-DID on the agent
  shape) leaked into the core.
- New implementations (browsers, mobile, alternative runtimes) had
  to re-implement these bolt-ons.

v1.0 removes all three from the core. This document describes the
replacements.

---

## 2. Agent shape

### 2.1 The core shape

An AD4M Agent, at the core, is just:

```
{
    did: string,          // the agent's DID
    perspective: Perspective,  // the agent's public perspective
}
```

That's it. Everything else an agent is "known for" — inbox, profile,
avatar, status, social graph edges — lives as links in the
perspective.

### 2.2 Migration from v0.x

In v0.x the Agent struct also carried `directMessageLanguage: Option<string>`.
v1.0 drops this field. Implementations MAY emit a one-release
compatibility shim that projects the old field onto the new
`ad4m://inbox` predicate (§3.1) on read, but new code SHOULD use the
predicate directly.

Concretely:

**v0.x (deprecated):**
```js
agent.directMessageLanguage  // string | null
```

**v1.0:**
```sparql
SELECT ?inbox WHERE {
  <did:key:z6Mk...> <ad4m://inbox> ?inbox .
}
```

Clients that want to write the inbox reference into the agent's
public perspective do so via a normal `addLink` / `perspectiveCommit`
call:

```js
await perspective.addLink({
    source: myDid,
    predicate: "ad4m://inbox",
    target: inboxLanguageAddress,
});
```

---

## 3. Reserved `ad4m://` predicates

The `ad4m://` URI scheme is reserved for well-known predicates
defined by AD4M social conventions. Implementations MUST NOT assign
`ad4m://` URIs for their own private use. Applications MAY use them
freely as link predicates. Each predicate is a normative statement
about how the link's `source` and `target` are meant to be
interpreted.

### 3.1 `ad4m://inbox`

```
(agent-did) -[ad4m://inbox]-> (language-ref)
```

Meaning: "The `language-ref` target is a Language instance the agent
at `agent-did` uses as their inbox. Other agents who want to send
messages to `agent-did` should do so by committing (via
`perspective-commit`) to that Language instance."

The target MUST be a valid Language reference (a `<lang-hash>://<address>`
URI; see the Language Interface spec on Language addressing).

Agents MAY declare multiple inboxes on their public perspective (for
example, personal vs. work, or per-app). In that case, senders
choose which to use via application logic — the convention does not
prescribe a default. If only one `ad4m://inbox` link exists, that's
unambiguously "the inbox."

### 3.2 `ad4m://friend-of`

```
(agent-did-1) -[ad4m://friend-of]-> (agent-did-2)
```

Meaning: "The agent at `agent-did-1` considers the agent at
`agent-did-2` to be a friend." This is **asymmetric by default** —
the convention does not require reciprocation. An application may
interpret reciprocal `friend-of` links as "mutual friendship" and
one-way links as "follows," "requests pending," or something else.
The spec does not prescribe.

Friendship lives in a perspective. Which perspective is an
implementation choice:

- The agent's **own public perspective** — friends are publicly
  queryable. Good for contact graphs the user doesn't mind sharing.
- A **dedicated private perspective** owned by the agent — friends
  are local-only and not replicated out.
- A **shared perspective between two agents** — when Alice and Bob
  mutually friend each other, the `friend-of` links live in a
  perspective they both have access to.

A runtime MAY expose a well-known "social" perspective where
`ad4m://friend-of` links live by convention, but apps that want
different privacy postures are free to use any perspective.

### 3.3 `ad4m://profile` (recommended)

```
(agent-did) -[ad4m://profile]-> (expression-uri)
```

Meaning: "The `expression-uri` resolves to a profile Expression
describing the agent." The profile Expression's schema is
application-defined (name, bio, avatar URL, pronouns, etc.), but
this predicate provides a uniform discovery point.

### 3.4 `ad4m://presence` (recommended)

```
(agent-did) -[ad4m://presence]-> (language-ref)
```

Meaning: "The `language-ref` is a Language instance exposing the
agent's real-time presence (online/offline, current status, custom
availability flags) via telepresence." Clients wanting to know "is
Alice online right now?" call `telepresence.get-online-agents` on
the referenced Language.

### 3.5 Extensibility

New `ad4m://` predicates are added to this document through normal
spec revisions. Implementations that need application-specific
predicates SHOULD use their own URI scheme (e.g.
`https://example.com/ad4m-vocab/whatever`) rather than squatting on
`ad4m://`.

---

## 4. The DM inbox pattern

### 4.1 Shape

A direct-message inbox is a **Language instance owned by the
recipient**, exporting:

- `perspective-commit` — senders drop messages in.
- `perspective-sync` — the owner replicates across their devices.
- `peers` — supports multi-device membership for the owner.
- `perspective-query` — optional, for searching the inbox.

The recipient's DID is **baked into the source** at template-clone
time, so the Language's internal logic can enforce:

```
if (actingAgent !== OWNER_DID) {
    // Only allow commit; reject sync/render/query.
}
```

This is **enforcement inside the Language**, not a spec-level
capability restriction. Capability detection via export presence
tells the runtime what the Language *can* do; runtime behavior
enforces *effective* permissions per caller.

### 4.2 Authoring (template)

A DM inbox Language is typically shipped as a **template** with a
recipient placeholder:

```js
// In the template source:
//!@ad4m-template-variable
const OWNER_DID = "<to-be-filled-at-clone-time>";

const lang = defineLanguage({
    name: "@coasys/dm-inbox",
    version: "1.0.0",
    isPublic: false,

    async init() { /* ... */ },
    teardown() { /* ... */ },

    commit: {
        async commit(diff) {
            // Anyone can drop a message into the inbox.
            // Messages are encrypted to OWNER_DID by the sender.
            await store(diff);
        },
    },

    sync: {
        async sync() {
            assertActingAgent(OWNER_DID);  // only owner can pull state
            return await pullState();
        },
        async render() {
            assertActingAgent(OWNER_DID);
            return await fullState();
        },
        currentRevision: async () => await latestRev(),
    },

    peers: {
        setLocal(agents) { /* track local device set for owner */ },
        async remote() { return []; },
    },
});

export default lang;
```

When Bob sets up his inbox, the runtime (or an onboarding flow)
instantiates this template with `OWNER_DID = bobDid` via the
existing `applyTemplateAndPublish` mechanism, producing a new
Language hash unique to Bob. Bob then publishes a link to it:

```js
await bobPublicPerspective.addLink({
    source: bobDid,
    predicate: "ad4m://inbox",
    target: bobInboxLanguageAddress,
});
```

### 4.3 Sending a message

Alice wants to DM Bob:

```
1. Alice resolves Bob's agent (agent language lookup).
2. Alice queries Bob's public perspective for `ad4m://inbox`.
3. Alice instantiates the referenced Language on her node (if she
   doesn't already have it installed). The Language hash uniquely
   identifies Bob's inbox.
4. Alice encrypts the message content to Bob's public key.
5. Alice calls `perspectiveCommit` on the Language instance with a
   diff containing the encrypted-message link.
6. The Language's transport (Holochain DHT, relay, whatever)
   delivers the commit to Bob's replica.
```

At no point does Alice call a `direct-message` capability. There
isn't one. She's just committing links to a shared Language.

### 4.4 Encryption

**Encryption is the template's responsibility**, not the spec's.
AD4M's Language interface has no encryption concept and
deliberately does not grow one. DM inbox templates encrypt message
payloads to the recipient's public key before committing; the
inbox's transport (friends, DHT, relay) sees only ciphertext.

This gives a nice property: **the AD4M core doesn't need to know
about encryption at all** because it's just opaque link payloads.

### 4.5 Online fast-path via telepresence

When the recipient is online, DMs get instant delivery via
telepresence:

```
1. Alice checks Bob's `ad4m://presence` Language (§3.4) for his
   online status.
2. If Bob is online, Alice ALSO calls `telepresence.sendSignal`
   on Bob's inbox Language (or on Bob's presence Language, depending
   on how the template wires it) with the same encrypted payload.
3. Bob's inbox Language, running on Bob's node, receives the
   telepresence signal and emits it to subscribers.
4. Alice's `perspectiveCommit` still happens — telepresence is the
   fast path, commit is the durable path.
```

If Bob is offline, only the commit happens. Bob sees the message
when his inbox Language's next `perspectiveSyncSync` runs after he
comes online.

---

## 5. Friend-relay: offline delivery

### 5.1 The problem

If Alice's DM inbox commit relies on direct delivery to Bob's node,
and Bob is offline, the message sits on Alice's node until Bob
reappears. This is bad for delivery latency — Alice has to stay
online until Bob does. DHT-backed transports (Holochain, IPFS-PubSub,
…) mitigate this by gossiping commits to nearby nodes, but
"nearby" on a DHT is not "people Bob trusts to hold his mail."

### 5.2 The approach

The **friends list acts as a store-and-forward relay set**. When
Bob's inbox Language is instantiated:

1. The Language's `init()` queries Bob's social perspective for
   `ad4m://friend-of` links where Bob is the source.
2. For each friend DID, the Language asks the friend's node to join
   the inbox's peer membrane as a relay node.
3. When Alice commits a message to Bob's inbox and Bob is offline,
   one of Bob's friends' nodes holds the commit in its local
   replica of the inbox perspective (via `perspective-sync`).
4. When Bob comes back online, his node's `perspectiveSyncSync`
   pulls the backlog from friend relays.

This is entirely a **template implementation detail**. The Language
interface spec does not mention friends, relays, or store-and-forward.
It's just `perspective-sync` over a peer set that happens to include
friend nodes.

### 5.3 Security considerations

Friend relays can see:

- **That a commit happened** (metadata: timestamp, approximate size).
- **Who the sender is** (from the diff's `author` field).
- **That the recipient is the inbox owner** (trivially — they're
  relaying for Bob).

Friend relays CANNOT see:

- **The message content** — encrypted to the owner's key.
- **The recipient's identity beyond the owner** — there is no
  other recipient in an inbox, by construction.

This is the standard end-to-end-encrypted store-and-forward
threat model. Templates SHOULD encrypt all metadata they can
(authorship, timestamps) as part of the payload, exposing only what
the transport needs to route.

---

## 6. Friends as a perspective

### 6.1 Representation

Friends are `ad4m://friend-of` links in a perspective. Which
perspective is an app/runtime choice; see §3.2.

```js
// Add Bob as a friend
await mySocialPerspective.addLink({
    source: myDid,
    predicate: "ad4m://friend-of",
    target: bobDid,
});

// Remove Bob as a friend
await mySocialPerspective.removeLink({
    source: myDid,
    predicate: "ad4m://friend-of",
    target: bobDid,
});

// List all friends (SPARQL)
const results = await mySocialPerspective.query({
    sparql: `
        SELECT ?friend WHERE {
            <${myDid}> <ad4m://friend-of> ?friend .
        }
    `
});
```

### 6.2 Migration from v0.x

In v0.x, the friends list was stored in a local SQLite table managed
by `RuntimeService` (`rust-executor/src/runtime_service/mod.rs`) and
surfaced via `addFriends` / `removeFriends` / `friends()` GraphQL
operations.

v1.0 replaces this with a perspective-backed convention. A runtime
SHOULD continue to expose the GraphQL shims for backward
compatibility, but the implementations become thin wrappers:

```
addFriends(dids)      → addLink(source: me, predicate: ad4m://friend-of, target: did) for each did
removeFriends(dids)   → removeLink(...) for each did
friends()             → SPARQL query over the social perspective
```

The SQLite `friends` table can be dropped, or kept as a local
index/cache.

### 6.3 Symmetry and handshakes

The convention does NOT prescribe symmetry. Applications are free to
interpret one-way `friend-of` as:

- A "follow" (no consent required).
- A pending friend request (awaiting reciprocation).
- Anything else.

Mutual friendship is "both parties have `friend-of` each other."
An app that wants traditional symmetric friends with consent can
require reciprocation before treating the edge as live. An app
that wants one-way follows can skip the handshake. Both are
expressible with the same primitive.

### 6.4 Privacy

Because the perspective hosting `friend-of` links is an app choice,
privacy is too:

- Friends in the **public perspective** → visible to everyone who
  can read the agent.
- Friends in a **private local perspective** → visible only to the
  agent's own nodes.
- Friends in a **shared perspective between two parties** →
  mutually visible but not publicly broadcast.

A runtime MAY default to "private local perspective" for friend
storage, which matches the v0.x SQLite-table semantics.

---

## 7. Summary table

| v0.x concept | v1.0 replacement |
|---|---|
| `Agent.directMessageLanguage` field | `ad4m://inbox` predicate in agent's public perspective |
| `direct-message` Language capability | `perspective-commit` (+ `perspective-sync`, `peers` for owner multi-device) |
| `friendSendMessage` GraphQL mutation | Resolve inbox, call `perspectiveCommit` on the resolved Language |
| `addFriends` / `removeFriends` local SQLite | `ad4m://friend-of` links in a perspective |
| `friends()` query | SPARQL query over the social perspective |
| Friend-gated DM sends (runtime check) | App-level policy using SPARQL over friend-of links |
| Offline DM delivery (unimplemented) | Friend-relay as template-level peer membrane composition |
| `emitDirectMessage` runtime import | `emitPerspectiveDiff` (DMs are just diffs) |

---

## 8. What this document deliberately does NOT do

- **Mandate a specific template for DM inboxes.** Multiple
  implementations are allowed and expected. The convention is just
  the `ad4m://inbox` discovery predicate and the
  capability-composition pattern.
- **Specify encryption schemes.** Templates choose. AD4M has no
  cryptography opinion beyond what signing requires.
- **Prescribe UI.** How a client renders a perspective as a "chat"
  vs. a "forum" vs. a "knowledge base" is an application concern.
  If a UI needs a hint, it can query the perspective's metadata or
  ask the Language via `is-public` / `language-name`.
- **Enforce symmetry on friend-of.** Apps can layer whatever
  social-graph semantics they want on top.
- **Reserve any runtime-level API surface.** Everything here is
  vocabulary on top of the existing Language interface. Runtimes
  SHOULD expose convenience wrappers (`addFriend`, `listInboxes`,
  `sendMessage`) but those are shortcuts over the primitives, not
  new capabilities.
