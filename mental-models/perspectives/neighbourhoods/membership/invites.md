# Open joins and invites

Each neighbourhood has a **join mode** in its membership graph: **open** (the default) or **invite only**. In an open neighbourhood, anyone who holds the neighbourhood's link and whose key maps to a person (a valid key in a log, with no bot mark) joins by signing their own `membership:member` link. In an invite-only neighbourhood, a newcomer's member link counts only with an **invite proof**: a signature by the secret half of an **invite key** whose public half sits in a `membership:invite` link with a role, an expiry and a use count. A guest always needs a guest invite ([Guests](guests.md)).

**Why:** Open by default makes sharing a neighbourhood as easy as sharing its link; an admin who wants control switches to invites, and the switch removes nobody. The option that lost was invites only, because an open rule, once neighbourhoods use it, cannot be taken back from them. Invites solve a second problem: the graph names people by `did:scid`, which a member rarely knows, so the inviter publishes a key instead of a name. The secret travels in the URL fragment, which never reaches a web server's log, and only its holder can sign the proof, so a reader of the plaintext graph cannot use an invite.

**Parent:** [Membership](README.md)

<details><summary>How it works</summary>

Open join:

1. The share link carries no secret: `…/join/<sharedId>?host=<executorUrl>`, or the bare `neighbourhood://` URL.
2. The joiner's executor checks that the session's agent has a `did:scid`, then signs its own member link ([Creating and joining](../creating-and-joining.md)).
3. The language sends that link on the membership channel with a not-yet-admitted token. The link server counts it when the join mode at the link's time is open, and admits the key. Any member's executor then grants the room keys ([Room keys](../../../languages/link-languages/server-link-language/room-keys.md)).

Invite (`neighbourhood.createInvite { role, expiresAt, maxUses }`, by an owner whose person holds the add right):

1. The inviter's executor makes an Ed25519 key pair from 32 random bytes and signs `membership:invite` with the public key, role, expiry and use count. It returns the secret once; at most 50 invites are live per neighbourhood.
2. The share link carries the secret in its fragment: `…#i=<secret>`. The executor never logs it and never sends it to the link server.
3. The joiner's self-join link adds `invite: { pub, sig }`, where `sig` signs `(<nh>, target, author did:key)` with the invite key. The replay counts it before the expiry and within the uses.

Rules that hold for both:

- A person's second device needs no invite: its key maps to a person the graph already names, so the server admits it at once.
- `neighbourhood.setJoinMode` (admins only) writes `membership:join`; the latest counted one wins from its timestamp.
- After an admin's `removed` link for a person, the open rule no longer counts that person's self-join; only an add by someone with the right, or a valid invite, admits them again. A person who left by their own `removed` link may rejoin an open neighbourhood.
- A rules literal without `join` reads as invite only: neighbourhoods made before the join mode keep their old rule.

</details>

<details><summary>Where in the code</summary>

- [ad4m-identity-core/src/membership.rs](../../../../ad4m-identity-core/src/membership.rs) (new): `Join`, the invite and self-join rules.
- [membership.rs](../../../../rust-executor/src/membership.rs) (new): invite use counts per neighbourhood.
- [api/neighbourhoods_ws.rs](../../../../rust-executor/src/api/neighbourhoods_ws.rs): `neighbourhood.createInvite`, `invites`, `setJoinMode`, `join { url, invite? }`; the refusal reasons of `neighbourhood-join-refused` (`not-a-member`, `invite-only`, `needs-identity`, `invite-expired`, `invite-used`, `guests-not-admitted`, `server-refused`).

</details>
