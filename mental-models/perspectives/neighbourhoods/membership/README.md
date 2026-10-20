# Membership

The **membership graph** of a neighbourhood is a named graph, `coasys:membership`, in its perspective: ordinary signed links that name the rules, the join mode, admins, members, guests, invites, removals and bots. It only grows, and no room key covers it. A link in it counts only when the graph, as of the link's time, gives the right to its author's person. The executor keeps one replay per neighbourhood and offers it to link languages as the **membrane interface**; the link server replays the same graph to admit room keys.

**Why:** Who may enter must not differ per link language, so membership lives in the perspective, not in the transport. The graph names people (`did:scid`), so a person's second device needs no new admission. It stays plaintext so that the link server can admit by person without reading the neighbourhood.

**Parent:** [Neighbourhoods](../README.md)

## Parts
- [Open joins and invites](invites.md): the join mode, self-joins, invite keys and proofs, and the rule after a removal.
- [Guests](guests.md): web visitors with only a device key, their invite, their name, and "Keep this".
- [Counts by person](counts-by-person.md): votes that count people, the limits on device-only identities, and the reach of a revocation.

<details><summary>How it works</summary>

| Link `<nh>` → predicate → target | Counts when the author's person … |
|---|---|
| `membership:rules` → `{ add, guests, join }` | is the `CREATOR`; first counted link only; the creator becomes the first admin |
| `membership:join` → `"open"` / `"invite"` | holds `admin`; applies from its timestamp, removes nobody |
| `membership:admin` → `did:scid` | holds `admin` |
| `membership:member` → `did:scid` | holds the add right; or targets itself and the neighbourhood is open, or the link carries a valid invite proof |
| `membership:guest` → `did:key` | targets itself, maps to no person, and carries a valid guest invite proof; the rules admit guests |
| `membership:invite` → `{ pub, role, expires, uses }` | holds the add right |
| `membership:removed` → person or key | holds `admin`, or is the target (leaving), or owns the bot |
| `membership:bot` → bot `did:key`, plus `approved-by` → owner | holds the add right; the owner approves with a key that is not a bot's |

1. Replay orders links by timestamp, then hash. A link that fails its rule is stored and counts for nothing. An `Unavailable` author keeps the link for a later retry.
2. Link languages read the replay through the `membership*` host imports ([Language isolates](../../../languages/runtime/isolates.md)).
3. Reads skip each link whose author maps to no member or admitted guest at the link's time ([Querying](../../querying/README.md)).
4. The executor refuses a local removal of a membership link, drops an incoming one, and ignores a membership link inside an encrypted diff.

RPC: `neighbourhood.members`, `addMember`, `removeMember`, `leave`, `createInvite`, `invites`, `setJoinMode`, `join`, `countByPerson`, `revocationReach`.

</details>

<details><summary>Where in the code</summary>

- [ad4m-identity-core/src/membership.rs](../../../../ad4m-identity-core/src/membership.rs) (new): `MembershipGraph`, the replay rules.
- [membership.rs](../../../../rust-executor/src/membership.rs) (new): `Membership`, one replay per neighbourhood.
- [api/neighbourhoods_ws.rs](../../../../rust-executor/src/api/neighbourhoods_ws.rs): the membership RPC methods.
- The replay on the link server: [Link server](../../../languages/link-languages/server-link-language/link-server.md).

</details>
