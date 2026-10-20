# Counts by person

`neighbourhood.countByPerson` gives neighbourhood templates one read that counts **people, not keys**: for a vote predicate, how many members chose each target, each person counted once through their latest counted link from any device. Bots, guests and device-only identities never count. The same answers from `author_of` set the limits on **device-only identities** in neighbourhoods, and the executor records the **reach of a revocation**: which neighbourhoods dropped a revoked key, and when a room-key rotation left it out.

**Why:** Votes and quorums inside templates must count people. If a template counted device keys, a person with three devices would vote three times, and a bot would vote as its owner. A lookup that fails counts as no person, so an outage can shrink a count but never inflate it. The revocation record lets the person's revoke screen list the neighbourhoods a revocation reached.

**Parent:** [Membership](README.md)

<details><summary>How it works</summary>

Count (`PersonCountQuery { perspectiveUuid, source?, predicate, at? }` → `PersonCount { by_target, counted, eligible, skipped }`):

1. Only `Valid` links count ([Link verdicts](../../links/verdicts.md)).
2. The author key maps to a person who is a member or admin at the link's time.
3. A bot, a guest, a `NoLog` key or an `Unavailable` answer never counts; `skipped` reports each kind, so a template knows a count may still grow.
4. Each person counts once: their latest counted link wins. A quorum is the template's own ratio of `counted` to `eligible`.

Flows do not use this count: their consensus fold counts distinct device keys ([Flow consensus](../../social-dna/flows/consensus.md)).

Device-only identities (`NoLog`: a guest, an agent with no key log):

| May not | Enforced by |
|---|---|
| create a shared neighbourhood | `neighbourhood.create` refuses `needs-identity`; the replay counts a `rules` link only from a `did:scid` creator |
| join as a member | `neighbourhood.join` without a guest invite refuses `needs-identity` |
| hold admin or the add right, or make an invite | the replay: those targets and authors must be persons |
| count in a vote | `countByPerson` skips it |

All limits end at once when an identity's first event lists the key.

Revocation reach: when `KeyResolver::subscribe` reports a key `Revoked`, the executor writes one `RevocationReach { did_key, perspective_uuid, dropped_at, rotated_at }` row per local neighbourhood where the key's person is a member. The server link language fills the times through the host import `roomKeyEvent` ([Room keys](../../../languages/link-languages/server-link-language/room-keys.md)). A Holochain or legacy neighbourhood gets a row with both times empty. Rows live 30 days; `neighbourhood.revocationReach { didKey }` answers the person who owns the key.

</details>

<details><summary>Where in the code</summary>

- [membership.rs](../../../../rust-executor/src/membership.rs) (new): `PersonCountQuery`, `PersonCount`, `SkipCounts`, `RevocationReach`.
- [identity/resolver.rs](../../../../rust-executor/src/identity/resolver.rs) (new): `author_of`, `KeyResolver::subscribe`.
- [api/neighbourhoods_ws.rs](../../../../rust-executor/src/api/neighbourhoods_ws.rs): `neighbourhood.countByPerson`, `neighbourhood.revocationReach`.

</details>
