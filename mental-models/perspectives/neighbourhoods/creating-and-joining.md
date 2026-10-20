# Creating and joining

**Creating** turns an existing perspective into a neighbourhood in one call, `neighbourhood.create`. The executor picks the link language from a **sync choice**, fills its template, publishes it and the neighbourhood expression, records a **link-language choice** that never changes, and signs the membership genesis. **Joining** goes the other way: `neighbourhood.join` fetches the neighbourhood expression by URL, installs its link language, signs the joiner's own member link, and binds a perspective to the neighbourhood once the membrane admits the joiner's person. If another agent on the same executor already joined that neighbourhood, the joining agent instead becomes a **co-owner** of the existing perspective. A device holds the neighbourhoods the person made or joined from it, and nothing else.

**Why:** The executor fills the template, so an app needs no link-server address and no template knowledge. A neighbourhood keeps the link language it starts with: no call changes its sync, so a move to another link language would be a new neighbourhood, never an edit. The room id derives from the creator and a random salt, so the neighbourhood URL binds the server, the room and the creator, a joiner needs no discovery, and nobody can create a room in another person's name. Co-owners on one executor share one perspective, so one store and one link-language isolate serve every local agent in the same neighbourhood, not one of each per agent. Local links are a layer in that same store, on top of the shared links.

**Parent:** [Neighbourhoods](README.md)

<details><summary>How it works</summary>

Create (`neighbourhood.create { perspectiveUuid, sync, rules }`, by an owner whose agent has a `did:scid`; else `403 needs-identity`):

1. `SyncChoice`: `Default` (the server link language on the [link-server directory](../../languages/link-languages/server-link-language/link-server-directory.md)'s default), `OwnLinkServer { url }` (refused unless the URL answers protocol 2 or later), or `Holochain` (refused with `coming-soon`).
2. Fill `SERVER_URL`, `UID` (a fresh 32-byte salt) and `CREATOR` (the person's `did:scid`); publish the language. Room id = `base58(sha256(CREATOR ‖ 0x00 ‖ UID))`.
3. Publish the neighbourhood expression, store `LinkLanguageChoice { kind, link_language, server_url, room_id, creator, fixed_at }`, set state `NeighbourhoodCreationInitiated`.
4. Sign the `rules` link (default `{ add: admin, guests: false, join: open }`) and the creator's `admin` link into the membership graph ([Membership](membership/README.md)). The language creates the room on the link server from them.

`neighbourhood.publish` stays for other link languages and old callers, and refuses the server link language's template ("use neighbourhood.create"). It writes no membership genesis, so a neighbourhood made with it has no membrane and admits whoever holds its URL, as Holochain neighbourhoods made before the membership graph do.

Join (`neighbourhood.join { url, invite? }`):

1. Fetch the `Neighbourhood`, install its link language, read `SERVER_URL`, the room id and `CREATOR` from it, store its `LinkLanguageChoice`.
2. Without a guest invite, the caller's agent must have a `did:scid` (else `needs-identity`). Sign the caller's own `membership:member` link, with an invite proof when the link carried one ([Open joins and invites](membership/invites.md)).
3. Only after the membrane admits the caller's person (or guest key): if a perspective already has this `shared_url`, add the caller's DID to its `owners` and call `update_local_agents`; otherwise create a perspective with `owners` = [caller]. A refusal sends `neighbourhood-join-refused` with its reason.
4. The background loops take over ([Neighbourhood sync](sync.md)).

Neighbourhoods a device holds: `spaces.list` returns a `SpaceHolding` per neighbourhood perspective the session's agent owns on this executor (`how`: made, joined or joined as guest; the choice; the sync state; the session person's role). "Remove from this device" is `perspective.remove` and changes no membership. The apps' "Leave the space" is `neighbourhood.leave`, which writes the person's `removed` link and may then remove the local copy. Neighbourhoods made before the membership graph keep their old open behaviour, with no membership replay, and `spaces.list` marks them legacy.

</details>

<details><summary>Where in the code</summary>

- [neighbourhoods/create.rs](../../../rust-executor/src/neighbourhoods/create.rs) (new): `SyncChoice`, `LinkLanguageChoice`, `CreateNeighbourhoodParams`.
- [neighbourhoods.rs](../../../rust-executor/src/neighbourhoods.rs): `neighbourhood_publish_from_perspective_with_context`, `install_neighbourhood_with_context`.
- [api/neighbourhoods_ws.rs](../../../rust-executor/src/api/neighbourhoods_ws.rs): `neighbourhood.create`, `publish_neighbourhood`, `join_neighbourhood`, `neighbourhood.linkLanguageChoice`, `neighbourhood.leave`, `spaces.list`.
- [types/domain.rs](../../../rust-executor/src/types/domain.rs): `PerspectiveHandle` (`owners`, `add_owner`, `is_owned_by`), `Neighbourhood`, `DecoratedNeighbourhoodExpression`.
- [db.rs](../../../rust-executor/src/db.rs): `add_owner_to_neighbourhood`, `get_neighbourhood_owners`.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `ensure_public_links_are_shared`, `update_local_agents`.
- [mcp/tools/neighbourhoods.rs](../../../rust-executor/src/mcp/tools/neighbourhoods.rs): the clone tool, which calls `neighbourhood.create`.
- SDK: [core/src/neighbourhood/NeighbourhoodClient.ts](../../../core/src/neighbourhood/NeighbourhoodClient.ts).

</details>
