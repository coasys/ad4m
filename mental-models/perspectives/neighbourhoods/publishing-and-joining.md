# Publishing and joining

**Publishing** turns an existing perspective into a neighbourhood. The executor installs the chosen link language, stores a neighbourhood expression through the neighbourhood language, and records the resulting `neighbourhood://` URL on the perspective. **Joining** goes the other way: fetch the neighbourhood expression by URL and create a new perspective bound to it. The perspective then installs the link language in the background. If another agent on the same executor already joined that neighbourhood, the joining agent instead becomes a **co-owner** of the existing perspective.

**Why:** Publishing from an existing perspective lets an agent start alone and share later; the executor pushes shared links written before publishing to the link language. Co-owners on one executor share one perspective, so one store and one link-language isolate serve every local agent in the same space, not one of each per agent. Local links are a layer in that same store, on top of the shared links.

**Parent:** [Neighbourhoods](README.md)

<details><summary>How it works</summary>

Publish (`neighbourhood.publish(perspectiveUuid, linkLanguage, meta)`):

1. `LanguageController::install_language(linkLanguage)`. The client normally creates a unique link language from a template first ([Language trust and templates](../../languages/runtime/installing/trust-and-templates.md)).
2. `create_neighbourhood_with_context` stores `Neighbourhood { link_language, meta }`, signed as the caller's agent.
3. Set `shared_url`, `neighbourhood`, state `NeighbourhoodCreationInitiated`; add the caller's DID to `owners`.
4. `ensure_public_links_are_shared`: commit the existing shared links. `ensure_link_language` does it again once the link language is loaded.

Join (`neighbourhood.join(url)`):

1. If any perspective already has this `shared_url`: add the caller's DID to its `owners` (memory and SQLite), and return that handle. A background task (`register_owners_in_background`) calls `update_local_agents`, which reads the owners when it sends.
2. Otherwise fetch the `Neighbourhood` and create a new perspective with name and `shared_url` = the URL, `owners` = [caller], state `NeighbourhoodJoinInitiated`. The join returns here. `ensure_link_language` installs the link language in the background. The state then moves to `LinkLanguageInstalledButNotSynced` and `Synced`, or to `LinkLanguageFailedToInstall`.
3. The background loops take over ([Neighbourhood sync](sync.md)).

</details>

<details><summary>Where in the code</summary>

- [neighbourhoods.rs](../../../rust-executor/src/neighbourhoods.rs): `neighbourhood_publish_from_perspective_with_context`, `install_neighbourhood_with_context`.
- [api/neighbourhoods_ws.rs](../../../rust-executor/src/api/neighbourhoods_ws.rs): `publish_neighbourhood`, `join_neighbourhood`.
- [types/domain.rs](../../../rust-executor/src/types/domain.rs): `PerspectiveHandle` (`owners`, `add_owner`, `is_owned_by`), `Neighbourhood`, `DecoratedNeighbourhoodExpression`.
- [db.rs](../../../rust-executor/src/db.rs): `add_owner_to_neighbourhood`, `get_neighbourhood_owners`.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `ensure_public_links_are_shared`, `update_local_agents`.
- SDK: [core/src/neighbourhood/NeighbourhoodClient.ts](../../../core/src/neighbourhood/NeighbourhoodClient.ts).

</details>
