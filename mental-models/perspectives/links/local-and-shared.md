# Local and shared links

Every stored link has a **link status**: `Shared` (the default) or `Local`. In a neighbourhood, the executor commits a shared link to the link language, so it reaches other agents; a local link stays in this executor's store. Status decides only whether a link leaves the executor.

**Why:** Apps need private notes, drafts and per-agent state next to shared data, and one graph lets one query read both. A per-link flag was the cheapest way to get this.

**Parent:** [Links and store](README.md)

<details><summary>How it works</summary>

| Path | Status used |
|---|---|
| `perspective.addLink`, `addLinks`, `linkMutations` additions | the `status` parameter (`parse_link_status`): missing, `"shared"` or `"SHARED"` means `Shared`; any other value means `Local` |
| `perspective.removeLinks`, `linkMutations` removals | the status the link was stored with, not the call's |
| Model property declared `local: true`, SHACL action with `local` | `Local` (`execute_commands`) |
| Links that arrive from the link language | always `Shared` (`diff_from_link_language`) |
| SDNA and SHACL links (`add_sdna`) | always `Shared` |
| Membership links (named graph `coasys:membership`) | always `Shared`; on the server link language they travel in plaintext through the room's membership channel, never inside an encrypted diff ([Membership](../neighbourhoods/membership/README.md)) |
| Built-in `InterpretationRun.debugPrompt` / `debugResponse` | `Local` ([hardwired_sdna/interpretation_run.json](../../../rust-executor/src/perspectives/hardwired_sdna/interpretation_run.json)) |

1. The status is stored as the literal `"Local"` or `"Shared"` on the link's reifier (`ad4m://ontology/status`).
2. After the store write, only shared links go to `spawn_commit_and_handle_error`, which commits them to the link language ([Neighbourhood sync](../neighbourhoods/sync.md)). A perspective without a neighbourhood commits nothing.
3. Reads do not filter by status unless asked: SPARQL must match `ad4m://ontology/status` itself; a model query takes `linkStatus`; a model property declared `local: true` reads only local links.

</details>

<details><summary>Where in the code</summary>

- [types/domain.rs](../../../rust-executor/src/types/domain.rs): `LinkStatus`.
- [api/perspectives_ws.rs](../../../rust-executor/src/api/perspectives_ws.rs): `parse_link_status`, `add_link`, `remove_links_bulk`.
- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `add_link_expression` (the `Shared` check), `link_mutations` (a removal keeps the stored status), `commit_batch` (splits a batch into shared and local).
- [model_query/sparql_builder.rs](../../../rust-executor/src/perspectives/model_query/sparql_builder.rs): `link_status_filter`, `local_status_filter`.
- SDK: `local` option in [core/src/model/decorators.ts](../../../core/src/model/decorators.ts).

</details>
