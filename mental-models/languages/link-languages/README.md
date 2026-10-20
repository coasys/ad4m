# Link languages

A **link language** holds and syncs the shared links of one **neighbourhood**, and carries its telepresence. The perspective commits its shared diffs to it; the language pushes other agents' diffs back. Each neighbourhood has its own templated copy, so a link-language address belongs to one perspective on an executor. Two agents who join one neighbourhood on one executor share that perspective as co-owners; their local links are a layer on top of it. A neighbourhood keeps the link language it starts with: the executor records the choice once (`LinkLanguageChoice`) and no call changes it. Every link language reads the neighbourhood's [membership graph](../../perspectives/neighbourhoods/membership/README.md) through one set of host imports.

**Why:** The sync protocol is the part that changes most: a self-hosted server, a Holochain distributed hash table (DHT), or local only. In a language, it can differ per neighbourhood without a change to the executor. Who may join must not differ per language, so admission lives in one membership graph that each language reads, not in the language. A fixed link language keeps the slice small: changing a neighbourhood's sync would need a migration to a new neighbourhood.

**Parent:** [Languages](../README.md)

## Parts
- [Server link language](server-link-language/README.md): the default for new neighbourhoods; syncs through a link server, with end-to-end encryption and admission by person.
- [Perspective diff sync](perspective-diff-sync.md): `p-diff-sync`, the Holochain link language; existing neighbourhoods keep it, new ones cannot pick it.
- [Local link language](local-link-language.md): no network, for tests, Docker and standalone executors.

<details><summary>How it works</summary>

A link language exports `perspectiveCommit`, and usually `perspectiveSyncSync`, peers and telepresence functions. The executor's loops that call it (load, `sync()`, the commit queue, `peersSetLocal` with the local owners) are on [Neighbourhood sync](../../perspectives/neighbourhoods/sync.md). The language side:

| Executor calls (`Language` in `language.rs`) | Language export |
|---|---|
| `commit(diff)` → revision | `perspectiveCommit` |
| `sync()`, `render()`, `current_revision()` | `perspectiveSyncSync`, `perspectiveSyncRender`, `perspectiveSyncCurrentRevision` |
| `set_local_agents(dids)`, `others()` | `peersSetLocal`, `peersRemote` |
| telepresence calls ([Presence and signals](../../perspectives/neighbourhoods/presence-and-signals.md)) | `telepresence*` |
| a membership change | `handleMembershipChange` (optional) |

1. Remote diffs reach the executor through `emitPerspectiveDiff(diff)` or the `linkSyncAddCallback` setter; both end in op `perspective_diff_received`, which acts only for the calling isolate's own language address.
2. `handle_perspective_diff_from_link_language` finds the perspective for the address and calls `diff_from_link_language`, which stores the links as `Shared` ([Link write path](../../perspectives/links/write-path.md)). The routing cache `LINK_LANG_TO_PERSPECTIVE_HANDLE` holds one perspective per address; on a miss it takes the first perspective whose `neighbourhood.data.link_language` equals the address. Join reuses a perspective by neighbourhood URL, which makes a second local agent a co-owner. The executor drops a removal of a membership link and a membership link inside an encrypted diff.
3. Sync state has two writers. The executor sets `Synced` after every successful `sync()` call (`nh_sync_loop`). The language reports state through `emitSyncStateChange`: p-diff-sync reports `LinkLanguageInstalledButNotSynced` when peer revisions differ; the server link language reports it while its key ring is not `ready`. The last write sets the perspective's state.
4. Telepresence signals go back through `emitTelepresenceSignal`.
5. The language's storage is sealed under the perspective's store key, because the address maps to one perspective ([Language isolates](../runtime/isolates.md)).

`neighbourhood.create` picks the language by `SyncChoice`: `Default` and `OwnLinkServer { url }` give the server link language; `Holochain` is refused with `coming-soon`. The create and join flows: [Creating and joining](../../perspectives/neighbourhoods/creating-and-joining.md). The membership graph: [Membership](../../perspectives/neighbourhoods/membership/README.md).

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/language.rs](../../../rust-executor/src/languages/language.rs): `commit`, `sync`, `current_revision`, `render`, `set_local_agents`, `others`, telepresence calls, the membership-change call.
- [rust-executor/src/perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `ensure_link_language`, `nh_sync_loop`, commit loop, `diff_from_link_language`.
- [rust-executor/src/perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): `LINK_LANG_TO_PERSPECTIVE_HANDLE`, `handle_perspective_diff_from_link_language`, `handle_sync_state_changed_from_link_language`, `handle_telepresence_signal_from_link_language`.
- [rust-executor/src/js_core/languages_extension.rs](../../../rust-executor/src/js_core/languages_extension.rs): ops from language to executor.
- [rust-executor/src/neighbourhoods/create.rs](../../../rust-executor/src/neighbourhoods/create.rs) (new): `SyncChoice`, `LinkLanguageChoice`; [rust-executor/src/neighbourhoods.rs](../../../rust-executor/src/neighbourhoods.rs): publish and join.
- [rust-executor/src/membership.rs](../../../rust-executor/src/membership.rs) (new): one membership replay per neighbourhood.

</details>
