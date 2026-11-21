# Link languages

A **link language** holds and syncs the shared links of one **neighbourhood**, and carries its telepresence. The perspective commits its shared diffs to it; the language pushes other agents' diffs back. Each neighbourhood normally has its own templated copy. The executor routes the diffs of one link-language address to one perspective. Two agents who join one neighbourhood on one executor share that perspective as co-owners; their local links are a layer on top of it.

**Why:** The sync protocol is the part that changes most: a Holochain distributed hash table (DHT), a self-hosted server, or local only. In a language, it can differ per neighbourhood without a change to the executor.

**Parent:** [Languages](../README.md)

## Parts
- [Perspective diff sync](perspective-diff-sync.md): `p-diff-sync`, the Holochain link language: a revision graph of diffs, merged by one scribe.
- [Server link language](server-link-language.md): syncs through a link server, with end-to-end encryption.
- [Link server](link-server.md): the self-hosted Node.js server behind the server link language.
- [Local link language](local-link-language.md): no network, for tests, Docker and standalone executors.

<details><summary>How it works</summary>

A link language exports `perspectiveCommit`, and usually `perspectiveSyncSync`, peers and telepresence functions. The executor's loops that call it (load, `sync()` every 3 s, the commit queue, `peersSetLocal` with the local owners) are on [Neighbourhood sync](../../perspectives/neighbourhoods/sync.md). The language side:

| Executor calls (`Language` in `language.rs`) | Language export |
|---|---|
| `commit(diff)` → revision | `perspectiveCommit` |
| `sync()`, `render()`, `current_revision()` | `perspectiveSyncSync`, `perspectiveSyncRender`, `perspectiveSyncCurrentRevision` |
| `set_local_agents(dids)`, `others()` | `peersSetLocal`, `peersRemote` |
| telepresence calls ([Presence and signals](../../perspectives/neighbourhoods/presence-and-signals.md)) | `telepresence*` |

1. Remote diffs reach the executor through `emitPerspectiveDiff(diff)` or the `linkSyncAddCallback` setter; both end in op `perspective_diff_received`.
2. `handle_perspective_diff_from_link_language` finds the perspective for the address and calls `diff_from_link_language`, which stores the links as `Shared` ([Link write path](../../perspectives/links/write-path.md)). The routing cache `LINK_LANG_TO_PERSPECTIVE_HANDLE` holds one perspective per address; on a miss it takes the first perspective whose `neighbourhood.data.link_language` equals the address. Join reuses a perspective by neighbourhood URL, which makes a second local agent a co-owner.
3. Sync state has two writers. The executor sets `Synced` after every successful `sync()` call (`nh_sync_loop`). The language reports state through `emitSyncStateChange`; p-diff-sync reports `LinkLanguageInstalledButNotSynced` when peer revisions differ. The last write sets the perspective's state.
4. Telepresence signals go back through `emitTelepresenceSignal`.

</details>

<details><summary>Where in the code</summary>

- [rust-executor/src/languages/language.rs](../../../rust-executor/src/languages/language.rs): `commit`, `sync`, `current_revision`, `render`, `set_local_agents`, `others`, telepresence calls.
- [rust-executor/src/perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `ensure_link_language`, `nh_sync_loop`, commit loop, `diff_from_link_language`.
- [rust-executor/src/perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): `LINK_LANG_TO_PERSPECTIVE_HANDLE`, `handle_perspective_diff_from_link_language`, `handle_sync_state_changed_from_link_language`, `handle_telepresence_signal_from_link_language`.
- [rust-executor/src/js_core/languages_extension.rs](../../../rust-executor/src/js_core/languages_extension.rs): ops from language to executor.
- [rust-executor/src/neighbourhoods.rs](../../../rust-executor/src/neighbourhoods.rs): publish and join.

</details>