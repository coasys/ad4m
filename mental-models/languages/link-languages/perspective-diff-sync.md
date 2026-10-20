# Perspective diff sync

**Perspective diff sync** (`p-diff-sync`, package `@coasys/perspective-diff-sync`) is a link language backed by a Holochain DNA. Each commit is a `PerspectiveDiffEntryReference` entry that points to its parent revisions, so the shared links form a revision graph. Agents exchange their current revision over signals and pull the diffs they do not have. Neighbourhoods that already run on it keep working, and anyone can join one by its URL; `neighbourhood.create` does not make new ones (`SyncChoice::Holochain` answers `coming-soon`).

**Why:** A distributed hash table (DHT) needs no server: every member holds the data and can keep syncing when others are offline. The revision graph lets agents merge concurrent changes. Only one agent, the **scribe**, makes merge commits, so two agents do not create competing merges. It is not the default for new neighbourhoods because it reads no membership graph: a Holochain neighbourhood admits whoever holds its URL.

**Parent:** [Link languages](README.md)

<details><summary>How it works</summary>

1. `init()` reads `agentDid()` (the main agent) into `myDid` and calls `holochainRegisterDnas` with the DNA bundled in `build/happ.js`. Templating sets the DNA `network_seed` from `uid`, so each neighbourhood is its own DHT. Every agent on the executor shares one Holochain agent key, so one cell per DNA ([Holochain service](../../agents/executor/services/holochain.md)). The language tells agents apart only by the `my_did` it sends.
2. `perspectiveCommit(diff)`: zome `commit` with `my_did`, up to 5 attempts. Returns the new revision. `my_did` is the `myDid` set at `init()`, also in a neighbourhood that a managed agent owns.
3. `perspectiveSyncSync()`: `create_did_pub_key_link` for each local agent, zome `sync`, then gossip:
   - collect peers from `get_online_agents` and from received broadcasts (dropped after 30 s);
   - scribe = the lowest DID in `[peers…, myDid]`, sorted. Each agent computes this from the peers it sees;
   - for each peer revision that differs, zome `pull({hash, is_scribe})`. A fast-forward applies at once; a merge only on the scribe;
   - report `Synced` or `LinkLanguageInstalledButNotSynced` (most peer revisions differ).
4. Pulled diffs go to the executor through `linkSyncAddCallback`.
5. Telepresence (`set_online_status`, `get_online_agents`, `send_signal`, `send_broadcast`) uses Holochain remote signals; `recv_remote_signal` in the zome forwards them to the language.
6. The executor records a `LinkLanguageChoice` of kind `Holochain` with no creator for each such neighbourhood. It builds no membership replay for it, and the language exports no `handleMembershipChange`.

</details>

<details><summary>Where in the code</summary>

- [bootstrap-languages/p-diff-sync/index.ts](../../../bootstrap-languages/p-diff-sync/index.ts): the language (gossip, scribe choice, exports).
- [bootstrap-languages/p-diff-sync/online-agents.ts](../../../bootstrap-languages/p-diff-sync/online-agents.ts): online agent list.
- [bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/lib.rs](../../../bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/lib.rs): zome externs.
- [bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/link_adapter/](../../../bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync/src/link_adapter): `commit`, `pull`, `render`, `snapshots`, `chunked_diffs`, `topo_sort`, `workspace`.
- [bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync_integrity/src/lib.rs](../../../bootstrap-languages/p-diff-sync/hc-dna/zomes/perspective_diff_sync_integrity/src/lib.rs): entry types (`PerspectiveDiffEntryReference`, `Snapshot`, `HashBroadcast`).
- [rust-executor/src/neighbourhoods/create.rs](../../../rust-executor/src/neighbourhoods/create.rs) (new): the `coming-soon` refusal of `SyncChoice::Holochain`.
- [Holochain service](../../agents/executor/services/holochain.md): the conductor this language runs on.

</details>
