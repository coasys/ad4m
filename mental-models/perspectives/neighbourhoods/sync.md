# Neighbourhood sync

On the perspective side, sync is four background loops on each neighbourhood `PerspectiveInstance`. They load the link language, call its `sync()`, flush the queue of commits that could not go out, and commit again the shared links the link language is missing. Peer diffs arrive through a callback, routed by link language address. The perspective's `PerspectiveState` reports where sync stands.

**Why:** A link language can be slow, offline or not yet installed, but local writes must never wait for it. So the executor stores a write first and commits it in the background. A failed commit goes to a queue in SQLite and is retried. Diffs were lost in practice (for example links written before publishing), so a fallback loop compares the store with the language's rendered state and commits what is missing.

**Parent:** [Neighbourhoods](README.md)

<details><summary>How it works</summary>

| `PerspectiveState` | Set when |
|---|---|
| `Private` | no neighbourhood |
| `NeighbourhoodCreationInitiated` | after publish |
| `LinkLanguageFailedToInstall` | install failed; retried after 0.5 s, doubling up to 30 s |
| `LinkLanguageInstalledButNotSynced` | language loaded, or the last `sync()` failed |
| `Synced` | `sync()` succeeded, or the language lacks the `PerspectiveSync` language capability |

Loops (`start_background_tasks`):

1. `ensure_link_language`: a private perspective waits, without a timer, until a publish sets its neighbourhood (`update_perspective_with` calls `link_language_changed`, which wakes the loop). Then `language_by_address`, else `install_language`; a failed install retries after 0.5 s, doubling up to 30 s. Once loaded, `attach_link_language` tells the language the local owners (`register_local_agents` → `set_local_agents`, one registration at a time) and registers the address → perspective mapping.
2. `nh_sync_loop`: the first `sync()` runs as soon as the link language attaches; then every 3 s, doubling up to 5 min after failures.
3. Outbound: `commit(diff)` sends at once while the queue is empty and "immediate commits" are left (20, refilled after each queue flush). Otherwise the diff goes to `perspective_diff` in SQLite. `pending_diffs_loop` (100 ms tick) flushes the queue after 1 s without new diffs, 3 s after a burst began, or at 150 diffs; one commit carries up to 3 MB.
4. `fallback_sync_loop` (when `Synced`; first after 30 s, then every 5 min after a success, doubling from 30 s up to 5 min after failures): `ensure_public_links_are_shared` compares local shared links with `render()` and commits the missing ones.
5. Inbound: the link language calls back; `handle_perspective_diff_from_link_language` finds the perspective by language address and calls `diff_from_link_language` ([Link write path](../links/write-path.md)).
6. The link language can also set the state: its `emitSyncStateChange` callback reaches `handle_sync_state_changed_from_link_language`, which calls `update_perspective_state` on the perspective.

</details>

<details><summary>Where in the code</summary>

- [perspectives/perspective_instance.rs](../../../rust-executor/src/perspectives/perspective_instance.rs): `ensure_link_language`, `attach_link_language`, `LoopWakes`, `nh_sync_loop`, `commit`, `spawn_commit_and_handle_error`, `pending_diffs_loop`, `commit_pending_diffs`, `fallback_sync_loop`, `ensure_public_links_are_shared`, `diff_from_link_language`, `update_perspective_state`; constants `IMMEDIATE_COMMITS_COUNT`, `MAX_PENDING_DIFFS_COUNT`, `MAX_PENDING_SECONDS`, `MAX_COMMIT_BYTES`, `LINK_LANGUAGE_RETRY_MIN`, `LINK_LANGUAGE_RETRY_MAX`.
- [perspectives/perspective_instance/local_agents.rs](../../../rust-executor/src/perspectives/perspective_instance/local_agents.rs): `register_local_agents`.
- [perspectives/mod.rs](../../../rust-executor/src/perspectives/mod.rs): `handle_perspective_diff_from_link_language`, `handle_sync_state_changed_from_link_language`, `register_link_language_perspective`.
- [db.rs](../../../rust-executor/src/db.rs): `add_pending_diff`, `get_pending_diffs`, `get_pending_diffs_by_size`, `clear_pending_diffs`.
- [types/domain.rs](../../../rust-executor/src/types/domain.rs): `PerspectiveState`.

</details>
