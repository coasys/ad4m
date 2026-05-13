# 5. P-Diff-Sync Protocol

## 5.1 Overview

**Perspective-Diff-Sync** (p-diff-sync) is the reference Link Language implementation that powers Neighbourhood synchronization. It uses a **Holochain DNA** to maintain a distributed DAG (Directed Acyclic Graph) of diffs, similar to a git commit history.

While p-diff-sync is the reference implementation, alternative Link Languages can be created. The [Language Interface](./03-language-interface.md) defines three orthogonal perspective capabilities (`perspective-commit`, `perspective-sync`, `perspective-query`) and the `peers` interface as the abstraction boundary. P-diff-sync exports all four.

## 5.2 Architecture

```
┌─────────────────────────────────────┐
│          AD4M Executor              │
│  ┌───────────────────────────────┐  │
│  │    PerspectiveInstance        │  │
│  │    (manages local state)      │  │
│  └──────────┬────────────────────┘  │
│             │ Flat Language Exports   │
│  ┌──────────▼────────────────────┐  │
│  │    p-diff-sync (ALDK)         │  │
│  │    - perspectiveCommit        │  │
│  │    - perspectiveSyncSync      │  │
│  │    - perspectiveSyncRender    │  │
│  │    - perspectiveQueryRun      │  │
│  │    - peersSetLocal/Remote     │  │
│  │    - telepresence*            │  │
│  └──────────┬────────────────────┘  │
│             │ Holochain Extension     │
│  ┌──────────▼────────────────────┐  │
│  │    Holochain Runtime          │  │
│  │    perspective_diff_sync DNA  │  │
│  └───────────────────────────────┘  │
└─────────────────────────────────────┘
```

## 5.3 Data Structures (Holochain Entries)

### PerspectiveDiff

```rust
struct PerspectiveDiff {
    additions: Vec<LinkExpression>,
    removals: Vec<LinkExpression>,
}

struct LinkExpression {
    author: String,
    data: Triple,
    timestamp: DateTime<Utc>,
    proof: ExpressionProof,
}

struct Triple {
    source: Option<String>,
    target: Option<String>,
    predicate: Option<String>,
}

struct ExpressionProof {
    signature: String,
    key: String,
}
```

### PerspectiveDiffEntryReference

Each commit in the DAG:

```rust
struct PerspectiveDiffEntryReference {
    diff: PerspectiveDiff,
    parents: Option<Vec<ActionHash>>,    // Parent commit(s) — None for root
    diffs_since_snapshot: usize,         // Counter for snapshot scheduling
    diff_chunks: Option<Vec<ActionHash>>, // For large diffs (chunked storage)
}
```

### Snapshot

Periodic full-state snapshots for faster sync:

```rust
struct Snapshot {
    diff_chunks: Vec<ActionHash>,        // References to chunked diff data
    included_diffs: Vec<ActionHash>,     // All diff entries included in snapshot
}
```

### HashBroadcast

Signal sent to peers when a new commit is made:

```rust
struct HashBroadcast {
    reference_hash: ActionHash,          // Hash of the new commit
    reference: PerspectiveDiffEntryReference, // The commit data
    broadcast_author: String,            // DID of the broadcasting agent
}
```

## 5.4 Sync Protocol

### Commit Flow

1. Agent creates/removes links locally
2. Executor batches changes and calls `perspectiveCommit(diff)` on the Language instance
3. The Language (via ALDK) calls the `commit` zome function
4. The zome:
   a. Gets the current revision (latest local commit hash)
   b. Creates a `PerspectiveDiffEntryReference` entry with the diff and parent hash
   c. Updates the local revision pointer
   d. If `diffs_since_snapshot >= SNAPSHOT_INTERVAL`, generates a snapshot
   e. Broadcasts the new hash to peers via Holochain signals

### Chunked Diffs

For large diffs (>500 link operations), the data is split into chunks to avoid Holochain's 4MB entry size limit:

1. Diff data is split into chunks of configurable size
2. Each chunk is stored as a separate entry
3. The parent `PerspectiveDiffEntryReference` references chunks via `diff_chunks`
4. Chunks MUST be stored and validated before the parent entry

### Pull Flow

When an agent receives a `HashBroadcast` signal:

1. Record the peer's current revision and last-seen time
2. During the next `gossip()` cycle:
   a. Compare own revision with peers' revisions
   b. If different, call `pull(hash, is_scribe)` for each unknown revision
   c. The zome traverses the DAG from the given hash, collecting all new diffs
   d. Apply diffs to local state
   e. Update local revision

### Scribe Election

The agent with the lexicographically first DID among online peers is elected "scribe." The scribe has additional responsibilities during pull operations (implementation-defined merge strategy).

### Sync State

Sync state is determined by comparing revisions:
- If the majority of peers share the same revision → `Synced`
- Otherwise → `LinkLanguageInstalledButNotSynced`

### Signal Routing

Signal routing uses the flat Language export model. When a Holochain signal arrives:
1. The runtime routes it via the DnaHash → Language instance map.
2. The Language's `handleHolochainSignal` export processes the signal.
3. The Language calls `emitPerspectiveDiff` / `emitTelepresenceSignal` as appropriate.

This is the event-driven model. See [Language Interface §3.9](./03-language-interface.md#39-event-handler-exports-runtime--language).

### Gossip Protocol

The gossip protocol uses `revisionHexes` in summaries for efficient comparison between peers. Key behaviors:
- **Exponential backoff** in sync loops when no new data is available, reducing network overhead.
- **Active peer discovery** — the gossip cycle actively discovers new peers rather than passively waiting for signals.

## 5.5 Telepresence

### Online Status

Each agent maintains an online status entry (a `PerspectiveExpression`) stored privately in Holochain and linked from an anchor.

### DID-to-AgentPubKey Mapping

Since Holochain uses its own public keys, p-diff-sync maintains a mapping from AD4M DIDs to Holochain `AgentPubKey`s via DHT links.

### Signal Routing

Signals can be:
- **Broadcast** — sent to all active agents
- **Targeted** — sent to a specific agent (by looking up their `AgentPubKey` from their DID)
- **Routed** — for multi-user nodes, signals include a `recipient_did` for routing to the correct user

```rust
struct RoutedSignalPayload {
    recipient_did: String,
    author: String,
    data: Perspective,
    timestamp: DateTime<Utc>,
    proof: ExpressionProof,
}
```

## 5.6 Zome Functions

The p-diff-sync DNA exposes these zome functions:

| Function | Input | Output | Description |
|----------|-------|--------|-------------|
| `commit` | `CommitInput` | `ActionHash` | Commit a diff |
| `current_revision` | `()` | `Option<ActionHash>` | Get current revision |
| `latest_revision` | `()` | `Option<ActionHash>` | Get latest known revision (including from peers) |
| `sync` | `String` (DID) | `Option<ActionHash>` | Broadcast current revision |
| `pull` | `PullArguments` | `PullResult` | Pull diffs from a revision |
| `render` | `()` | `Perspective` | Get full rendered state |
| `set_online_status` | `PerspectiveExpression` | `()` | Set online status |
| `get_online_agents` | `()` | `Vec<OnlineAgent>` | List online agents |
| `send_signal` | `SignalData` | `PerspectiveExpression` | Send targeted signal |
| `send_broadcast` | `PerspectiveExpression` | `PerspectiveExpression` | Broadcast signal |
| `create_did_pub_key_link` | `String` (DID) | `()` | Register DID mapping |
| `get_active_agents` | `()` | `Vec<AgentPubKey>` | List active agents |

## 5.7 Configuration

Configurable parameters (set via Holochain DNA properties or constants):

| Parameter | Default | Description |
|-----------|---------|-------------|
| `SNAPSHOT_INTERVAL` | Implementation-defined | Diffs between snapshots |
| `CHUNK_SIZE` | Implementation-defined | Max links per chunk |
| `CHUNKING_THRESHOLD` | 500 | Diff size triggering chunked storage |
| `ENABLE_SIGNALS` | Implementation-defined | Whether to use Holochain signals |

## 5.8 Validation

The DNA validates entries:
- `PerspectiveDiffEntryReference` entries: all referenced parents MUST exist
- Chunked entries: all referenced chunks MUST exist and be valid
- Other entry types: accepted by default
