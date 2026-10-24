# Auto-processor

An **auto-processor** is an `AutoProcessor` instance in a perspective that makes the executor run interpretation by itself. It watches a scope of messages (a SPARQL source query), collects new turns into batches, and runs interpretation once per batch. In a neighbourhood every member's executor watches the same scope. So the executors elect **one** member to run each batch, and the winner writes a **processing claim** so the others back off.

**Why:** When app UI code ran interpretation, every open client could process the same messages. That doubled LLM cost and wrote duplicate instances. In the executor the loop runs whether or not a UI is open. The elected runner is the first online author of the batch, so the pass runs as a participant and the written instances carry a participant's DID.

**Parent:** [Interpretation](README.md)

<details><summary>How it works</summary>

1. `perspective.addAutoProcessor` stores an `AutoProcessorConfig` as an `AutoProcessor` instance: `processorId`, `sourceScopeQuery`, `interpretationClasses` (class URIs; `perspective.runInterpretation` takes class names), `flows`, `basePrefix`, `debounceMs`, `batchMin`, `batchMax`, claim TTL, source window, scopes, and an optional tool-call budget for the harness path.
2. Each perspective runs `auto_processor_supervisor`. It runs one watch loop as the main agent. In multi-user mode it also runs one loop per managed agent whose `last_seen` is inside the online window (`MANAGED_USER_ONLINE_WINDOW_S`, 600 s: twice `LAST_SEEN_WRITE_THROTTLE_S`), re-checked every 5 s. This covers every such managed agent, also one that does not own the perspective; a non-owner stands down at election.
3. Each tick (`run_auto_processor_tick`): load processors, gather turns from the source query minus turns already in an `InterpretationRun.sources` (the cursor), debounce and batch (`WatcherState`).
4. For a ready batch: `elect_author` picks the first batch author, in message order, who is this agent or online (`online_agents`, where a co-owner counts as online for 300 s after its `last_seen`; [Presence and signals](../../neighbourhoods/presence-and-signals.md)). If that is someone else, stand down.
5. The winner writes a `ProcessingClaim` (shared link) for the batch key. A live claim from a peer means back off. The claim syncs like any link, so two peers can both claim before either sees the other's claim; then the lowest DID wins.
6. Run interpretation (single shot, or the harness when `max_tool_calls > 0`) and its flow passes; emit step events (`auto-processor-event`).

</details>

<details><summary>Where in the code</summary>

- [perspectives/auto_processor/](../../../../rust-executor/src/perspectives/auto_processor): [config.rs](../../../../rust-executor/src/perspectives/auto_processor/config.rs) (`AutoProcessorConfig`, `load_processors`), [watcher.rs](../../../../rust-executor/src/perspectives/auto_processor/watcher.rs) (`WatcherState`, `elect_author`, `select_online_managed_users`), [cursor.rs](../../../../rust-executor/src/perspectives/auto_processor/cursor.rs), [claim.rs](../../../../rust-executor/src/perspectives/auto_processor/claim.rs) (`ProcessingClaim`, `try_claim`, `batch_key`), [events.rs](../../../../rust-executor/src/perspectives/auto_processor/events.rs).
- [perspectives/perspective_instance.rs](../../../../rust-executor/src/perspectives/perspective_instance.rs): `auto_processor_supervisor`, `managed_user_auto_processor_supervisor`, `auto_processor_watch_loop`, `run_auto_processor_tick`.
- [hardwired_sdna/auto_processor.json](../../../../rust-executor/src/perspectives/hardwired_sdna/auto_processor.json); SDK [core/src/perspectives/AutoProcessor.ts](../../../../core/src/perspectives/AutoProcessor.ts).
- Design: [docs/auto-processor-batch-cursor.md](../../../../docs/auto-processor-batch-cursor.md).

</details>
