# rust-executor logging

Log-level policy and emoji legend for the `ad4m-executor` crate. Applies to
every `log::{trace,debug,info,warn,error}!` (and the surviving `use log::…`
shorthands) in `rust-executor/src/**`. See PR #942 for the motivating audit
and review discussion.

The crate does **not** use `tracing::*`; every log call resolves to the
`log` crate.

## Level policy (short version)

**INFO** is for events an operator or the human user cares about at glance
level:

- Heavy AI work start/end + model + latency + token counts
  (`AIService::prompt`, `AIService::embed`).
- Interpretation runs: which strategy, which model, verdict, latency
  (`run_interpretation_with_strategy_and_model`).
- Auto-processor: task pick + completion + rate
  (`auto_processor::watcher::run_one_pass`). Passive-branch outcomes
  (standing-down, awaiting-author, backed-off, empty batch) are the
  steady state of a peer that isn't the elected processor and stay at
  debug.
- Language load / holochain app install / uninstall / conductor
  start-stop / agent create-unlock-logout: one line per lifecycle event,
  not per internal stage.

**DEBUG** is for stage-level detail:

- Subscription lifecycle (created / dropped: one per client query and
  per closed socket), subscription evaluation runs, individual link diff commits, per-language
  install detail, holochain zome call boundaries, per-tool-call AI
  rounds, prompt/response bodies (truncated), auto-processor stage
  transitions.

Anything that fires on a timer or per poll, message or connection stays
off info, however short: an idle executor's info log should be quiet.

**Languages.** A language's `console.log` and `console.debug` are logged
at debug, `console.info` at info, `console.warn` and `console.error` at
warn and error (`js_core/utils_extension.js`, installed as the language
isolate's `console` by `language_bootstrap.js`), all as `[JSCORE]: ...`
under the target `rust_executor::js_core::utils_extension`. Set that
target to `debug` to see a language's chatter. Deno's own console wrote
to stdout at no level, past `RUST_LOG` and `--log-dir`.

**TRACE** is for hot-path internals:

- Prolog runtime chatter, holochain internals, deno v8 chatter, pubsub
  per-message, VAD chunks, raw `{:?}` zome-result dumps.

**WARN** is reserved for genuine anomalies. Anything that fires on an
explicitly-expected branch (e.g. `RACE CONDITION PREVENTED`, "Prolog is
DISABLED" when the operator set that mode, credit deduction after the
operation already committed) is **not** a warn — it is debug.

**ERROR** is reserved for actual failures the user or operator must know
about. Do not use error for a 30 s timeout on a periodic maintenance
call, or for a per-tick re-evaluation of a failing subscription.

## Emoji legend

Emojis are cheap semantic tags — an operator can `rg '⚙️' logs/` and get
every auto-processor pass in one grep. Use them consistently. Prefer to
lead the message with the emoji(s) so multi-tag lines start with the
most-specific tag on the left.

| Emoji | Domain |
| ----- | --- |
| 🤖 | AI (prompt/embed/model calls) |
| 🧠 | Interpretation engine |
| ⚙️ | Auto-processor |
| 🔗 | Subscriptions (live queries) |
| 🧠 🔗 | Model-based subscription branch |
| 🔎 🔗 | SPARQL subscription branch |
| 🐝 | Holochain |
| 📚 | Languages |
| 👤 | Agent |
| 🌐 | Network / websocket |
| 🔐 | Auth / capabilities |
| 💾 | Storage / db (also link-diff commits) |
| 💳 | Billing / credits |
| 📨 | Pubsub |
| 🪝 | Hooks |
| ⏱️ | Latency / perf |
| ✅ | Success |
| ⚠️ | Warn |
| ❌ | Error |
| 📡 | Signals |
| 🧩 | Expression |
| 🎧 | Audio (whisper transcription) |
| 📧 | Email |

## Format conventions

- Prefix operator-visible lifecycle events with `✅` on success and
  `❌`/`⚠️` on failure/anomaly.
- Include latency in ms and token/byte counts on any AI or interpretation
  info line. Format: `latency={ms}ms tokens_in=… tokens_out=…`.
- Never log raw user speech / verification codes / auth challenges / MCP
  capability codes. Truncate + redact. Speech transcript segments are
  debug + 40-char preview. Verification codes are debug-only and gated
  on `debug_assertions` + `AD4M_LOG_SECRETS=1`.
- Do not emit `{:?}` dumps of large structs at info level. Either build a
  compact summary line at info + a `{:?}` at trace, or push the whole
  thing to trace.
- Coarse at info, stages at debug, hot-path chatter at trace. AI, in
  particular: one info at "start", one info at "done"; every internal
  round trip, tool call, or retry lives at debug or trace.

## Call diagnostics (`[call-diag]`)

Lines that say where a call's time went (sockets, slow RPCs, broadcasts, language
queues) go through `call_diag!` (`src/call_diag.rs`): one line, the tag
`[call-diag]` first, then `event=<name>` and `key=value` fields (a value with a
space is quoted). `grep '\[call-diag\]'` lists them; `grep 'event=rpc.slow'`
one kind. The events and their levels are in the module doc on `call_diag`.
A line for every socket (`ws.open`, `ws.close`) or for the queue timer
(`language.queue`) is debug: anything that fires per connection or on a timer
stays off info. A line for a call that was slow or late (`rpc.slow`,
`ws.pong-late`, `broadcast.slow`) stays at warn. `log.filter-set` is warn and
`log.filter-reverted` is info: one line per operator action, not a timer.

## Changing the filter while running

`runtime.setLogFilter { filter, ttlSeconds? }` (operator only; SDK
`runtime.setLogFilter`) puts directives like `ws_rpc=debug,sfu=debug` over the
startup filter for `ttlSeconds` (default 1800, at most 14400); then the
startup filter is back by itself. `runtime.logFilter` reads both and the revert
time. A directive name matches whole module path segments anywhere in a target
(`ws_rpc` = `rust_executor::api::ws_rpc`); targets it does not name keep their
startup level (`src/log_filter.rs`), with two exceptions. The override is
rewritten the same way as the startup filter: unless it names `rmcp`,
`rmcp=warn` is added, so `ws_rpc=debug` is in force as `ws_rpc=debug,rmcp=warn`
even when startup had `rmcp=off`. A name matched in the middle of a path does
not apply inside `rmcp`: `session=debug` does not match
`rmcp::transport::streamable_http_server::session::local`.

## Subscription branch naming

Live queries in `perspectives/perspective_instance.rs` have two branches:

1. `execute_model_query` — model-based subscription — 🧠 🔗
2. `sparql_query` — SPARQL subscription — 🔎 🔗

Log messages should distinguish these.

### Model RPC and response-size diagnostics

`rpc.slow` stays at **warn**, but triggers when either elapsed time is **> 2 s**
(from arrival through serialization) or serialized response size is **> 1,000,000
bytes**. `reason=slow|size|slow,size` distinguishes triggers; one record is emitted
when both apply. No per-call info logging is added. `kind=response` is an ordinary
reply; `kind=initial` is a live-query opening reply. Sizes are serialized UTF-8
wire-envelope bytes, not just the embedded result JSON.

`perspective.modelQuery` and `perspective.subscribe` with `model` add `perspective`,
comma-joined `classes`, and `results` (returned instances, not totalCount;
subscriptions use the initial result). Errors may lack results. Handlers write a
request-task-local slot, collected by dispatch even when its handler is cancelled;
no slot is shared between concurrent requests or inherited by spawned tasks.

`shape` contains only limit, offset, order property/direction, scope presence
(`parent`), where property/operator names, and include/projection keys. No filter
operands, scope IDs, cursor tokens, or instance content are logged. Operand objects
are not traversed. Nested where structure stops at depth 16. `shape_hash` is a
16-hex-digit FNV-1a grouping hint over the sanitized JSON before display truncation
(not a cryptographic identity); object keys are sorted. It deliberately ignores
values and does not describe include/projection sub-query bodies.

Raw field limits in UTF-8 bytes: shape 2048, classes 512, perspective 128. Truncated
fields end in `...`; the common formatter escapes line breaks and quotes.
`Origin` is read once during upgrade, capped at 512 bytes, stored on the connection,
and included in debug `ws.open` and triggered `rpc.slow`. Missing/invalid headers
use `-`. Origin is client-supplied evidence, never identity or authorization.

The event forwarding path also measures serialized `query-subscription-update`
frames and emits size-triggered `rpc.slow` with `kind=update`, perspective,
subscription ID, connection, caller and origin. Updates are deltas, not a complete
instance result: no `results` count, class or query shape is inferred from their
payload. They have no request latency. Other event types are not measured here.
Projection missing-relation warnings include the bounded shape class; the projection
resolver has no perspective or request context, so it cannot include those IDs.
