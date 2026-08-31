# rust-executor logging

Log-level policy and emoji legend for the `ad4m-executor` crate. Applies to
every `log::{trace,debug,info,warn,error}!` (and the surviving `use log::…`
shorthands) in `rust-executor/src/**`. See `ad4m-log-audit.md` for the audit
that motivated this policy.

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
- Subscription lifecycle (created / dropped). The 60 s heartbeat that
  logs how many subscriptions are still active is **debug**, not info.

**DEBUG** is for stage-level detail:

- Subscription evaluation runs, individual link diff commits, per-language
  install detail, holochain zome call boundaries, per-tool-call AI
  rounds, prompt/response bodies (truncated), auto-processor stage
  transitions.

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
| 🔗 | Subscriptions (renamed from "Prolog subscription") |
| 🧠 🔗 | Model-based subscription branch |
| 🔎 🔗 | SPARQL subscription branch |
| 📜 🔗 | Prolog subscription branch |
| 🐝 | Holochain |
| 📚 | Languages |
| 👤 | Agent |
| 🌐 | Network / websocket |
| 🔐 | Auth / capabilities |
| 💾 | Storage / db |
| 💳 | Billing / credits |
| 📨 / 🪝 | pubsub / hooks |
| 🪝 | Hooks |
| ⏱️ | Latency / perf |
| ✅ | Success |
| ⚠️ | Warn |
| ❌ | Error |
| 📡 | Signals |
| 🧩 | Expression |
| 💾 | Storage/db (also link-diff commits) |
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

## Subscription branch naming

The subscription subsystem in
`perspectives/perspective_instance.rs` used to log every branch as "Prolog
subscription", but the dispatch has three branches:

1. `execute_model_query` — model-based subscription — 🧠 🔗
2. `sparql_query` — SPARQL subscription — 🔎 🔗
3. `prolog_query_subscription_with_context` — Prolog subscription — 📜 🔗

Log messages should distinguish these. Field / type identifiers
(`PrologSubscription`, `trigger_prolog_subscription_check`) are unchanged
in this pass to keep the diff scoped.
