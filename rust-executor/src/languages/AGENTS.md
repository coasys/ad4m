# languages/ — agent guide

AD4M Languages are JS bundles (built with ALDK, see `../../ad4m-ldk/`) that
implement expression/link-language adapters. The executor runs each loaded
Language in its own Deno isolate on its own OS thread. Split plan: spec item 9.

## languages/

| File | Role |
|---|---|
| `mod.rs` (2.9k, no tests) | `LanguageController` global (`LanguageController::global_instance()`; lazily self-initialises). Load/unload runtimes, `execute_on_language[_with_context]`, install from address / bundle, system languages, settings, `language_by_ref` (306 lines), expression get/create/interact, **and** neighbourhood create/get + Holochain DNA templating (both moving out) |
| `language_runtime.rs` | One runtime = thread + `JsCore::new_for_language` + request channel; the request loop (ordered calls and reads, all on one task); each call's `AgentContext` registered for its script's agent token |
| `language_runtime_handle.rs` | Cloneable handle: send JS source string, await result |
| `call_deadline_tests.rs` | Calls into one runtime through a real handle: the call deadline, send order, the agent a call signs as; `#[ignore]` benchmarks |
| `language.rs` | `Language` struct: executor-side view (name, address, adapters present) |
| `conductor_languages.rs` | `load_link_and_installed_languages`: the system-language steps whose constructors wait for the Holochain conductor (known link languages, installed languages). Run in the background after generate/unlock by `agent::conductor_startup` |
| `language_context.rs`, `capability.rs`, `literal.rs`, `byte_array.rs`, `error.rs`, `wasm_delegate.rs` | context passed at load, per-language capability grants, `Literal` codec, `LanguageError` |

A script call is **ordered** (`CallKind::Ordered`, the default:
`execute_on_language[_with_context]`) or a **read** (`CallKind::Read`:
`execute_read_on_language`). Ordered calls run one at a time, in send order:
commits must reach the link language in the order they were made, and a
language's own state (p-diff-sync's `myRevision`, server-link's `configured`)
changes across their awaits. A read starts as soon as it arrives and runs
beside the ordered call and other reads, so a slow commit no longer delays an
expression get. Mark a call a read only when it changes no state a later call
depends on; today that is `get_expression`, `is_immutable_expression`,
`peersRemote` (`Language::others`) and `getOnlineAgents`. Loading, callback
registration and teardown run with no call beside them. Everything runs on the
runtime's one task (`process_requests`): the V8 event loop keeps one waker.

The agent a call acts as travels with the call, not with the thread: the
runtime registers it under the request id and runs the script as
`__ad4mRunCall(id, async () => (script))` (`js_core/agent_extension.js`), which
takes it out once as a `CallAgent` token and keeps it in a V8 async-context
variable that every await, promise reaction and timer of the call inherits.
The agent ops sign as that token. With no token they refuse (signing, sealed
boxes, and `did` / `signingKeyId` too: code with no token acts as no agent).
The host's own entries into a language run the same way, with a main-agent
token (`LanguageRuntime::execute_as_host`): the bundle's evaluation (the
module is loaded unevaluated and the capture script's `import()` evaluates it
under the token), `init`, callback registration and teardown. Timers,
sockets and promises they start inherit that token, so server-link's
WebSocket handlers and reconnects act as the main agent; Holochain signals
and syncs arrive as calls as the main agent. Never bring back a runtime-wide
"current agent": a call that resumes beside another would sign as the other
call's agent (`overlapping_calls_each_act_as_their_own_agent`), and never let
a missing token mean the main agent: code in a managed user's call would
clear its async context and sign as the node
(`an_agent_op_without_the_call_token_acts_as_no_agent`).

What this does not enforce: a token is a JS value, and language code can keep
it. `Deno.core` is gone from user code, but `Deno[Deno.internal].core` is not,
so language code reaches `getAsyncContext` / `setAsyncContext` (the imported
`agent_*` ops themselves are removed from `core.ops`). Code that saves the
async context during
Bob's call and restores it in a later call for Alice signs as Bob, and code
that keeps the context of a host entry signs as the main agent in any later
call. The runtime only stops code from gaining an agent it was never handed.
Separating agents from each other needs per-agent isolation (one isolate per
agent, SEC-4). Language state shared across calls has the same limit:
server-link keeps one auth `_session` per language, and a read (`others`) may
refresh it with `auth.getValidToken()`, signing the challenge as whichever
agent that read runs for. Reads run as the main agent today; if reads ever run
per agent, that session must be keyed by DID.

The runtime never abandons or interrupts a call it has started. The caller of
a running call waits until it ends: a call that keeps making progress (a file
upload with one zome call per chunk) has no total bound. Once the running
ordered call has run for `LANGUAGE_CALL_DEADLINE` (60 s), an ordered call still
queued behind it gets an error, and a new one fails at once without being
queued. A read that has not started `LANGUAGE_CALL_DEADLINE` after it was sent
(it waits only behind a lifecycle request, or a JS CPU loop that holds the
thread) gets an error too. A request counts as started on its future's first
poll, when its script begins, not when the loop admits it: reads admitted in
one burst start one after another, so a read stuck behind another read's CPU
loop is still unstarted and its caller gets the deadline error
(`a_read_admitted_beside_a_read_that_holds_the_thread_gets_an_error_at_the_deadline`).
The runtime never
starts a call whose caller has gone, so a deadline error means the call did
not run. A call that never returns (a hung `await` or a JS CPU loop) still
blocks the ordered calls of its language and its own caller, but the callers
behind it get errors instead of waiting for ever. A JS CPU loop blocks reads
too: the isolate has one thread. Loading and teardown wait as before.

Calls into a Language are built as JS source via `format!` and evaluated (≈20
sites in `mod.rs`). A malformed template used to deadlock that language forever
(`js_core/mod.rs` comments). Keep templates small and escape inputs with
`serde_json::to_string`.

The Deno isolate machinery (worker construction, `ad4m:host` shim, `#[op2]`
extensions, snapshot) lives in `js_core/` — read
[`../js_core/AGENTS.md`](../js_core/AGENTS.md) as well; do not duplicate its
file table here.

Obsolete: the "Phase 1" hybrid where JS owned the language controller. There is no
`core.languageController` any more; Rust owns everything, JS only runs bundles.
