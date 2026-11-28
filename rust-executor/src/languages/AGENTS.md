# languages/ — agent guide

AD4M Languages are JS bundles (built with ALDK, see `../../ad4m-ldk/`) that
implement expression/link-language adapters. The executor runs each loaded
Language in its own Deno isolate on its own OS thread. Split plan: spec item 9.

## languages/

| File | Role |
|---|---|
| `mod.rs` (2.9k, no tests) | `LanguageController` global (`LanguageController::global_instance()`; lazily self-initialises). Load/unload runtimes, `call_language` (the one script-call entry, `execute_on_language[_with_context]` for the ordered default), install from address / bundle, system languages, settings, `language_by_ref` (306 lines), expression get/create/interact, **and** neighbourhood create/get + Holochain DNA templating (both moving out) |
| `language_runtime.rs` | One runtime = thread + `JsCore::new_for_language` + request channel; the request loop (ordered calls and reads, all on one task); each call's `AgentContext` registered for its script's agent token |
| `language_runtime_handle.rs` | Cloneable handle: send JS source string, await result |
| `call_deadline_tests.rs` | Calls into one runtime through a real handle: the call deadline, send order, the agent a call signs as; `#[ignore]` benchmarks |
| `language.rs` | `Language` struct: executor-side view (name, address, adapters present) |
| `conductor_languages.rs` | `load_link_and_installed_languages`: the system-language steps whose constructors wait for the Holochain conductor (known link languages, installed languages). Run in the background after generate/unlock by `agent::conductor_startup` |
| `language_context.rs`, `capability.rs`, `literal.rs`, `byte_array.rs`, `error.rs`, `wasm_delegate.rs` | context passed at load, per-language capability grants, `Literal` codec, `LanguageError` |

A script call goes through `call_language` with a `CallKind`: **ordered**
(`CallKind::Ordered`, the default — `execute_on_language[_with_context]` wraps
it) or **concurrent** (`CallKind::Concurrent`). Ordered calls run one at a time,
in send order: commits must reach the link language in the order they were made, and a
language's own state (p-diff-sync's `myRevision` and `peers`, server-link's
cursor and `_session`, local link's `links`/`revision`) changes across their
awaits. A concurrent call starts as soon as it arrives and runs beside the
ordered call and other concurrent calls, up to `MAX_CONCURRENT_CALLS` (16) in
flight — the next ones wait for a slot without holding up ordered calls. Mark a
call concurrent only when it changes no state a later call depends on and needs
no position in the send order. Today that is: the reads `get_expression`,
`is_immutable_expression`, `peersRemote`
(`Language::others`), `getOnlineAgents` (p-diff-sync shares one lookup
between overlapping calls, so they hold one `STATUS_CONCURRENCY` of zome-call
permits), every language-language
`expressionGet`/`languageGetSource` fetch, the three icon reads,
`interactions()` (not `expressionInteract`, which can write), the identity
reads (`identityLog`, `keyOwner`, `listOffers`, `offerState`, the
`expressionGet` behind `agent_version`), and the telepresence emits
`sendSignal`/`sendBroadcast` — one message each, whose order the protocol
tolerates (MeshManager numbers offers and drops overtaken ones). Ordered stays:
`commit` and `sync` (diff order, the sync cursor), `current_revision` and
`render` (the fallback sync reads them as a fence behind an in-flight commit:
`share_links_missing_remotely` would otherwise commit its links again),
`setLocalAgents`,
`setOnlineStatus`, `expressionCreate`, `expressionInteract`, Holochain signal
delivery, identity writes (`append`, `putAgent`, `openOffer`, `claimOffer`),
language publish. Loading, callback registration and teardown run with no call
beside them. Everything runs on the runtime's one task (`process_requests`):
the V8 event loop keeps one waker.

A bundle that is wasm-bindgen output (a Rust/ALDK language) runs every call
ordered, `CallKind::Concurrent` included: `load_module` marks the handle
(`LanguageRuntimeHandle::is_wasm_bundle` — the `__wbindgen`/`__wbg_` names a
bundler cannot rename), whose `send` then writes `Ordered` into every call it
sends; the runtime only ever sees the resolved kind. wasm-bindgen-futures polls
all of a module's futures from one microtask queue that does not restore each
task's V8 async context, so a
call resumed beside another would sign as the other call's agent; and the
ALDK's async exports hold the instance's one lock across their awaits while
its sync exports (`interactions`, `expressionInteract`) answer busy when it
is held — a second call could never run beside an in-flight one correctly
anyway (`a_wasm_language_runs_concurrent_calls_in_order_as_their_agents`).

The agent a call acts as travels with the call, not with the thread: the
runtime registers it under the request id and runs the script as
`__ad4mRunCall(id, async () => (script))` (`js_core/agent_extension.js`), which
takes it out once as a `CallAgent` token and keeps it in a V8 async-context
variable that every await, promise reaction and timer of the call inherits.
The agent ops sign as that token. With no token they refuse (signing, sealed
boxes, and `did` / `signingKeyId` too: code with no token acts as no agent).
So do the `HOLOCHAIN_SERVICE` ops that touch a language's cells
(`installApp`, `getAppInfo`, `callZomeFunction`, #1775).
The host's own entries into a language run the same way, with a main-agent
token (`LanguageRuntime::execute_as_host`): the bundle's evaluation
(`JsCore::load_module_from_source` evaluates it under the token, which
`__ad4mEnterCallAgent`/`__ad4mLeaveCallAgent` enter and restore around
`mod_evaluate`), `init`, callback registration and teardown.
Timers,
sockets and promises they start inherit that token, so server-link's
WebSocket handlers and reconnects act as the main agent; Holochain signals
and syncs arrive as calls as the main agent. And a callback a language fires
from inside a call — an `emit*` import, a closure it was handed, a promise
it started — runs in that call's async context, so it acts as the invoking
call's agent. Never bring back a runtime-wide
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
server-link keeps one auth `_session` per language, and a concurrent call
(`others`) may refresh it with `auth.getValidToken()`, signing the challenge as
whichever agent that call runs for. Concurrent calls run as the main agent
today; if they ever run per agent, that session must be keyed by DID.

The runtime never abandons or interrupts a call it has started. The caller of
a running call waits until it ends: a call that keeps making progress (a file
upload with one zome call per chunk) has no total bound. Once the running
ordered call has run for `LANGUAGE_CALL_DEADLINE` (60 s), an ordered call still
queued behind it gets an error, and a new one fails at once without being
queued. A concurrent call that has not started `LANGUAGE_CALL_DEADLINE` after
it was sent (it waits behind a lifecycle request, the `MAX_CONCURRENT_CALLS`
bound, or a JS CPU loop that holds the thread) gets an error too. A request
counts as started on its future's first poll, when its script begins, not when
the loop admits it: calls admitted in one burst start one after another, so one
stuck behind another's CPU loop is still unstarted and its caller gets the
deadline error
(`a_read_admitted_beside_a_read_that_holds_the_thread_gets_an_error_at_the_deadline`).
The runtime never
starts a call whose caller has gone, so a deadline error means the call did
not run. The deadline reads the tokio clock (`tokio::time::Instant`), so a test
on a paused clock (`start_paused`) reaches it with no real wait
(`the_deadline_follows_the_paused_tokio_clock`). A test with a real runtime
thread must not pause the clock: the paused clock advances while the test waits
on the other thread, and deadlines fire early. `call_deadline_tests.rs` uses a
200 ms deadline there (`short(&handle)`, a clone of the handle with that
deadline, only for the call whose expiry the test asserts; the calls that must
start go on the default deadline) and waits for the runtime's running marker
(`ordered_call_started`, `concurrent_calls_started`) or the queue report
(`calls_waiting`) instead of sleeping until a call has probably started.
`send` inserts the queue report's entry and sends the request under one hold of
the report's lock, so a depth a reader sees counts requests already in the
channel.

A call that never returns, by kind. A hung ordered call (an `await` nothing
settles) blocks the language's ordered calls and its own caller for ever; the
callers queued behind it get the deadline error instead of waiting for ever,
and a new one fails at once. A hung concurrent call occupies one of the 16
slots — further concurrent calls queue only once all 16 are taken — and it
delays the lifecycle requests that run with no call beside them (loading,
callback registration, teardown) until it ends. No deadline bounds a running
call: the deadline only covers calls that have not started, so a hung
concurrent call's own caller waits until it ends — for ever, if it never
does. A JS CPU loop is worse: the isolate has one thread, so it blocks every
call of that language, ordered and concurrent, and the event loop itself —
a queued caller's deadline still fires and reports the error, but nothing
runs until the loop yields. When every handle to the language is gone the
channel closes and the runtime's thread ends; a call still in flight ends
with the isolate.

`get_expression` caches only what the **agent language** returns, in
`agent_profile_cache.rs` (key: language address + DID, so a changed agent language
never serves old entries): a profile 5 minutes, "no such profile" 60 seconds, never
an error, at most 10 000 entries (the constants carry the reasons). A hit costs no
language call at all (not even `isImmutableExpression`). The cache keeps the expression
unverified (`fetch_expression`): `get_expression` checks the proof on every read, a hit
too, because the verdict depends on the time (a device key can expire) and on the
identity module. Every write the executor makes drops the author's entry:
`expression_create` on the agent language (the signing DID and the content's `did`) and
`AgentLanguageBackend::put_agent` (`forget_agent_profile`), so a local profile update
reads back at once. Each drop raises the cache's generation, and a miss stores its
answer only if the generation it read before the fetch still holds: a read in flight
across a write never puts the old profile back. A profile
changed on another executor shows after the TTL, as in the SDK's own 5-minute cache.
Readers that need the live record (`agent_version`) call the language directly.
Other languages are not cached here (only immutable expressions, in the database).

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
