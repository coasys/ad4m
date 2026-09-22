# Reply to the review of #1044 (native tool calling on `/v1/chat/completions`)

Thanks — and thanks in particular for checking the parity claims against the code
rather than against the PR body. All four notes are fixed on the branch, in four
commits, one per note. Full crate suite green (1563 passed, 40 ignored), `cargo
fmt` clean. Each behaviour fix has a test that I confirmed fails with the fix
reverted.

| | Note | Commit |
|---|---|---|
| 1 | Empty assistant/user turn reaches the provider | `c766d71ca` |
| 3 | `parameters` passed through unvalidated (+ duplicate tool names) | `66f5a7b9b` |
| 2 | Provider refusal reports as 500 | not fixed here — see below |
| 4 | `parallel_tool_calls` footnote | `734777c05` |
| — | CodeRabbit: non-object tool arguments | `55bd0dcef` |

## 1. The empty turn — confirmed, fixed in `to_turns`

Your trace holds end to end, and the last link is worth writing down because it is
the one that decides the severity: `anthropic::assistant_content` returns
`Value::String(turn.content.clone())` for a turn with no calls
(`anthropic.rs:290`), with nothing above it filtering blanks, so an empty
assistant turn reaches the wire as `{"role": "assistant", "content": ""}`. Same
for an empty user turn via `user_content`. That is a request-level refusal, not a
degraded answer.

And the reason it is *this* path is the right diagnosis: the harness writes its
own messages. An arbitrary client does not, and `content: null` with no
`tool_calls` is a legal, common placeholder — it is what an OpenAI SDK produces
when you append the assistant message before you have the reply.

Fixed where you said, in `to_turns` after the fold: a turn with no text, no
`tool_calls` and no `tool_result_for` is dropped. The exclusions are the point, so
they have their own test — a blank turn that *carries* a call, or that *answers*
one, is kept whatever its text, because removing either of those is precisely the
unbalanced conversation `structured_turns` exists to prevent.

Two decisions worth flagging:

**I did not put it in `structured_turns`.** There is a real argument for it —
`close` already applies the blank-turn rule in one special case (a turn that lost
all its calls), so the fold half-owns the rule. I left it out because the fold is
the interpretation harness's live path and the rule is about message sets the fold
has no reason to see. If a second non-harness caller ever appears, that is the
moment to move it down.

**A request whose messages all come out blank is now a 400.** It was the same
failure as yours with the same 500, and it is the one case where the caller can
see the request is empty without a provider round trip.

**One case I left alone, deliberately, and it is the weakest part of this fix:** a
`tool_result` whose content is the empty string is kept, because dropping it
breaks the pairing. Whether Anthropic accepts an empty `tool_result` content I
have not established — I have not put it to a live provider either. If it refuses,
the repair belongs in `user_content` (substituting a placeholder body), not in the
fold, since the block has to exist. Flagging it rather than guessing.

## 3. `parameters`, and the duplicate names you mentioned in passing

Both fixed. `.filter(Value::is_object)` before the `unwrap_or_else`, so
`"parameters": []`, `""` and `null` all land on the same empty object schema the
no-parameters case already gets — one rule, as you said, and no new one.

I took the duplicate-names remark as a fix too, since it is the same failure class
with the same client-side consequence: `duplicate_tool_name` names the first
repeated name and the request comes back as `invalid_request`. Refusing rather
than keeping one of them is the deliberate choice — dropping a duplicate is a
guess about which of the two the model should be able to call, and a tool that
silently is not there is worse than a request that says why.

## 2. The 500 — agreed, and not here

You are right about the mechanism and right about the consequence. `anyhow::Error`
out of `prompt_with_tools` carries no upstream status, `OpenAIError::internal` is
all the shim can honestly say, and an OpenAI SDK will then retry a request that
cannot succeed, three times, at full prompt cost, while telling the developer the
executor is broken.

`ProviderRejected(StatusCode, String)` on the provider error is the seam, and it
is a change to `providers/mod.rs` and both clients, not to this file. It wants its
own PR — I have not opened an issue for it yet.

What the two fixes above do in the meantime is take the two likeliest triggers out
of that class: a non-object `input_schema` and duplicate tool names were the most
probable ways for a caller to earn a 500, and both are now 400s named before
anything is sent. What is left in the class is genuinely provider-side — an
`input_schema` that is an object but invalid JSON Schema, a token limit, an empty
`tool_result` if that turns out to be refused. Those need the status to come back
up through the stack; there is no way to predict them here.

## 4. The footnote — added, and the reasoning is yours

Added to the module header, next to the `tool_choice` one. Your reason for wanting
it is the one I'd have got wrong: the footnote is not for the reader who wonders
whether `parallel_tool_calls` works, it is for the reader who checks `tool_choice`
against the doc, finds it accounted for, and reads the silence as coverage. It
also says what wiring either one through would actually mean — a provider-level
request field — so the next person starts from the right place.

## CodeRabbit's note: non-object tool arguments

Same underlying fact, and it belonged in the same pass: `"[]"`, `"1"` and `"null"`
parse fine and are not objects, and a `tool_use` input has to be an object.

I did **not** take the suggested fix of returning an error, because `to_provider_call`
is on the harness's path too, and the module's own doc says why: by the time that
fold runs the call has already been dispatched and its `tool_result` is waiting to
name it, so erroring (or dropping) turns a malformed argument list into a refused
conversation. They go under `_raw` with the strings that did not parse at all —
same reasoning, same representation, and the model can see what went wrong and
retry. The doc above the function now covers both cases instead of just the
unparseable one.

---

On the approval itself: the `usage_of` reading you confirmed is the one I was
least sure of and it is load-bearing for anything that bills off `prompt_tokens`,
so thank you for checking it against the provider's own semantics rather than the
comment.
