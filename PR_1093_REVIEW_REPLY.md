# Reply to the review of #1093 (bounded traversal)

Thanks — this is the review I wanted. All three findings are real, I reproduced
each one against the branch before touching anything, and **#1095 is merged into
`feat/bounded-traversal`** (`2e595bcc9`). Two follow-up commits on top of it close
gaps the fixes left behind rather than changing what they do.

## The three findings

**1. `limitPerAnchor` never arriving — you're right, and it's worse than a missing
attribute.** I traced the same path and it is exactly as you describe:
`prepareModelQueryParams` spreads the scope verbatim (`{ ...query.parent,
predicate }` in `Ad4mModel.ts`), so the camelCase spelling goes straight onto the
wire. What makes the fix obviously correct rather than merely effective is that
`Scope` was the *only* type in `model_query/types.rs` without `rename_all =
"camelCase"` — `ModelQueryInput`, `ProjectionInput` and `ModelQueryResult` all
carry it. So this isn't a patch over the feature, it is the file's own convention
finally reaching the one type that had never needed it. Every prior `Scope` field
being a single word is the whole reason the gap stayed invisible.

I checked the serialize direction too, since `rename_all` is symmetric and the
enum derives `Serialize`: the one external serializer,
`interpretation/graph/read.rs:295`, writes a `Scope` into a query JSON that is
deserialized in the same binary, so the round trip moves together. Nothing else
reads the field names.

This one deserves a line in the PR body's follow-ups rather than just a fix,
because it sharpens the point I made there about `ProjectionInput` against an old
executor: I flagged the silent-drop failure class as a *future* hazard and had
already shipped an instance of it. The lesson is the untagged enum, not the
spelling — an untagged variant will accept a payload that means something other
than what the caller wrote, and stay quiet about it.

**2. The global window composing with the slice — agreed in both directions.**
The pushable half I'd have found eventually; the non-pushable half I would not
have, because `sparql_pagination.is_some()` meaning "the store already truncated
this" was true right up until this branch made the scope force the phases on its
own account, and I changed the premise without changing the reader. Splitting out
`window_pushed` is the right repair, and the property that makes it safe to merge
is the one you named: when `scope_needs_phases` is false, `push_window ==
can_push_pagination` exactly, so every existing query is byte-identical.

**On the semantics choice you flagged — keep it, don't swap it for a refusal.**
"Slice per anchor, then one window over the ordered union" is the only reading
under which the two features compose, and refusing the combination would make the
feature unusable for the case it exists for: a paged thread is a per-anchor slice
*and* a global page, and WE asks for both in the same query. The refusals are for
combinations where one option contradicts another; these two answer different
questions and stack cleanly.

I did verify the placement rather than take it on trust — the executor-side window
lands after the post-hydration where-filter, which is what makes the non-pushable
case correct rather than merely non-empty.

**3. Unenforced mutual exclusions — agreed, and the diagnosis generalises.** The
docs said "not combinable" and the code quietly picked a winner, which is the
failure mode this PR refuses everywhere else. Refusing is right.

## What I added on top

**`docs`: the documentation still described a refusal as advice.** After #1095,
`levels` + `transitive` and `levels` + `limitPerAnchor` are errors, but the doc
comments on both sides still read "Not combinable with `transitive`" and "Nor is
`limitPerAnchor` a substitute" — guidance a caller can weigh and decline. A
caller who reads that and writes both now gets an exception, which is the right
behaviour reached by a surprising route. The `transitive` and `limitPerAnchor`
docs also said nothing about `levels` at all, so the refusal was only discoverable
from the side that triggers it. Both now say "refused", from both ends, and the
`limitPerAnchor` doc picks up the window ordering as a property a caller can rely
on rather than an implementation note.

**`test`: seeding `seen` with the roots changed a semantic, and only half of it is
pinned.** Your cycle test covers a graph that loops back to the anchor. It does
not cover the other way an anchor gets reached again, which needs no cycle at all:
a caller naming two anchors where one sits below the other — "refresh these two
branches" — which is the spelling someone reaches by accident rather than by
constructing a cycle. Before the seeding, that anchor came back as the other's
child *and* had its subtree walked twice, once at each depth. After it, the anchor
is reported nowhere and its subtree is walked once, from itself.

That is the right answer and it matches `transitive`'s documented "excludes the
anchor itself" — an anchor is where the walk started, not something the walk
found — but it was behaviour nothing stated and nothing covered. Now it is both:
a test over `comment_tree_store` with anchors `[root, c1]`, and a line in the
`levels` doc saying the anchors are excluded from their own result whether the
walk reaches them through a cycle or through the caller's own overlapping list.

## The builder observation

Agreed and deliberately left out. A fluent `.traverse()` is not a thin wrapper
over `.parent()` — it has to decide how multi-anchor, `levels` and the two refused
combinations read as a chain, and whether the refusals become type errors at the
call site or stay runtime errors from the executor. That's a small design worth
doing properly and separately, on a PR that isn't already 1,485 lines. The
object-form query is not a workaround in the meantime; it's the form the feature
was designed against.

## Verification

Run on the merged tree, not taken from either PR body:

- `cargo test --release -p ad4m-executor --lib -- --test-threads=1 model_query` —
  **336 passed, 0 failed** (your 335, plus the multi-anchor overlap test)
- `npx jest src/model` in `core/` — **287 passed**
- `cargo fmt --all -- --check` — clean

Unchanged from the base PR: `pnpm run test-main`'s other five suites are still not
run, and the four `test-model` failures are still the `gemma3:12b` / `gemma3:4b`
mismatch on this machine rather than anything in the branch.

## Still open, unchanged

The follow-ups in the PR body stand, with #1 now reading differently:

- **`ProjectionInput.transitive` against an old executor** silently becomes
  direct-child counts. Same failure class as finding 1, and finding 1 is the
  argument for taking it seriously rather than filing it.
- **`total_count` counts all scope matches**, ignoring the per-anchor limit. Your
  reading is mine: it answers "how many exist", not "how many you got". Leaving it.
- **Ordering is required for a meaningful per-anchor limit**, still not enforced,
  since "any N" is a legitimate ask.
- **Per-group limits pushed further down**, if Oxigraph ever grows the extension
  or a profile shows the id over-fetch. Still not measured under a pathologically
  wide parent.
