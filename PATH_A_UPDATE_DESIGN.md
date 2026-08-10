# Option-A merge: how the tree-aware write path lands on `create_subject`

*Design note for PR #883 (`feature/generic-extraction-tree`) after merging the
renamed #879 (`feature/generic-extraction`, post-#884). Written 2026-08-07.*

## The problem

#883 was built against the pre-#884 write path: the planner produced
`Vec<Link>` per op and `apply_extraction_ops` wrote them with `add_links` /
`remove_link`, encoding scalars itself via `value_to_literal_uri`. #884 deleted
that entire path — writes now go through `create_subject`, which reads the
class's `ad4m://constructor` + per-property `ad4m://setter` actions from the
perspective's SDNA and encodes each value through its `resolveLanguage`.

So the three #883 op kinds needed re-homing:

| op | pre-#884 | post-#884 |
| --- | --- | --- |
| Create | `add_links(instance_links(...))` | `create_subject` |
| Update | remove-then-add per predicate | **no existing API** |
| AddLinks | `add_links(...)` | `add_links(...)` (unchanged) |

## Decision 1 — ops carry *values*, not links

`InterpretationOp::{Create,Update}` now carry
`values: serde_json::Map<String, Value>` plus the class name, instead of
`Vec<Link>`. The planner cannot pre-encode a scalar any more: encoding is the
setter's job, and with `resolveLanguage: literal` the target is a **signed
expression envelope** (`literal:json:{author,timestamp,data,proof}`) that is not
byte-stable across two writes of the same value.

`AddLinks` keeps `Vec<Link>` — relation targets are instance URIs, so there is
nothing to encode.

Consequence: `Create` and `Update` differ *only* in whether the constructor
runs. That is exactly the semantic distinction #883 wanted ("update leaves the
type flag in place").

## Decision 2 — `update_subject` on `PerspectiveInstance`

There was no model-level "patch an existing instance" API. `create_subject`
can't be reused as-is: it always runs the constructor, which would re-mint the
type-flag link.

Added `PerspectiveInstance::update_subject(subject_class, expression_address,
values, batch_id, context)` — literally `create_subject` minus
`get_constructor_actions`. It resolves the same per-property setter commands,
resolves values through the same `resolve_property_value`, and executes them
through the same `execute_commands`. So per predicate the write is identical to
what `create_subject` would have done; only the class-minting half is skipped.

Rejected alternatives:

* **Hand-rolled remove-then-add links** (what #883 did). Would re-introduce the
  manual link handling #884 deleted and the manual literal encoder Nico
  specifically asked to remove. Also wrong: it would bypass `resolveLanguage`,
  so updated scalars would be encoded differently from created ones.
* **`create_subject` with a "skip constructor" flag.** More invasive to a
  widely-used API for a narrow need, and `update_subject` reads better at call
  sites.
* **`mcp::tools::subjects::set_subject_property`.** Model-level, but hand-rolls
  predicate resolution and expression creation rather than using the class's
  declared setter actions — the same problem as the first option, one layer up.

`update_subject` is generally useful beyond interpretation (it is the missing
half of `create_subject`), so it is a `pub` method rather than something private
to this module.

## Decision 3 — `strip_noop_updates` compares *decoded* values

#883 compared raw link targets per predicate. That no longer works: a signed
literal envelope embeds a timestamp, so writing the same string twice yields two
different targets and *every* update would look like a real change.

The check now maps each proposed property name to its predicate via the
`ModelShape`, reads back `(base, predicate)` links, and decodes each target with
`model_query::utils::parse_literal_value` — the canonical decoder, which also
unwraps `.data` out of a signed envelope. An update survives if any property's
decoded current value set differs from `{proposed value}`. A property whose
predicate can't be resolved from the shape is treated as "can't prove it's a
no-op", so the op survives (fail-open, never silently drops a write).

## Decision 4 — relations are always a separate `AddLinks` op

#883 folded a `Create`'s resolved relation links into that op's `links`. With
`create_subject` there is nowhere to put them (setters are for scalars), so
every proposal now emits at most two ops: a `Create`/`Update` for its scalars
and an `AddLinks` for its resolved relation refs. Ordering within the batch is
irrelevant — the whole run commits atomically, and relation predicates never
collide with scalar predicates.

Relation links are written `LinkStatus::Shared`, matching the default an SDNA
action takes when it declares no `local` flag (`execute_commands` uses
`local.unwrap_or(false)`). Relations have no setter to consult for a per-property
status; if that becomes a real requirement, an `ad4m://adder` action on the
relation property is the natural place for it.

## Decision 5 — `existing_instance_context` merges both needs

#879 had `existing_instance_identities` (dedup: `class -> [identity value]`);
#883 wanted `existing_instance_context` (prompt: `class -> [{id, title, class}]`).
The interpretation now needs both, and `model_query` already returns `id` on
every row, so one query serves both: `existing_instance_context` returns the
rich rows, with `identities_from_context` and `ids_from_context` as the two
projections. The dedup key stays whichever property the class declares as its
`identity` (`owner` is deliberately *not* an identity), and the prompt renders
that value under the LLM-facing key `"title"` — the name the system prompt and
few-shot examples use for "the human-readable handle of this instance".

## Consequence for `run_interpretation`'s return type

It still returns `Vec<(String, Vec<Link>)>` (base + links read back), so #881's
WS caller and the e2e harness are unaffected. The list is now the **touched**
bases — creates, updates, and `AddLinks` sources — de-duplicated in op order, so
an upsert shows up as a placement on the existing base.
