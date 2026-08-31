# Flow Interpretation Hints — Design Doc

*Data — 2026-08-17 v1; 2026-08-18 v2 after Coasys planning meeting; 2026-08-18 v3 after model_query reframe + processing-model + role-consensus refinements. Branch: `feature/flow-interpretation-hints` — schema-field commit `2ff0752f1` already landed.*

**Change log:**
- **v1 (2026-08-17):** Core design settled — SPARQL guardQuery, proposal-only LLM output, per-flow AutoProcessor, multi-agent consensus in v1
- **v2 (2026-08-18):** Added `FlowInstance` wrapper class, subscription API, overlay-exclusion, evidence-hashing, north-star demo
- **v3 (2026-08-18):** Replaced `guardQuery: SPARQL` with `requires: ModelQuery` (reuses Ad4mModel query mechanism, no new DSL). Replaced "one-flow-one-AutoProcessor" with **channel-scoped, flow-aware extraction + reactive evaluation** (Model C — three alternatives named in §5). Extended `consensusRule` with ontology-bootstrapped role definitions — all role expressions collapse to a model_query, so new role types don't need code changes. Design principle added: **v1 must allow experimentation via UI/config, not code changes.**
- **v3.1 (2026-08-19, Nico + James review):** `requires` is a `ModelQuery[]` (array) with AND semantics across entries. Retires the §11 "compound `requires`" open item — "one supporting AND one opposing" is now just two array entries. Worked examples rewritten to array shape.
- **v4 (2026-08-19, Nico + James live-review continued):** Flow-level typed I/O. `SHACLFlow` gains `inputTypes: string[]` (subject-class URIs the flow accepts as base) and `outputTypes: string[]` (classes that must exist for completion). `flowable` retired — `inputTypes` replaces it. **Actions unify with flows:** a zero-state flow with just inputTypes + outputTypes + a `consensusRule` is a first-class "action" (e.g. Like). New §6.3 worked example. §1 gains the group-mind framing (representation the group processes together). Speech-detection stays subsumed by `semanticCheck` — no new field. UI templates deliberately NOT referenced here — they get their own doc later.
- **v5 (2026-08-19, Nico follow-up post PR #902):** Adds §8 "Available actions — the read side of a flow." This closes the missing productisation half of v4: the same "what can be done here?" query drives both **UI affordances** (buttons, menus, drag-targets) and **LLM speech-detection** during a call — one deterministic surface, two consumers. `flow.availableActions(item, agent?)` spelled out with query shape + UI examples + LLM extraction-prompt integration + how zero-state flows collapse into buttons. Sections 8-12 renumbered to 9-13.

## 1. Context & goal

We shipped a generic **interpretation engine** for subject classes (PRs #879 / #883 / #881 / #885): every SHACL class carrying an English `extractionHint` can be materialized from conversation transcripts by the LLM, written via `create_subject`, and audited via `InterpretationRun` / `InterpretationOverlay`.

The natural next primitive is the **flow**. A flow is **typed processing**: it declares the inputs it accepts, the outputs it produces, and (optionally) the states it passes through in between. It attaches to a base expression — a Belief, Intention, Plan, Proposal, Task, or any node in a **state-of-affairs tree** — and drives that base from an initial input toward a completion condition. As people talk in a meeting, the transcript should not only mint new instances but also **advance the flows that are already in play**. Same input, same engine, extra output surface.

This unlocks the "cybernetic team system just with text" vision: user-defined ontologies + user-defined flows + English hints = an AI-assisted process layer. Flows + ontologies = **social DNA modules** — shareable templates that make a community's coordination legible to AI without any code.

### 1.1 Why this matters — the group-mind framing

The deeper reason to build this: what living systems and intelligence systems do is *represent the world with internal types, process those representations together, and act on an updated shared model*. That is what individual cognition does, that is what teams do when they work well, and that is what a global collective intelligence would have to do to counter centralised AI.

Classes give the group a typed vocabulary for what exists. Flows give the group a typed vocabulary for how it processes that world together — what inputs move through what stages toward what outputs, and how consensus is reached along the way. The graph of instances is the shared model. Everything the LLM does (extraction, overlay, flow advancement) is that group-mind updating itself against what participants are saying. This is what makes flows more than a workflow engine: they are the mechanism by which a distributed group's shared representation actually changes state.

Coasys itself will be the first user: the SoA tree the team started on Miro on 2026-08-18 will be dogfooded inside the new app. That is the acceptance bar for v1.

## 2. What SHACLFlow already gives us

Post-Prolog-migration state (`core/src/shacl/SHACLFlow.ts`):

- **`FlowState`** = `{ name, value: number, stateCheck: LinkPattern, interpretationHint? }` — currently a simple link check.
- **`FlowTransition`** = `{ actionName, fromState, toState, actions: AD4MAction[] }` — explicitly declared; actions flip state markers.
- **`flowable: "any" | LinkPattern`** — restricts which expressions can start this flow.
- **`startAction: AD4MAction[]`** — mints the initial state marker.
- Serializes to plain AD4M links.

Consumer API today lives on `PerspectiveProxy` as free functions (`addFlow`, `getFlow`, `startFlow`, `flowState`, `runFlowAction`, etc.). No wrapper class for a *running instance* exists. §4.3 fixes that.

## 3. Design principles (settled)

1. **Scope = base item + everything graph-dangling from it.** No new `scope` field. AD4M is subject-oriented — many things can coexist on the same base URI.
2. **Guards talk about models, not raw graph.** `requires: ModelQuery[]` (reuse Ad4mModel query mechanism, array with AND semantics). Zero new DSL.
3. **Transitions explicit.** Back-edges must be declared.
4. **Two-layer safety.** LLM proposes only. Engine deterministically evaluates `requires` + consensus.
5. **Overlays don't count as evidence.** Only committed graph state satisfies `requires`.
6. **Evidence integrity via content-hashing.** Proposals snapshot content-hashes; consensus firing re-verifies. Synergy-Fuel-ready.
7. **Multi-agent from v1** via configurable `consensusRule` (with optional role restriction — all roles reduce to model_query, so no code changes needed to define new roles).
8. **Channel-scoped extraction, flow-aware.** ONE LLM pass per channel-scope transcript batch, prompt includes summaries of active flows in scope. Not per-flow. See §5 alternatives A/B/C.
9. **Reuse everything** — `InterpretationRun`, `InterpretationOverlay`, `AutoProcessorConfig`, `ProcessingClaim`, `create_subject`, `runInterpretation`, `model_query`.
10. **Config over code.** V1 must let humans experiment with new flows, ontologies, roles, and hints without touching Rust — everything expressed in the graph as data.

## 4. Schema additions

### 4.1 SHACLFlow / FlowState — new fields

```typescript
export interface FlowState {
  name: string;
  value: number;
  requires?: ModelQuery[];       // NEW — replaces stateCheck+guardQuery; array of queries, ALL must match (AND across the array); each query's matches become evidence
  interpretationHint?: string;   // ALREADY LANDED (2ff0752f1)
  consensusRule?: ConsensusRule; // NEW — see §7
  semanticCheck?: string;        // NEW — optional English hint for a targeted LLM confirmation (§5)
}

export type ModelQuery = {
  className: string;             // subject class to search for
  where?: { [prop: string]: PropertyCondition | string };
  count?: { min?: number; max?: number };  // default { min: 1 }
  linkedTo?: "flow" | "base" | { via: string; to: "flow" | "base" };
  // Template variables allowed inside where values:
  //   "$flow.base", "$flow.uri", "$did" (for role checks)
};

export type PropertyCondition =
  | string | number | boolean
  | { equals: any } | { in: any[] } | { exists: true } | { matches: string };

export class SHACLFlow {
  // ...existing...
  public inputTypes: string[];          // NEW — subject-class URIs the flow accepts as its base (replaces `flowable`)
  public outputTypes: string[];         // NEW — subject-class URIs the flow must produce (at least one instance of each, linked to the base) to be "done"
  public interpretationHint?: string;   // ALREADY LANDED — top-level frame
  public creationHint?: string;         // NEW — how to recognize we should spawn a new instance
  public context?: ModelQuery[];        // NEW — extra queries pulled into LLM prompt as BACKGROUND (not evidence)
  public consensusRule?: ConsensusRule; // NEW — top-level consensus for flows without any FlowStates (see §7)
}
```

**`flowable` is retired** — `inputTypes` replaces it. Same `stateCheck` treatment: nothing in production consumed it, so we drop cleanly rather than carry back-compat.

### 4.1.1 Typed I/O + zero-state flows: actions unified

The flow-level `inputTypes` / `outputTypes` do two jobs at once:

- **Contract:** a UI or an agent inspecting the perspective can see *what a flow accepts and produces* without walking its state machine — enough to render the right controls, or to auto-wire outputs of one flow into inputs of another.
- **Terminal condition for stateless flows:** a flow with **zero `FlowState`s** completes when (a) at least one instance of each declared `outputType` exists in scope and is linked to the base expression, AND (b) the flow-level `consensusRule` is satisfied. This makes every "action" a first-class flow: a Like is a flow with `inputTypes: ["Post"]`, `outputTypes: ["Like"]`, `consensusRule: {n: 1}`, no states. Every button in the UI is a zero-state flow.

Flows with `FlowState`s still work exactly as v3.1 described — the state machine is the *processing between* input and output. `outputTypes` in that case are documentation of what a completed run leaves in the graph; the completion signal is the terminal state's transition firing (same as v3.1).

**`stateCheck: LinkPattern`** — removed. Nothing in production depends on it; the existing TODO test flow is migrated in the schema PR.

Serialization predicates: `ad4m://requires` (JSON literal), `ad4m://consensusRule` (JSON literal), `ad4m://creationHint`, `ad4m://context` (JSON literal), `ad4m://semanticCheck`.

### 4.2 New @Model class — `FlowTransitionProposal`

Analogous to `InterpretationOverlay` but for state transitions.

```typescript
@Subject
export class FlowTransitionProposal {
  @Model
  static className = "ad4m://FlowTransitionProposal";

  @Property({ resolveLanguage: "literal" }) flowInstance: string;
  @Property({ resolveLanguage: "literal" }) fromState: string;
  @Property({ resolveLanguage: "literal" }) toState: string;
  @Property({ resolveLanguage: "literal" }) proposer: string;       // DID
  @HasMany() evidence: string[];                                    // URIs of instances that satisfy `requires`
  @Property({ resolveLanguage: "literal" }) evidenceHashes: string; // JSON: {[uri]: contentHash}
  @Property({ resolveLanguage: "literal" }) runUri?: string;
  @Property({ resolveLanguage: "literal" }) rationale?: string;
  @Property({ resolveLanguage: "literal" }) createdAt: string;
}
```

**Evidence-hash spec:** for each cited URI, canonicalize its graph-visible properties (sorted by property URI, multi-values sorted lexicographically) → SHA-256. Stored as JSON blob.

**Acceptance links** (multiple agents can accept):
- Predicate: `ad4m://acceptedBy`
- Source: proposal URI
- Target: DID of accepting agent

**Rejection** = delete the proposal.

### 4.3 `FlowInstance` runtime wrapper class

Ad4mModel-style wrapper for a running flow. Returned by `perspective.startFlow(shape, base)` and `perspective.getFlowInstance(shape, base)`.

```typescript
export class FlowInstance {
  readonly shape: SHACLFlow;
  readonly baseExpression: string;
  readonly uri: string;

  get currentState(): Promise<FlowState>;
  get availableTransitions(): Promise<FlowTransition[]>;
  get proposals(): Promise<FlowTransitionProposal[]>;
  get history(): Promise<{ state: string; at: string }[]>;
  get evidence(): Promise<{ [stateName: string]: Instance[] }>; // via model_query on each state's `requires`

  proposeTransition(toState: string, evidence: string[], rationale?: string): Promise<FlowTransitionProposal>;
  accept(proposalUri: string): Promise<void>;
  reject(proposalUri: string): Promise<void>;
  fireAction(actionName: string): Promise<void>;

  onStateChange(handler: (newState: FlowState, oldState: FlowState) => void): Unsubscribe;
  onProposalAdded(handler: (p: FlowTransitionProposal) => void): Unsubscribe;
  onProposalResolved(handler: (p: FlowTransitionProposal, outcome: "fired" | "rejected") => void): Unsubscribe;
}
```

Dynamic per-transition methods (`flow.identifyTension()`) → v1.5.

## 5. Runtime — three processing models

The design has to decide: where does extraction run, and how flow-aware is it? Three alternatives, presented so James + Josh can push back before v1 lands.

### 5.1 Model A — Pure graph-reactive

- Extraction pass runs at channel/neighborhood level, mints instances, has zero flow-awareness in its prompt
- Flows only react to graph state via `model_query` post-extraction
- **Pros:** cheapest — 1 LLM call per transcript batch. Cleanest separation. Flows are pure functions of graph.
- **Cons:** LLM has no idea what flows are looking for — might mint a `Perspective(position="neutral")` when a flow-aware LLM would have caught subtle opposition. Interpretation hints on flow states become dead code.

### 5.2 Model B — Per-flow processing

- Every active flow gets its own AutoProcessor + LLM session on the transcript
- **Pros:** flow-focused prompts, high proposal quality
- **Cons:** N flows × M transcripts = N × M LLM calls, redundant minting, coordination overhead. Very costly at Coasys-standup scale where a single call touches many flows.

### 5.3 Model C — Flow-aware extraction + reactive evaluation (RECOMMENDED)

- **ONE extraction pass per channel-scope transcript batch.** Prompt includes:
  - Active subject classes + their extractionHints (existing)
  - **Summary of active flows in scope: each flow's current state + reachable next-states' `requires` (in human-readable form) + `interpretationHint` for each next-state**
  - Recent transcript
  - Existing evidence in scope (base graph)
  - Pending overlays (labelled "don't cite as evidence")
- LLM output: instances to mint + optionally one or more `FlowTransitionProposal`s + minted instances that satisfy any of the summarised `requires`
- **Post-extraction, deterministic engine step:** for each flow in scope, run `requires` model_query per reachable next-state → on newly-satisfied requires, either fire (if `consensusRule = {n:1}` and the LLM's DID counts) or emit a `FlowTransitionProposal` for consensus to gather
- **Optional per-state `semanticCheck`:** if set, a targeted second LLM pass fires the moment structural requires just became satisfied ("given this specific evidence, does the interpretation hint really hold?"). Cheap (small prompt). Blocks proposal firing if the LLM says no.

**Why C wins:**
- Flow-aware extraction: hints are used, quality improves without extra passes
- Cost scales with transcripts, not with flow count
- Deterministic guard evaluation via model_query = safe + auditable
- Reactive: if extraction missed something and it gets added later, flows re-check via graph subscription
- Semantic nuance available where needed, opt-in

### 5.4 The channel-scoped pass, step by step

Given a channel `C` in a neighborhood, whose graph scope reaches flow instances `{F₁, F₂, …}`:

1. **Trigger:** new transcript content in `C` (or scope-graph change). AutoProcessor re-fires after debounce.
2. **Coordinate:** `ProcessingClaim` election — exactly one peer runs this pass.
3. **Gather context:**
   - Active subject classes in `C`'s scope + their extractionHints
   - For each active flow `Fᵢ`: currentState + reachable-next-states summary
   - Existing evidence in scope (base graph model instances)
   - Pending overlays (marked "pending, don't cite")
   - Optional: results of each flow's `context` queries
   - Unprocessed transcript turns
4. **LLM call** with the composite prompt.
5. **Post-processing (engine, deterministic):**
   - Mint new class instances via `create_subject`
   - For each flow `Fᵢ`: run each reachable next-state's `requires` model_query
     - If satisfied and LLM emitted a proposal → hash evidence, store the proposal
     - If satisfied and no LLM proposal → engine emits the proposal on behalf of the extraction-LLM DID
   - If any state has `semanticCheck` → targeted small LLM call; on "no" the proposal is discarded
6. **Consensus engine (independent loop):** for each live proposal on `(flow, toState)`, count distinct qualifying DIDs → if consensusRule met → re-verify all evidence hashes → run transition actions → delete resolved proposals → emit `flow-state-changed`.
7. **Record:** `InterpretationRun` with turn-hash for dedup.

### 5.5 High-level diagram

```
┌───────────────────────────────────────────────────────────────┐
│                     PERSPECTIVE (graph)                       │
│   ontology classes • instances • flows • proposals • overlays │
└─────────────────────────┬─────────────────────────────────────┘
                          │  scope change / new transcript
                          ▼
              ┌─────────────────────────┐
              │  Channel AutoProcessor  │
              │  ─ debounce             │
              │  ─ ProcessingClaim      │
              └───────────┬─────────────┘
                          │
                          ▼
              ┌─────────────────────────┐
              │  Context gather         │
              │   • active classes      │
              │   • active flows +      │
              │     current state +     │
              │     next-state hints    │
              │     + requires (human   │
              │     readable summary)   │
              │   • in-scope instances  │
              │   • context queries     │
              │   • pending overlays    │
              │     (marked "pending")  │
              │   • transcript          │
              └───────────┬─────────────┘
                          │
                          ▼
              ┌─────────────────────────┐
              │       LLM pass          │
              │  outputs:               │
              │   1. instances to mint  │
              │   2. (optional) transi- │
              │      tion proposals    │
              └───────────┬─────────────┘
                          │
                          ▼
              ┌─────────────────────────┐
              │ Deterministic post      │
              │  • mint via create_     │
              │    subject             │
              │  • for each flow×next- │
              │    state:               │
              │      run model_query   │
              │      on `requires`     │
              │      → if satisfied &  │
              │      no LLM proposal:  │
              │      engine emits one  │
              │  • semanticCheck LLM   │
              │    (only when triggered)│
              │  • content-hash        │
              │    evidence            │
              └───────────┬─────────────┘
                          │
                          ▼
              ┌─────────────────────────┐
              │   Consensus engine      │
              │  per proposal:          │
              │   • count qualifying    │
              │     DIDs (§7 role-      │
              │     restricted or not)  │
              │   • if ≥ n:             │
              │     re-verify hashes    │
              │     → run actions       │
              │     → emit flow-state-  │
              │       changed          │
              └───────────┬─────────────┘
                          │
                          ▼
                (GraphQL subscriptions)
                 UI, other flows, bots
```

## 6. Three worked examples

### 6.1 Deliberation Flow (on Proposal)

```typescript
const delib = new SHACLFlow("Deliberation", "coasys://");
delib.interpretationHint = "Tracks a group deliberation from an initial proposal to a shared understanding.";
delib.inputTypes = ["coasys://Proposal"];
delib.outputTypes = ["coasys://Resolution"];

delib.addState({
  name: "collectingPerspectives", value: 0.25,
  requires: [
    {
      className: "coasys://Perspective",
      linkedTo: "flow",
      count: { min: 1 },
    },
  ],
  interpretationHint: "Participants are voicing their own views; disagreement not yet surfaced.",
});

delib.addState({
  name: "tensionIdentified", value: 0.5,
  requires: [
    // AND across the array: at least one supporting Perspective AND at least one opposing.
    // No compound-query DSL needed — two independent queries do the job.
    { className: "coasys://Perspective", linkedTo: "flow", where: { position: "supports" } },
    { className: "coasys://Perspective", linkedTo: "flow", where: { position: "opposes" } },
  ],
  interpretationHint: "Two or more participants have voiced conflicting positions.",
  semanticCheck: "Confirm the positions actually conflict — not just two people speaking on the same side.",
  consensusRule: { n: 1 },
});

delib.addState({
  name: "resolved", value: 1,
  requires: [
    {
      className: "coasys://Resolution",
      linkedTo: "flow",
      count: { min: 1 },
    },
  ],
  interpretationHint: "A shared understanding or decision has been articulated by the group.",
  consensusRule: { n: 2 },  // two distinct agents must agree
});

// Transitions declared explicitly, back-edge included
delib.addTransition({ actionName: "IdentifyTension", fromState: "collectingPerspectives", toState: "tensionIdentified", actions: [/*flip state marker*/] });
delib.addTransition({ actionName: "ReopenAsTension", fromState: "resolved", toState: "tensionIdentified", actions: [/*flip state marker*/] });
```

### 6.2 Delivery Flow (on Task)

```typescript
const delivery = new SHACLFlow("Delivery", "coasys://");
delivery.interpretationHint = "Tracks a piece of work from identification to done.";
delivery.creationHint = "Spawn when someone commits to a concrete, actionable task.";
delivery.inputTypes = ["coasys://Task"];
delivery.outputTypes = ["coasys://Delivery"];

delivery.addState({ name: "scoped", value: 0.25,
  requires: [{ className: "coasys://Scope", where: { forTask: "$flow.base" } }],
  interpretationHint: "The what/why/acceptance-criteria have been articulated." });

delivery.addState({ name: "inProgress", value: 0.5,
  requires: [{ className: "coasys://Intention", where: { forTask: "$flow.base", active: true } }],
  interpretationHint: "Someone has committed to actively working on it." });

delivery.addState({ name: "done", value: 1,
  requires: [{ className: "coasys://Delivery", where: { forTask: "$flow.base", accepted: true } }],
  interpretationHint: "Reviewer has accepted the deliverable.",
  consensusRule: {
    n: 1,
    fromRole: {
      className: "coasys://Reviewer",
      where: { forTask: "$flow.base" }
    }
  }
});
```

The last one: only a DID that has been designated `Reviewer` for this Task can advance the flow to `done`. Assigning a reviewer = minting a `Reviewer` instance (via UI, via the LLM, via manual API). Config-driven; no code change to add a new role type.

### 6.3 Like Action Flow — zero states (actions unified)

The smallest possible flow. No `FlowState`s, no `FlowTransition`s: just typed I/O and a consensus rule. Demonstrates the actions-are-flows unification from §4.1.1.

```typescript
const like = new SHACLFlow("Like", "we://");
like.interpretationHint = "One participant expresses approval of a Post.";
like.inputTypes = ["we://Post"];
like.outputTypes = ["we://Like"];
like.consensusRule = { n: 1 };
// No states, no transitions.
```

**Runtime behaviour:** the flow completes on a given Post the moment (a) at least one `we://Like` instance exists linked to that Post AND (b) `consensusRule.n = 1` distinct DID has been recorded for the completion (proposer or acceptor).

**How that happens in practice:** the interpretation engine — either during a channel-scoped extraction pass or triggered directly by a UI click — mints a `Like { author: <did>, forPost: <post-uri> }` instance. The Phase 2 batch commits it. The flow-mode auto-processor sees the new instance, checks `outputTypes` against the base's linked-graph, sees the `Like` is there, checks `consensusRule` (one distinct DID satisfied by whoever authored the Like), fires the completion — no state transition needed because there were no states.

The same primitive scales up: add one state ("pending review") with `requires` and now the Like is moderated. Add three states and it's a Deliberation. Zero → one → many is a continuum. Every button in the UI, every state advance, every complex process — one primitive.

## 7. Consensus rule + roles

### 7.1 v1 shape

```typescript
export type ConsensusRule = {
  n: number;                // required count of distinct DIDs (default 1)
  fromRole?: ModelQuery;    // optional — restricts eligible DIDs; all matching DIDs count
};
```

`{ n: 1 }` — any agent's proposal advances the state.
`{ n: 2 }` — 2 distinct agents.
`{ n: 1, fromRole: {...} }` — only agents matching the role query.
`{ n: 2, fromRole: {...} }` — 2 distinct role-matching agents.

### 7.2 How roles work — the ontology-bootstrap trick

The `fromRole` query returns a set of DIDs. Semantics: a DID counts toward consensus iff (a) it has proposed or accepted, AND (b) it appears in the `fromRole` query's result set.

**How does the query return DIDs?** Two shapes supported in v1:

**Shape 1: instance-carries-DID.** The query matches instances of a class whose property IS a DID.

```typescript
// "Reviewers for this task"
{ className: "Reviewer", where: { forTask: "$flow.base" }, didProperty: "agent" }
```

Engine runs the query → for each result, extracts `agent` property as a DID.

**Shape 2: instance-is-per-DID.** The query is templated with `$did`; engine runs it once per candidate DID.

```typescript
// "any agent with reputation ≥ 100"
{ className: "Reputation", where: { agent: "$did", score: { gte: 100 } } }
```

Engine iterates candidates, substitutes `$did`, runs query, keeps DIDs where the query returns at least one instance.

Shape 1 is preferred (more efficient). Shape 2 is available for computed / relational role definitions.

### 7.3 Example role definitions — all config, no code

**Static set of "team members":**
```typescript
// Ontology defines: class TeamMember { did: DID, joinedAt: string }
// Mint TeamMember instances for each team member.
fromRole: { className: "TeamMember", didProperty: "did" }
```

**Reviewers per-task:**
```typescript
// Ontology defines: class Reviewer { agent: DID, forTask: URI }
// Mint one Reviewer instance per (person, task) pair.
fromRole: { className: "Reviewer", where: { forTask: "$flow.base" }, didProperty: "agent" }
```

**Assignee only:**
```typescript
// The assignee is a property on the Task itself.
fromRole: { className: "Task", where: { uri: "$flow.base" }, didProperty: "assignee" }
```

**Reputation gated:** (Shape 2)
```typescript
fromRole: { className: "Reputation", where: { agent: "$did", score: { gte: 100 } } }
```

**Multi-role hybrid** (either reviewer OR admin):
```typescript
fromRole: {
  or: [
    { className: "Reviewer", where: { forTask: "$flow.base" }, didProperty: "agent" },
    { className: "Admin", didProperty: "agent" }
  ]
}
```

### 7.4 Why this shape is right for v1

- **Zero new primitives.** Roles are just subject classes. Assigning a role is just minting an instance. Removing a role is just deleting the instance.
- **Zero code changes to define new role types.** A community can invent "Contributor", "Reviewer", "Guardian", "Moderator" in their ontology and reference them from flows — all via UI.
- **Ties naturally into Synergy Fuel.** When reputation / stake enters, it's just another class the role query can reference.
- **Composable.** `or` / `and` / `not` at the role level fall out of the same model_query mechanism.

### 7.5 v1.5+ extensions (design allows, but not required)

- **Weighted consensus** — instead of counting distinct DIDs, weight by stake/reputation. Extension: `{ n: 100, fromRole: {...}, weightBy: {property: "score"} }`.
- **Time-decay** — recent role assignments count more. Extension via `where` clause on `assignedAt` timestamps.
- **Delegation** — a DID delegates its vote to another. Encoded as a `Delegation` class + role query that follows the chain.

All of these are additive, no v1 change needed to be forward-compatible.

## 8. Available actions — the read side of a flow

v4 pinned down flow *definition* (input types, states, transitions, outputs). But nothing there tells a **rendering shell** what to draw as a button, or an **LLM** what to listen for during a call. That's the piece this section adds: one query, two consumers.

### 8.1 The core query — `flow.availableActions(item, agent?)`

Given a base expression `item` (and optionally the agent viewing it), return the set of transitions currently available on `item` — i.e. every flow move the perspective would accept right now, without asking the LLM.

```typescript
type AvailableAction = {
  flow:       string;       // SHACLFlow URI (e.g. "coasys://Delivery")
  flowLabel:  string;       // Human-visible name (e.g. "Delivery")
  transition: {
    from?: string;          // FlowState name; absent for zero-state flow-spawn
    to:    string;          // FlowState name; for zero-state flows: the outputType (e.g. "we://Like")
  };
  requires: ModelQuery[];   // The evidence the transition needs (already satisfied)
  consensusRule?: ConsensusRule; // If present, transition needs consensus to fire
  actionKind: "spawn-flow" | "advance-state" | "atomic-action";
};

perspective.availableActions(item: string, agent?: string): Promise<AvailableAction[]>
```

**How the engine computes the set** — three passes over the flow registry, each pure and deterministic:

1. **Spawn candidates** (`actionKind: "spawn-flow"`) — every `SHACLFlow` where `item`'s class is in `inputTypes`, and no `FlowInstance` for that flow already exists on `item`. For a zero-state flow, this is also the atomic-action row (see 3.).
2. **Advance candidates** (`actionKind: "advance-state"`) — for every running `FlowInstance` on `item`, every outgoing `FlowTransition` from its `currentState` whose `to.requires` are already satisfied by the current graph. `requires` evaluation reuses the exact machinery that fires transitions post-consensus (§4.2) — same code path, no drift.
3. **Atomic actions** (`actionKind: "atomic-action"`) — zero-state flows collapse: no `from`, `to = outputTypes[0]`, no state to advance, one consensus check away from producing the output. This is what makes "Like" a button.

`availableActions` is a **pure read**. It never touches the LLM, never writes to the graph, never triggers consensus. It's the single source both consumers project from.

### 8.2 UI consumer — rendering next-action affordances

A generic UI shell asks the perspective what's possible on the currently-focused item and renders accordingly:

```typescript
const actions = await perspective.availableActions(currentItem, self.did);
// [{flow: "we://Like", flowLabel: "Like", transition: {to: "we://Like"}, actionKind: "atomic-action"},
//  {flow: "coasys://Delivery", flowLabel: "Delivery", transition: {from: "inProgress", to: "review"}, actionKind: "advance-state"},
//  ...]
```

**Rendering conventions (implications, not spec):**

| `actionKind` | Rendered as |
|---|---|
| `atomic-action` | Button in the item's action row (Like, React, Bookmark, …) |
| `spawn-flow` (with states) | "Start Delivery flow" menu item — creates a new `FlowInstance` |
| `advance-state` | Contextual button on the item's flow strip: "Mark as review" |

The **same** query drives command-palette entries ("what can I do here?"), long-press menus, drag-target hints, and voice-command grammars. Every space in AD4M becomes browsable through one generic shell that renders differently *per space, per item, per role* purely because the flows in its social DNA differ — no per-app UI code. This is the productisation payoff §4.1.1 asserted; §8 makes the mechanism concrete.

Optional `agent` argument filters by `consensusRule.fromRole`: an action that requires `Reviewer` role never surfaces on a non-reviewer's UI. Same query surface, per-viewer filtering falls out for free.

### 8.3 LLM consumer — speech = clicking

During a call, the AutoProcessor's extraction prompt already asks the LLM to materialize new class instances from transcript turns. The **read side gives that same prompt a second job**: watch for utterances that fire an available action.

Every extraction pass, the prompt is assembled with:

```
Currently available actions in this scope:
- Delivery flow on "OAuth refactor" task: transition inProgress → review
  Fires when: someone says the task is done, ready to test, needs review, "PR is up", "please look at it"
- Delivery flow on "OAuth refactor" task: transition review → done
  Fires when: reviewer expresses approval, "looks good", "shipping it", "merged"
- Like action on message from Alice
  Fires when: someone reacts positively to this specific message: "nice", "love that", "yes exactly"
```

The English "Fires when" clauses come directly from each transition's `semanticCheck` (§4.1). No new schema field — the affordance query harvests the exact strings already declared on `FlowTransition` and threads them into the prompt.

When the LLM detects a firing utterance, it emits a `FlowTransitionProposal` (§4.1) with the same shape as any other extracted instance. From then on: identical to a button click. Deterministic engine verifies `requires`, checks consensus, fires or discards. Speech = clicking = both produce a proposal, both go through the same gate.

### 8.4 The bridge — one query, two consumers

The value of putting this into `availableActions` (and not building parallel "which buttons?" and "which speech triggers?" pipelines) is stark:

- **Add a new flow** → both UI and LLM notice, no code change.
- **Change a `requires`** → the button disappears when the requirement stops holding; the LLM stops listening for it. Both from the same evaluator.
- **Change a `semanticCheck`** → LLM's watch-list updates on the next extraction pass. UI unaffected.
- **Role-restrict a transition** → `fromRole` filter applies to both the button visibility and the LLM's prompt (per-agent).

The read-side is what makes "controller logic lives in the graph" (v4 §4.1.1) operational. The definition side (v4) says what a flow *is*; §8 says how the world *sees* it and *reacts to* it. Without §8, v4 is a theory; with §8, it's a runtime.

### 8.5 v1 scope for §8

- `perspective.availableActions(item, agent?)` API + `AvailableAction` type
- Three-pass evaluator (spawn / advance / atomic) — reuses existing `requires` code path
- AutoProcessor extraction prompt: automatically pre-pended with `availableActions` output for the scope's base (channel-scope prompt gets the union across every item in the drained batch, capped for prompt size)
- Subscription: `available-actions-changed(item)` fires when any flow state or `requires` result on `item` transitions the affordance set. Powers reactive UI without polling.
- Test: given a Task in `inProgress` + a rule "review needs one Reviewer", assert `availableActions(task, alice)` shows the review button for Alice-the-Reviewer but not Bob-the-non-Reviewer.

### 8.6 Deferred to v1.5+

- `AvailableAction.description` (LLM-generated helper text for tooltips / voice-command hints) — separate from `semanticCheck` which is for transition detection
- Predicted-next-actions (LLM suggests what a user might want to do, based on transcript context) — this is the harness territory (Task A in the pre-flow work sizing)
- Cross-flow suggestions ("this Task is done — start a Delivery flow on the parent Epic?")

## 9. Human review UX (implications, not spec)

Same overlay pattern as class-instance interpretation:
- `flow-proposal-added` subscription surfaces new proposals to the UI
- UI renders: "Data proposes we've moved to `tensionIdentified`. Evidence: [3 Perspective instances]. Rationale: '...'. [Accept] [Reject]"
- Accept → add `acceptedBy` link → engine re-evaluates consensus → if role-restricted, checks whether accepter matches `fromRole`
- Reject → delete → dedup prevents immediate re-proposal on same turn-hash

`FlowInstance.onStateChange` gives UIs a clean subscription surface — same pattern as §8's `available-actions-changed`, but for the *outcome* of a transition rather than the *availability* of one.

## 10. What's in v1 vs. later

### v1 (this PR arc)
- Schema fields on `SHACLFlow`: `inputTypes: string[]`, `outputTypes: string[]`, `consensusRule` at flow-level (with optional `fromRole`), `creationHint`, `context: ModelQuery[]`
- Schema fields on `FlowState`: `requires: ModelQuery[]` (AND across the array), `semanticCheck`, per-state `consensusRule` override
- Remove `stateCheck: LinkPattern` and retire `flowable: LinkPattern` (superseded by `inputTypes`)
- **Zero-state flows are first-class** — a `SHACLFlow` with no declared `FlowState`s is an atomic action; consensus fires immediately once `inputTypes` are satisfied and produces `outputTypes`
- `FlowTransitionProposal` @Model class with `evidenceHashes`
- `FlowInstance` runtime wrapper class + subscription APIs
- **AutoProcessor: channel-scope, flow-aware prompt (Model C)** — no per-flow AutoProcessors
- Deterministic engine layer: `requires` evaluation via model_query, evidence-hash verify, role-based consensus, transition firing (including zero-state fast-path)
- Overlay-exclusion in evidence-gathering
- `allRunningFlows` enumeration API + three subscription topics
- **`perspective.availableActions(item, agent?)` API** (§8): pure-read affordance query, three-pass evaluator (spawn / advance / atomic), agent-filtered by `consensusRule.fromRole`
- **`available-actions-changed(item)` subscription** — reactive UI without polling
- AutoProcessor extraction prompt pre-pended with per-item / per-scope `availableActions` (channel-scope union, prompt-size capped)
- Simple deterministic flow-spawn (on new instance matching any `inputTypes` URI)
- Auto-invalidation of superseded / hash-mismatched proposals

### v1.5+ (not this PR)
- LLM-driven flow *creation* via `creationHint`
- Dynamic per-transition methods on `FlowInstance`
- Weighted/reputation-based consensus (design already supports; wire in later)
- Time-decay and delegation for role queries
- Ontology-migration-as-flow (Josh's idea)
- Cross-flow inheritance (spawned flow inherits participants / evidence)
- LLM tool-calling for context-on-demand

### Adjacent (separate design docs when needed)
- Computed model properties (Miro rollup)
- Wii graph-view flow rendering
- Bot-interface skill descriptions (OpenClaw plugin for bots consuming flows)
- Flow deletion / cancellation semantics

### Non-goals for v1
- Real-time Meet/Zoom transcription integration (upstream)
- Historical replay to backfill flow states
- Reactive model_query change-triggering (AutoProcessor debounce handles it)

## 11. Test plan

**Unit (Rust, no LLM):**
- `model_query` execution against fixture instances (existing machinery, just confirm we call it right)
- Content-hash canonicalization determinism
- `evidenceHashes` re-verify at consensus firing (mismatch → proposal rejected)
- Role `fromRole` evaluation for Shape 1 (instance-carries-DID) and Shape 2 (per-DID templated)
- Deterministic engine: valid proposal + requires satisfied + consensus met + hashes stable → transition fires
- Overlay-exclusion: overlay-only property does not satisfy `requires`; once accepted-and-written, it does
- **`availableActions` — three-pass evaluator (§8):**
  - Spawn: `item.class ∈ flow.inputTypes` + no `FlowInstance` for that flow yet → row emitted with `actionKind: "spawn-flow"`
  - Advance: running instance in state S → every outgoing transition whose `to.requires` are satisfied appears; unsatisfied transitions do not
  - Atomic: zero-state flow with `inputTypes` satisfied → single `actionKind: "atomic-action"` row with `to = outputTypes[0]`
  - `fromRole` filter: agent-A's action list omits transitions requiring role-R when A is not in R; agent-B (in R) sees them
- **`available-actions-changed` subscription fires** when a `requires` result flips on any item in the perspective (relies on same event bus as `flow-state-changed`)

**Unit (TS):**
- `SHACLFlow.test.ts` extended for new fields' round-trip through `toLinks`/`fromLinks`
- `FlowTransitionProposal` @Model round-trip
- `FlowInstance` wrapper: getters return live data, subscriptions fire on pubsub events

**Integration (Rust, real LLM on Marvin):**
- **Deliberation demo:** scripted 5-turn transcript on a Proposal in a channel; assert flow advances `collectingPerspectives → tensionIdentified`; `FlowTransitionProposal` records exist with valid evidence hashes.
- **Delivery demo:** scripted transcript with "let's build X, I'll take it, done"; Delivery flow spawns + advances to `done`.
- **Role-consensus demo:** flow with `fromRole = { className: "Reviewer", where: {forTask: "$flow.base"}, didProperty: "agent" }` and `n: 1`. Mint one Reviewer instance for Bob. Alice proposes → does not fire. Bob accepts → fires.
- **Semantic-check demo:** flow with `semanticCheck: "..."`. Structural requires met + LLM says "not really" → proposal discarded.
- **Overlay-exclusion demo:** overlay-only property does not satisfy `requires`; once accepted → does.
- **Hash-verify demo:** proposal cites an evidence instance; instance edited between proposal and consensus → transition rejected.
- **`availableActions` prompt-integration demo (§8.3):** AutoProcessor's extraction prompt includes the current affordance set for the channel scope; a scripted transcript containing a firing utterance ("PR is up, please review") produces a `FlowTransitionProposal` for the matching Delivery `inProgress → review` transition. Same test with a non-firing utterance ("weather is nice") produces no proposal.

**North-star integration test:**
"3-way call over Marvin → channel-scoped AutoProcessor extracts instances flow-aware → a Delivery flow on a Task advances through inProgress → done → visible via `flow-state-changed` subscription in Wii graph view; **the same call renders a live next-actions palette from `availableActions`, and the button shown at inProgress fires the same transition the LLM would fire from speech**." When this passes, Col 2 + Col 3 of the SoA tree turn green together AND the read-side loop is proven end-to-end.

## 12. Open items to nail down at build time

- **Compound `requires` — RESOLVED.** The Deliberation `tensionIdentified` case ("one supporting AND one opposing") is handled by putting two independent `ModelQuery` entries in the `requires` array — the array has AND semantics across its entries. No combinator DSL needed. If a flow ever needs OR / NOT it can either mint an intermediate marker class or (v1.5) we add an explicit combinator wrapper; deferred until a real use case shows up.
- **`$flow.base`, `$flow.uri`, `$did` template substitution:** engine walks the ModelQuery JSON tree pre-execution, substitutes strings. Simple.
- **model_query engine path from Rust:** the extension already exists (ORM ships model_query on the Rust side). Confirm at build time that we can call it deterministically from the flow-guard evaluator.
- **Content-hash canonicalization spec:** properties sorted by URI, multi-values sorted lexicographically, JSON-string of `{property → [values]}`, SHA-256 hex.
- **Turn-hash scope for dedup:** `(flow, turn-hash)` — the same transcript may be relevant to different flows for different reasons.
- **§8 `availableActions` — prompt-budget for AutoProcessor scope union.** A channel-scope AutoProcessor sees N items in a batch; naive projection is O(N × flows) rows in the prompt. Cap: only project affordances for items *touched by* the drained batch (turn-authors + their reply-targets), with a hard cap on unique row count. Overflow drops least-recent items with a `[+N more actions available]` sentinel. Real prompt shape gets locked at build time.
- **§8 `available-actions-changed` fanout.** A single `requires`-relevant graph edit can flip affordances on many items simultaneously. First cut: fire once per affected item; future optimisation: bulk `available-actions-changed(items[])` if UI clients get overwhelmed. Not gating v1.

## 13. PR sequence

1. **Schema + serialization commits (TS + Rust):** on `SHACLFlow` — `inputTypes`, `outputTypes`, `consensusRule` (w/ `fromRole`), `creationHint`, `context`; on `FlowState` — `requires`, `semanticCheck`, per-state `consensusRule` override. Remove `stateCheck: LinkPattern` and retire `flowable: LinkPattern`. Migrate the existing TODO flow test to the new shape.
2. **`FlowTransitionProposal` @Model + accept/reject API + content-hash canonicalization utility**
3. **Rust: `requires` evaluator (calls model_query) + role evaluator (Shape 1 + Shape 2) + evidence-hash verify + zero-state fast-path** — unit-tested without LLM
4. **AutoProcessor: channel-scope flow-aware prompt** — extends existing AutoProcessorConfig, adds active-flow-summary in prompt, wires overlay-exclusion in evidence gathering, wires deterministic post-extraction requires-check; spawns new flow instance when extraction produces an instance matching any `inputTypes` URI
5. **Optional `semanticCheck` targeted second LLM pass**
6. **`FlowInstance` wrapper class + `allRunningFlows` + three subscription topics + `PerspectiveProxy` API additions**
7. **`availableActions(item, agent?)` API + `available-actions-changed` subscription + AutoProcessor prompt integration** (§8 — read side) — pure-read affordance query, no LLM in the evaluator, powers both UI rendering and speech-detection
8. **Integration tests on Marvin:** Deliberation, Delivery, Role-consensus, Semantic-check, Overlay-exclusion, Hash-verify, **zero-state action-flow (Like)**, **`availableActions` role-filter**
9. **North-star demo:** 3-way call → channel AutoProcessor → Delivery flow advances → visible via subscription; **generic UI shell rendering next-actions from `availableActions` on the same call**

Stacked on `dev` post-#881. Breaking changes: removes `stateCheck: LinkPattern` and `flowable: LinkPattern` from the public interface — one test flow migrates. All other changes are additive.

---

*v1 signed off via Telegram voice 2026-08-17: scoping = base-item + graph-dangling, transitions explicit + declared, proposal-only LLM output, multi-agent consensus in v1.*

*v2 (2026-08-18): added FlowInstance wrapper, evidence content-hashing (Synergy-Fuel-ready), enumeration + subscription APIs, overlay-exclusion, north-star demo.*

*v3 (2026-08-18): replaced SPARQL guardQuery with model_query-based `requires` (reuse Ad4mModel query mechanism). Adopted **Model C — channel-scoped flow-aware extraction** (Models A/B/C laid out for team review). Extended `consensusRule` with `fromRole: ModelQuery` — role definitions collapse to model_query so new roles need no code. Design principle added: v1 must let humans experiment via UI/config, not code changes.*

*v3.1 (2026-08-19, Nico + James live review): `requires` is a `ModelQuery[]` array (AND across entries). Retires the compound-query open item.*

*v4 (2026-08-19, post Nico+James review, incorporated): Flow reframed as **typed processing** (input → processing → output). New fields on `SHACLFlow`: `inputTypes: string[]` + `outputTypes: string[]` + top-level `consensusRule`. **Actions unified as zero-state flows** — no separate "action" primitive. Retired `flowable: LinkPattern` (superseded by `inputTypes`). Added §1.1 group-mind framing. Added §6.3 "Like Action Flow" as first-class zero-state worked example. UI templates deliberately **not** referenced yet — separate design doc when their time comes.*

*v5 (2026-08-19, Nico follow-up post PR #902): Added **§8 "Available actions — the read side of a flow"**. The core productisation piece missing from v4: `availableActions(item, agent?)` returns the current affordance set (spawn / advance / atomic), driven by the same `requires` evaluator that fires transitions. **UI renders it as buttons**; **AutoProcessor pre-pends it to the extraction prompt** so the LLM watches for firing utterances during a call (speech = clicking). One query, two consumers, zero duplication. Sections 8–12 renumbered to 9–13. v1 scope + PR sequence updated to include the read-side API + subscription + Marvin integration test.*
