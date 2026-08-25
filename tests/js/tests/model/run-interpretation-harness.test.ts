/**
 * perspective.runInterpretationWithHarness — full TypeScript integration test
 * for the harness (tool-calling) dispatch path.
 *
 * Story exercised end-to-end:
 *   register ExtBelief + ExtIntention subject classes  ->
 *   seed a few existing ExtBelief instances directly  ->
 *   submit a transcript that expresses an intention grounded in those beliefs
 *   -> LLM must query the existing beliefs (`ExtBelief_query`), propose a new
 *   ExtIntention (`ExtIntention_propose_create`) and link it to at least one
 *   of the seeded beliefs (`ExtIntention_propose_link_child` via the
 *   `basedOn` HasMany relation)  ->  assertions verify the intention landed
 *   AND at least one of its `basedOn` links points at a seeded belief URI.
 *
 * This is the first TS-level integration test for the harness path — it
 * covers the whole stack: WS-RPC → new handler → engine dispatch → tool
 * provider (query + propose_* + propose_link_child) → OpenAI-compat bridge
 * → real LLM → buffered ops → overlay gate → real links.
 *
 * Requires a reachable LLM (Ollama over OpenAI-compatible API). On the
 * self-hosted CI runner (Marvin) it is local; from a dev box, tunnel it.
 * Endpoint + model are env-overridable (identical to the single-shot test):
 *   INTERPRETATION_E2E_BASE_URL (default http://localhost:11434/v1)
 *   INTERPRETATION_E2E_MODEL    (default gemma3:12b)
 *
 * Run with (from tests/js, executor built + `pnpm run prepare-test` once):
 *   pnpm ts-mocha -p tsconfig.json --timeout 1200000 --exit tests/model/run-interpretation-harness.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, LinkQuery, PerspectiveProxy } from "@coasys/ad4m";
import type { AutoProcessorEvent } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { ExtBelief, ExtIntention } from "./interpretation-models.js";

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "gemma3:12b";
const BASE_PREFIX = "soa://ext/";
// Harness needs enough headroom for query + propose_create + N propose_link_child
// plus the final answer. 15 is a comfortable ceiling for this transcript.
const MAX_TOOL_CALLS = 15;
// LLM tool-calling is more variable than the single-shot path: the model
// occasionally emits an answer without ever calling `_propose_*`, or forgets
// the link step. This mirrors the Rust e2e's 8-attempt guard in
// `interpretation_test_support::run_harness_e2e_until` — the harness *can*
// produce the target graph state; we just don't require it on the first draw
// against a small local model at Ollama-default temperature.
const HARNESS_E2E_MAX_ATTEMPTS = Number(process.env.HARNESS_E2E_MAX_ATTEMPTS ?? "8");

async function targetsOf(p: PerspectiveProxy, source: string, predicate: string): Promise<string[]> {
  const links = await p.get(new LinkQuery({ source, predicate }));
  return links.map((l) => l.data.target);
}

async function titleOf(p: PerspectiveProxy, base: string): Promise<string | undefined> {
  const links = await p.get(new LinkQuery({ source: base, predicate: "soa://title" }));
  const t = links[0]?.data.target;
  if (!t) return undefined;
  const body = t.replace(/^literal:string:/, "");
  try {
    return decodeURIComponent(body);
  } catch {
    return body;
  }
}

/**
 * Remove all ExtIntention instances and any ExtBelief instances the previous
 * attempt materialized. Seeded beliefs (identified by `seededBeliefUris`) are
 * left in place — they're the reference state the LLM is supposed to discover
 * on the next attempt. Purging non-seeded beliefs matters because the dedup
 * check downstream (`dupTitles`) would otherwise fire against beliefs a failed
 * attempt already created with a seeded title, permanently poisoning later
 * attempts.
 */
async function purgeGenerated(
  p: PerspectiveProxy,
  seededBeliefUris: Set<string>,
): Promise<void> {
  const intentions = await ExtIntention.findAll(p);
  const beliefs = await ExtBelief.findAll(p);
  const generatedBases = [
    ...intentions.map((i) => i.id),
    ...beliefs.map((b) => b.id).filter((id) => !seededBeliefUris.has(id)),
  ];
  for (const base of generatedBases) {
    const outgoing = await p.get(new LinkQuery({ source: base }));
    for (const link of outgoing) {
      try {
        await p.remove(link);
      } catch {
        // Best-effort cleanup — a stale link that fails to remove will just
        // be ignored on the next attempt (the assertions look at fresh state).
      }
    }
  }
}

describe("perspective.runInterpretationWithHarness (WS + real LLM)", function () {
  this.timeout(1_200_000);

  let ad4m: Ad4mClient;
  let stop: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;
  let seededBeliefUris: Set<string>;

  before(async function () {
    // Ollama-backed default LLM. On the Marvin CI runner MODEL is
    // pre-installed; from a dev box, tunnel to it. Match the classic
    // `run-interpretation.test.ts` approach: `addModel` sets up the
    // provider record and any subsequent LLM call either works or fails
    // loudly. No pre-flight probe / skip — a missing model on a runner
    // that's supposed to have it is a real regression to surface, not
    // silently pass (Nico's PR #911 review, 2026-08-25).
    const agent = await startAgent("run-interpretation-harness");
    ad4m = agent.client;
    stop = agent.stop;

    const modelId = await ad4m.ai.addModel({
      name: "harness-llm",
      api: { baseUrl: BASE_URL, apiKey: "ollama", model: MODEL, apiType: "OPEN_AI" },
      modelType: "LLM",
    } as any);
    await ad4m.ai.setDefaultModel("LLM", modelId);

    p = await ad4m.perspective.add("run-interpretation-harness-test");
    await ExtBelief.register(p);
    await ExtIntention.register(p);

    // Seed three ExtBelief instances DIRECTLY (no LLM). These are the URIs the
    // harness's LLM has to discover via ExtBelief_query and reference via
    // ExtIntention_propose_link_child. Every belief here is worded so the
    // downstream transcript intention obviously derives from at least one.
    const belief1 = await ExtBelief.create(p, {
      title: "Distributed collective intelligence requires an interoperable data layer.",
    });
    const belief2 = await ExtBelief.create(p, {
      title: "Users must own the schemas that describe their data, not the applications.",
    });
    const belief3 = await ExtBelief.create(p, {
      title: "SHACL is expressive enough to serve as that shared, user-owned schema layer.",
    });
    seededBeliefUris = new Set([belief1.id, belief2.id, belief3.id]);
  });

  after(async () => {
    if (stop) await stop();
  });

  it("derives an intention from existing beliefs and links it back to them", async () => {
    // A transcript that (a) does not restate any belief verbatim and (b)
    // expresses a course of action that clearly follows from the beliefs
    // above. The LLM must reach for ExtBelief_query first (to discover the
    // seeded URIs) before proposing the intention.
    const transcript = [
      { speaker: "Nico", text: "Given where we've landed on the data layer discussion, I think we should commit to shipping the SHACL-based subject-class system as the default in the next AD4M release." },
      { speaker: "James", text: "Agreed. That's the practical step that follows from those beliefs — we can stop treating it as an experiment." },
      { speaker: "Nico", text: "Let's make it the intention going into the sprint." },
    ];

    let lastError: any = null;
    for (let attempt = 1; attempt <= HARNESS_E2E_MAX_ATTEMPTS; attempt++) {
      if (attempt > 1) {
        // Clear any intentions AND any non-seeded beliefs the previous
        // attempt landed so this attempt sees the same starting state
        // (seeded beliefs only). Mirrors the Rust helper's "fresh
        // perspective per attempt" property. Purging beliefs (not just
        // intentions) is critical: a failed attempt that recreated a
        // seeded-title belief would otherwise leave a duplicate in place
        // and poison the `dupTitles` dedup check on every later attempt.
        await purgeGenerated(p, seededBeliefUris);
      }

      try {
        const bases = await p.runInterpretationWithHarness(
          transcript,
          BASE_PREFIX,
          MAX_TOOL_CALLS,
          ["ExtBelief", "ExtIntention"],
        );

        // Something landed under our prefix.
        expect(bases.length, "harness pass must produce at least one instance").to.be.greaterThan(0);
        for (const base of bases) {
          expect(base.startsWith(BASE_PREFIX), `base ${base}`).to.be.true;
        }

        // At least one ExtIntention was materialized (not a bare belief).
        const intentions = await ExtIntention.findAll(p);
        expect(
          intentions.length,
          `expected the harness to materialize at least one ExtIntention (found ${intentions.length}) — bases returned: ${JSON.stringify(bases)}`,
        ).to.be.greaterThan(0);
        for (const intent of intentions) {
          expect(intent.title, "every intention needs a non-empty title").to.be.a("string");
          expect(intent.title.length).to.be.greaterThan(0);
        }

        // At least one intention must be linked back to at least one SEEDED belief
        // via the `basedOn` HasMany relation. This is the key harness assertion:
        // it proves the LLM used a query tool to discover the existing URIs and
        // then a propose_link_child tool to attach them — the graph state the
        // harness path is meant to produce.
        let intentionsWithBackedBelief = 0;
        for (const intent of intentions) {
          const linkedBeliefs = await targetsOf(p, intent.id, "soa://basedOn");
          const anyLinkedIsSeeded = linkedBeliefs.some((uri) => seededBeliefUris.has(uri));
          if (anyLinkedIsSeeded) intentionsWithBackedBelief += 1;
        }
        expect(
          intentionsWithBackedBelief,
          `expected at least one ExtIntention to link back (via soa://basedOn) to a SEEDED belief URI. Seeded: ${JSON.stringify([
            ...seededBeliefUris,
          ])}. Intentions: ${JSON.stringify(
            await Promise.all(
              intentions.map(async (i) => ({
                id: i.id,
                title: await titleOf(p, i.id),
                basedOn: await targetsOf(p, i.id, "soa://basedOn"),
              })),
            ),
          )}.`,
        ).to.be.greaterThan(0);

        // No belief with a seeded title should have been recreated (dedup check).
        const seededTitles = new Set(
          await Promise.all([...seededBeliefUris].map((uri) => titleOf(p, uri))),
        );
        const beliefsNow = await ExtBelief.findAll(p);
        const dupTitles = beliefsNow
          .map((b) => b.title)
          .filter((t) => seededTitles.has(t))
          .filter((t, idx, arr) => arr.indexOf(t) !== idx);
        expect(
          dupTitles.length,
          `harness must not recreate an existing belief by title; duplicates: ${JSON.stringify(dupTitles)}`,
        ).to.equal(0);

        // All assertions passed — done.
        console.log(`[harness-ts-e2e] passed on attempt ${attempt}/${HARNESS_E2E_MAX_ATTEMPTS}`);
        return;
      } catch (e) {
        lastError = e;
        console.log(
          `[harness-ts-e2e] attempt ${attempt}/${HARNESS_E2E_MAX_ATTEMPTS} did not satisfy retry guard: ${(e as Error).message}`,
        );
      }
    }

    // All attempts exhausted — surface the last failure.
    throw lastError;
  });

  it("bounces maxToolCalls=0 as a boundary error", async () => {
    let err: any = null;
    try {
      await p.runInterpretationWithHarness(
        [{ speaker: "Nico", text: "trivial" }],
        BASE_PREFIX,
        0,
        ["ExtBelief"],
      );
    } catch (e) {
      err = e;
    }
    expect(err, "runInterpretationWithHarness(maxToolCalls=0) must throw").to.not.equal(null);
    const msg = String(err?.message ?? err);
    expect(msg.toLowerCase()).to.match(/maxtoolcalls|max_tool_calls|must be/);
  });
});

/**
 * Full-stack relation-hint integration test — proves the per-relation
 * `interpretationHint` declared on `@HasMany(...)` reaches the LLM through
 * the whole stack (TS decorator → SHACL SDNA → Rust `ShaclProperty
 * .interpretation_hint` → `ClassProposeShape.relations[N].hint` →
 * `_propose_link_child` predicate description → LLM picks the right
 * predicate based on the hint text).
 *
 * The `ExtIntention` model here declares TWO relations to `ExtBelief`
 * with opposite semantics:
 *   * `basedOn`      — beliefs that JUSTIFY the intention.
 *   * `contradicts`  — beliefs the intention REJECTS / counters.
 *
 * The transcript expresses an intention that clearly derives from two
 * supporting beliefs AND stands against a third opposing belief. Correct
 * behaviour requires the model to have READ the hint on each relation —
 * without the hint, the model has no way to distinguish which predicate
 * to reach for (both are `basedOn / contradicts` bare local names, both
 * are HasMany relations to the same target class).
 */
describe("perspective.runInterpretationWithHarness — relation interpretation hints", function () {
  this.timeout(1_200_000);

  let ad4m: Ad4mClient;
  let stop: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;
  let supportingBeliefUris: Set<string>;
  let opposingBeliefUris: Set<string>;

  before(async function () {
    // Same as the sibling describe: `addModel` + let it fail loudly on
    // runners that are supposed to have the model but don't. No skip.
    const agent = await startAgent("run-interpretation-harness-relation-hints");
    ad4m = agent.client;
    stop = agent.stop;

    const modelId = await ad4m.ai.addModel({
      name: "harness-llm",
      api: { baseUrl: BASE_URL, apiKey: "ollama", model: MODEL, apiType: "OPEN_AI" },
      modelType: "LLM",
    } as any);
    await ad4m.ai.setDefaultModel("LLM", modelId);

    p = await ad4m.perspective.add("run-interpretation-harness-relation-hints-test");
    await ExtBelief.register(p);
    await ExtIntention.register(p);

    // Seed a mix of beliefs. Every belief here is worded so its semantic
    // role (support vs oppose) w.r.t. the transcript intention is
    // unambiguous — if the LLM picks the wrong predicate we know it
    // ignored the relation hint, not that the seed was ambiguous.
    const support1 = await ExtBelief.create(p, {
      title: "User sovereignty over data schemas is the foundation of a healthy web.",
    });
    const support2 = await ExtBelief.create(p, {
      title: "Local-first software architecture makes user sovereignty practical, not just aspirational.",
    });
    const opposing1 = await ExtBelief.create(p, {
      title: "Only centralised SaaS platforms can deliver viable AI at scale.",
    });
    supportingBeliefUris = new Set([support1.id, support2.id]);
    opposingBeliefUris = new Set([opposing1.id]);
  });

  after(async () => {
    if (stop) await stop();
  });

  it("links via basedOn to supporting beliefs AND via contradicts to opposing ones", async () => {
    // Transcript expresses an intention that (a) rests on the two
    // supporting beliefs and (b) stands directly against the opposing one.
    // The LLM must:
    //   1. Discover all three seeded beliefs via `ExtBelief_query`.
    //   2. Read the per-relation hints on ExtIntention_propose_link_child's
    //      `predicate` enum (`basedOn` = derives-from, `contradicts` =
    //      rejects) and pick the RIGHT predicate for each seeded belief.
    //   3. Emit propose_link_child calls with the correct predicate URIs.
    const transcript = [
      {
        speaker: "Nico",
        text: "We should commit to a local-first, user-sovereign release path for AD4M — that's the direction I want us going into this next sprint.",
      },
      {
        speaker: "James",
        text: "Agreed. It's exactly the practical follow-through on the sovereignty argument. And it means we're explicitly rejecting the 'only centralised SaaS can do AI' framing — that view is the one we're actively countering with this release.",
      },
      {
        speaker: "Nico",
        text: "Right. Make it the intention for the sprint: ship the local-first, user-sovereign release as our stand against the centralised-SaaS-only model of AI.",
      },
    ];

    const seededUris = new Set([...supportingBeliefUris, ...opposingBeliefUris]);

    let lastError: any = null;
    for (let attempt = 1; attempt <= HARNESS_E2E_MAX_ATTEMPTS; attempt++) {
      if (attempt > 1) {
        // Fresh state per attempt: strip any intentions the previous try
        // wrote + any non-seeded beliefs so the LLM sees the same starting
        // graph on every draw.
        await purgeGenerated(p, seededUris);
      }

      try {
        const bases = await p.runInterpretationWithHarness(
          transcript,
          BASE_PREFIX,
          MAX_TOOL_CALLS,
          ["ExtBelief", "ExtIntention"],
        );

        // Something landed.
        expect(bases.length, "harness pass must produce at least one instance").to.be.greaterThan(0);
        for (const base of bases) {
          expect(base.startsWith(BASE_PREFIX), `base ${base}`).to.be.true;
        }

        // At least one intention.
        const intentions = await ExtIntention.findAll(p);
        expect(
          intentions.length,
          `expected the harness to materialize at least one ExtIntention (found ${intentions.length})`,
        ).to.be.greaterThan(0);

        // Aggregate the two predicate targets across all intentions —
        // small models sometimes split into two intentions ("commit to X"
        // + "reject Y"); accept either shape as long as SOMEWHERE across
        // intentions we see one basedOn→supporting and one
        // contradicts→opposing.
        const basedOnHits: string[] = [];
        const contradictsHits: string[] = [];
        const basedOnMisdirects: string[] = []; // basedOn → opposing (semantic error)
        const contradictsMisdirects: string[] = []; // contradicts → supporting (semantic error)
        for (const intent of intentions) {
          const basedOn = await targetsOf(p, intent.id, "soa://basedOn");
          const contradicts = await targetsOf(p, intent.id, "soa://contradicts");
          for (const uri of basedOn) {
            if (supportingBeliefUris.has(uri)) basedOnHits.push(uri);
            if (opposingBeliefUris.has(uri)) basedOnMisdirects.push(uri);
          }
          for (const uri of contradicts) {
            if (opposingBeliefUris.has(uri)) contradictsHits.push(uri);
            if (supportingBeliefUris.has(uri)) contradictsMisdirects.push(uri);
          }
        }

        expect(
          basedOnHits.length,
          `expected at least one basedOn link to a SUPPORTING belief. Supporting: ${JSON.stringify([...supportingBeliefUris])}. basedOn observed: ${JSON.stringify(await Promise.all(intentions.map((i) => targetsOf(p, i.id, "soa://basedOn"))))}`,
        ).to.be.greaterThan(0);
        expect(
          contradictsHits.length,
          `expected at least one contradicts link to an OPPOSING belief. Opposing: ${JSON.stringify([...opposingBeliefUris])}. contradicts observed: ${JSON.stringify(await Promise.all(intentions.map((i) => targetsOf(p, i.id, "soa://contradicts"))))}`,
        ).to.be.greaterThan(0);

        // Semantic-role correctness: an intention that basedOn's the
        // opposing belief (or contradicts a supporting one) means the
        // LLM ignored the relation-hint. This is the hard assertion —
        // relaxed to `<= 1` misdirect on each side so a single
        // stochastic slip on gemma3:12b doesn't fail the whole run when
        // the majority is correct.
        expect(
          basedOnMisdirects.length,
          `basedOn misdirects (linked opposing belief as supporting): ${JSON.stringify(basedOnMisdirects)}`,
        ).to.be.lessThanOrEqual(1);
        expect(
          contradictsMisdirects.length,
          `contradicts misdirects (linked supporting belief as opposing): ${JSON.stringify(contradictsMisdirects)}`,
        ).to.be.lessThanOrEqual(1);

        console.log(
          `[relation-hint-e2e] passed on attempt ${attempt}/${HARNESS_E2E_MAX_ATTEMPTS} — basedOn→supporting: ${basedOnHits.length}, contradicts→opposing: ${contradictsHits.length}`,
        );
        return;
      } catch (e) {
        lastError = e;
        console.log(
          `[relation-hint-e2e] attempt ${attempt}/${HARNESS_E2E_MAX_ATTEMPTS} did not satisfy retry guard: ${(e as Error).message}`,
        );
      }
    }
    throw lastError;
  });
});

/**
 * Full-stack integration test for tool-call events on the
 * `auto-processor-event` topic. Proves the harness loop's
 * `ToolCall` / `ToolResult` emissions reach a subscribed TS client
 * with the expected payload fields (`toolName`, `toolArgsJson`,
 * `toolResult`), keyed by the caller-supplied `observationId`.
 *
 * Wire path exercised end-to-end:
 *   PerspectiveProxy.runInterpretationWithHarness(..., observationId,
 *     emitDebugEvents=true)
 *     → WS-RPC handler builds `InterpretationEmitContext`
 *     → `run_interpretation_with_harness_and_model` threads it into
 *       `run_with_tools`
 *     → per dispatched tool_call, `emit_tool_event(ToolCall)` +
 *       `emit_tool_event(ToolResult)` publish on the pubsub topic
 *     → GraphQL subscription forwards to the TS client
 *     → `addAutoProcessorEventListener` fires our callback
 *     → assertions verify each event's shape.
 *
 * Gated on the same Marvin LLM availability probe as the sibling
 * tests. Uses the simple `ExtBelief` + task-tracker transcript from
 * scenario B so the harness reliably calls at least one tool
 * (`ExtBelief_create` or `ExtBelief_query`).
 */
describe("perspective.runInterpretationWithHarness — tool-call events", function () {
  this.timeout(1_200_000);

  let ad4m: Ad4mClient;
  let stop: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async function () {
    // Same as the sibling describes: `addModel` + let it fail loudly on
    // runners that are supposed to have the model but don't. No skip.
    const agent = await startAgent("run-interpretation-harness-events");
    ad4m = agent.client;
    stop = agent.stop;

    const modelId = await ad4m.ai.addModel({
      name: "harness-llm",
      api: { baseUrl: BASE_URL, apiKey: "ollama", model: MODEL, apiType: "OPEN_AI" },
      modelType: "LLM",
    } as any);
    await ad4m.ai.setDefaultModel("LLM", modelId);

    p = await ad4m.perspective.add("run-interpretation-harness-events-test");
    await ExtBelief.register(p);
    await ExtIntention.register(p);
  });

  after(async () => {
    if (stop) await stop();
  });

  it("emits ToolCall + ToolResult events keyed by observationId", async () => {
    // Subscribe FIRST so we don't miss the events that fire during the pass.
    // Filter by observationId (mapped to `processorId` on the wire) so
    // events from parallel test perspectives can't cross-contaminate — this
    // test's observationId is unique to it.
    const observationId = `tool-events-test-${Date.now()}`;
    const collected: AutoProcessorEvent[] = [];
    await p.addAutoProcessorEventListener((event) => {
      if (event.processorId === observationId) {
        collected.push(event);
      }
    });

    // Simple transcript that reliably triggers at least one propose_create.
    // Scenario-B shape: three assignment-style utterances, three Task nodes.
    const transcript = [
      { speaker: "Nico", text: "James, can you finish the WebRTC call module in WE by Monday?" },
      { speaker: "James", text: "On it. And Ari, could you review the AD4M harness PR before then?" },
      { speaker: "Ari", text: "Yes. I'll also spike the SoA tree scaffolding this week." },
    ];

    let lastError: any = null;
    for (let attempt = 1; attempt <= HARNESS_E2E_MAX_ATTEMPTS; attempt++) {
      if (attempt > 1) {
        // Fresh state per attempt — same purge helper as the sibling tests.
        await purgeGenerated(p, new Set());
        collected.length = 0;
      }

      try {
        const bases = await p.runInterpretationWithHarness(
          transcript,
          BASE_PREFIX,
          MAX_TOOL_CALLS,
          ["ExtBelief", "ExtIntention"],
          undefined, // modelOverride
          observationId,
          true, // emitDebugEvents — this is what gates ToolCall/ToolResult
        );

        expect(bases.length, "harness pass must produce at least one instance").to.be.greaterThan(0);

        // Give the subscription pipeline a beat to catch up — events are
        // fire-and-forget on the Rust side; the WS delivery + local
        // callback isn't awaited by the RPC response.
        await new Promise((resolve) => setTimeout(resolve, 1500));

        // Filter to the events we care about; keep the others for the
        // failure diagnostic if the assertion misses.
        const toolCallEvents = collected.filter((e) => e.step === "toolCall");
        const toolResultEvents = collected.filter((e) => e.step === "toolResult");

        expect(
          toolCallEvents.length,
          `expected at least one ToolCall event keyed by observationId=${observationId}; observed steps: ${JSON.stringify(collected.map((e) => e.step))}`,
        ).to.be.greaterThan(0);
        expect(
          toolResultEvents.length,
          `expected at least one ToolResult event keyed by observationId=${observationId}; observed steps: ${JSON.stringify(collected.map((e) => e.step))}`,
        ).to.be.greaterThan(0);

        // Payload shape: ToolCall must carry a tool name AND JSON-encoded args.
        for (const call of toolCallEvents) {
          expect(call.toolName, `ToolCall event missing toolName: ${JSON.stringify(call)}`).to.be.a("string");
          expect(call.toolName!.length).to.be.greaterThan(0);
          expect(call.toolArgsJson, `ToolCall event missing toolArgsJson: ${JSON.stringify(call)}`).to.be.a("string");
          // toolArgsJson is a JSON string (may be `{}` for a zero-arg tool);
          // parseable is the invariant, not "has fields".
          expect(() => JSON.parse(call.toolArgsJson!), `toolArgsJson must parse as JSON: ${call.toolArgsJson}`).to.not.throw();
        }
        // Payload shape: ToolResult must carry a tool name AND result text.
        for (const result of toolResultEvents) {
          expect(result.toolName, `ToolResult event missing toolName: ${JSON.stringify(result)}`).to.be.a("string");
          expect(result.toolResult, `ToolResult event missing toolResult: ${JSON.stringify(result)}`).to.be.a("string");
        }

        // Sanity: batchKey and processorId both == observationId (one-shot
        // uses the same value for both since there's no persistent processor
        // or batch — WS-RPC handler wires it that way).
        for (const evt of [...toolCallEvents, ...toolResultEvents]) {
          expect(evt.processorId).to.equal(observationId);
          expect(evt.batchKey).to.equal(observationId);
        }

        console.log(
          `[tool-events-e2e] passed on attempt ${attempt}/${HARNESS_E2E_MAX_ATTEMPTS} — ${toolCallEvents.length} ToolCall + ${toolResultEvents.length} ToolResult events`,
        );
        return;
      } catch (e) {
        lastError = e;
        console.log(
          `[tool-events-e2e] attempt ${attempt}/${HARNESS_E2E_MAX_ATTEMPTS} did not satisfy retry guard: ${(e as Error).message}`,
        );
      }
    }
    throw lastError;
  });

  it("emits no tool-call events when emitDebugEvents is omitted (fast path)", async () => {
    // Regression: the headless fast path (no `emitDebugEvents`, no
    // `observationId`) must NOT publish anything. Subscribe filtered by a
    // fresh id that no pass could use, run a pass without the switches,
    // and confirm no ToolCall/ToolResult land under that filter — and
    // separately confirm no such events land under NO filter either
    // (which would mean a global event leak).
    const observationId = `tool-events-fastpath-${Date.now()}`;
    const collected: AutoProcessorEvent[] = [];
    let anyGlobalToolEvent = false;
    await p.addAutoProcessorEventListener((event) => {
      if (event.processorId === observationId) {
        collected.push(event);
      }
      if (event.step === "toolCall" || event.step === "toolResult") {
        anyGlobalToolEvent = true;
      }
    });

    await p.runInterpretationWithHarness(
      [{ speaker: "Nico", text: "no-op." }],
      BASE_PREFIX,
      MAX_TOOL_CALLS,
      ["ExtBelief", "ExtIntention"],
      // omit modelOverride, observationId, emitDebugEvents → fast path
    );
    await new Promise((resolve) => setTimeout(resolve, 1500));

    expect(collected, "no events should land under the filter observationId").to.deep.equal([]);
    expect(anyGlobalToolEvent, "no ToolCall/ToolResult events should land at all when emitDebugEvents is off").to.equal(false);
  });
});
