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
import { startAgent } from "../../helpers/index.js";
import { ExtBelief, ExtIntention } from "./interpretation-models.js";

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "gemma3:12b";
const BASE_PREFIX = "soa://ext/";
// Harness needs enough headroom for query + propose_create + N propose_link_child
// plus the final answer. 15 is a comfortable ceiling for this transcript.
const MAX_TOOL_CALLS = 15;

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

describe("perspective.runInterpretationWithHarness (WS + real LLM)", function () {
  this.timeout(1_200_000);

  let ad4m: Ad4mClient;
  let stop: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;
  let seededBeliefUris: Set<string>;

  before(async () => {
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
