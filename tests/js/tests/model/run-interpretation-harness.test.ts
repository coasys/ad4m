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
    // LLM availability gate — see file header. The test needs a reachable
    // OpenAI-compatible endpoint at BASE_URL (Ollama on Marvin locally; a
    // tunnel from a dev box). Skip cleanly on runners without it so the
    // whole suite doesn't fail on environments that just don't have the
    // hardware.
    try {
      const probe = await fetch(BASE_URL.replace(/\/v1\/?$/, "") + "/v1/models", {
        signal: AbortSignal.timeout(3000),
      });
      if (!probe.ok) throw new Error(`probe ${probe.status}`);
      // Gate on the specific model too — an endpoint that answers /v1/models
      // but doesn't host MODEL would still fail the first LLM call. Skip
      // instead of failing the run.
      const body = (await probe.json()) as { data?: Array<{ id?: string }> };
      const ids = (body.data ?? []).map((m) => m.id).filter((id): id is string => !!id);
      if (!ids.includes(MODEL)) {
        throw new Error(`model ${MODEL} not present in /v1/models (have: ${ids.join(", ") || "none"})`);
      }
    } catch (e) {
      console.log(`Skipping harness e2e — LLM endpoint ${BASE_URL} unreachable: ${(e as Error).message}`);
      this.skip();
    }

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
