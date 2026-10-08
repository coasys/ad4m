/**
 * perspective.runInterpretation — full TypeScript integration test.
 *
 * Drives the generic LLM interpretation end-to-end over the real WS API:
 *   register SoA subject classes  ->  p.runInterpretation(transcript)  ->
 *   typed instances written into the perspective, queryable via the model API.
 *
 * Requires a reachable LLM (Ollama over the OpenAI-compatible API). On the
 * self-hosted CI runner (Marvin) the endpoint is local; from a dev box, tunnel
 * it. Endpoint + model are env-overridable to match the Rust e2e suite:
 *   INTERPRETATION_E2E_BASE_URL (default http://localhost:11434/v1)
 *   INTERPRETATION_E2E_MODEL    (default gemma3:12b)
 *
 * Run with (from tests/js, executor built + `pnpm run prepare-test` once):
 *   pnpm ts-mocha -p tsconfig.json --timeout 1200000 --exit tests/model/run-interpretation.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, LinkQuery, PerspectiveProxy } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { ExtBelief, ExtQuestion, ExtTask } from "./interpretation-models.js";

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "gemma3:12b";
const BASE_PREFIX = "soa://ext/";

async function typeOf(p: PerspectiveProxy, base: string): Promise<string | undefined> {
  const links = await p.get(new LinkQuery({ source: base, predicate: "soa://type" }));
  return links[0]?.data.target;
}

async function titleOf(p: PerspectiveProxy, base: string): Promise<string | undefined> {
  const links = await p.get(new LinkQuery({ source: base, predicate: "soa://title" }));
  const t = links[0]?.data.target;
  if (!t) return undefined;
  const body = t.replace(/^literal:string:/, "");
  // `decodeURIComponent` throws `URIError` on any lone `%` — an LLM-generated
  // title like "100% coverage" would then crash the test with a decode error
  // instead of an assertion message. Fall back to the raw body so the caller
  // still gets a comparable string (CodeRabbit #881 review).
  try {
    return decodeURIComponent(body);
  } catch {
    return body;
  }
}

// LLM E2E gate — this suite drives the executor's real LLM path. Skipped by
// default (unset `LLM_E2E`); the nightly `llm-e2e` workflow on `dev` runs it
// against Marvin's Ollama. Run locally with `LLM_E2E=1`, or the umbrella
// `./scripts/run-llm-e2e.sh` at the repo root.
const describeIfLLM: Mocha.SuiteFunction = (process.env.LLM_E2E === "1"
  ? describe
  : (describe.skip as unknown as Mocha.SuiteFunction));

describeIfLLM("perspective.runInterpretation (WS + real LLM)", function () {
  this.timeout(1_200_000);

  let ad4m: Ad4mClient;
  let stop: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const agent = await startAgent("run-interpretation");
    ad4m = agent.client;
    stop = agent.stop;

    // Register the Ollama-backed default LLM the extractor prompts against.
    const modelId = await ad4m.ai.addModel({
      name: "interpretation-llm",
      api: { baseUrl: BASE_URL, apiKey: "ollama", model: MODEL, apiType: "OPEN_AI" },
      modelType: "LLM",
    } as any);
    await ad4m.ai.setDefaultModel("LLM", modelId);

    p = await ad4m.perspective.add("run-interpretation-test");
    await ExtTask.register(p);
    await ExtBelief.register(p);
    await ExtQuestion.register(p);
  });

  after(async () => {
    if (stop) await stop();
  });

  it("extracts typed instances from a transcript into the perspective's classes", async () => {
    const transcript = [
      { speaker: "Nico", text: "James, can you write the integration test for the interpretation endpoint?" },
      { speaker: "James", text: "Sure. I still think the WS layer is the cleanest way to expose this." },
      { speaker: "Nico", text: "How do we handle a perspective that has no subject classes registered?" },
    ];

    const bases = await p.runInterpretation(transcript, BASE_PREFIX);

    // Something typed came out, minted under our prefix, with a type flag.
    expect(bases.length).to.be.greaterThan(0);
    for (const base of bases) {
      expect(base.startsWith(BASE_PREFIX), `base ${base}`).to.be.true;
      const typeTarget = await typeOf(p, base);
      expect(typeTarget, `every instance must carry soa://type; missing on ${base}`).to.be.a("string");
    }

    // The task assignment should have produced at least one queryable ExtTask.
    const tasks = await ExtTask.findAll(p);
    expect(tasks.length, "expected at least one extracted task").to.be.greaterThan(0);
    expect(tasks.every((t) => typeof t.title === "string" && t.title.length > 0)).to.be.true;

    // Links actually landed in the perspective graph.
    const typeLinks = await p.get(new LinkQuery({ predicate: "soa://type" }));
    expect(typeLinks.length).to.be.greaterThan(0);
  });

  it("does not recreate a task already present (dedup on re-run)", async () => {
    const before = await ExtTask.findAll(p);
    expect(before.length, "precondition: a task exists from the first run").to.be.greaterThan(0);
    const knownTitles = new Set(before.map((t) => t.title.toLowerCase()));

    // Restate an existing task (no new information) + nothing else actionable.
    const bases = await p.runInterpretation(
      [{ speaker: "Nico", text: `Reminder: ${before[0].title}.` }],
      BASE_PREFIX,
    );

    // None of the newly placed instances may duplicate a known task title.
    for (const base of bases) {
      const title = (await titleOf(p, base)) ?? "";
      expect(
        knownTitles.has(title.toLowerCase()),
        `must not recreate existing task "${title}"`,
      ).to.be.false;
    }
  });

  it("extracts only into the selected classes", async () => {
    // Transcript has a task, a belief, and a question — but select ONLY ExtTask.
    const transcript = [
      { speaker: "Nico", text: "James, please add docs for the WS runInterpretation endpoint." },
      { speaker: "James", text: "Will do. I think good docs are underrated." },
      { speaker: "Nico", text: "Should we version the WS API before release?" },
    ];

    const bases = await p.runInterpretation(transcript, BASE_PREFIX, ["ExtTask"]);

    expect(bases.length, "expected at least the task").to.be.greaterThan(0);
    // Every base must be an ExtTask — no Belief/Question leaked in.
    for (const base of bases) {
      const typeTarget = await typeOf(p, base);
      expect(typeTarget, `unexpected non-task type ${typeTarget} on ${base}`).to.equal("soa://task");
    }
  });
});
