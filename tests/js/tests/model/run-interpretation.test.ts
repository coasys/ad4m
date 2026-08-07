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
 *   INTERPRETATION_E2E_MODEL    (default qwen3.5-27b-opus:latest)
 *
 * Run with (from tests/js, executor built + `pnpm run prepare-test` once):
 *   pnpm ts-mocha -p tsconfig.json --timeout 1200000 --exit tests/model/run-interpretation.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, LinkQuery, PerspectiveProxy } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { ExtBelief, ExtQuestion, ExtTask } from "./interpretation-models.js";

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "qwen3.5-27b-opus:latest";
const BASE_PREFIX = "soa://ext/";

describe("perspective.runInterpretation (WS + real LLM)", function () {
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

    const placements = await p.runInterpretation(transcript, BASE_PREFIX);

    // Something typed came out, minted under our prefix, with links.
    expect(placements.length).to.be.greaterThan(0);
    for (const pl of placements) {
      expect(pl.base.startsWith(BASE_PREFIX), `base ${pl.base}`).to.be.true;
      expect(pl.links.length).to.be.greaterThan(0);
      // every instance carries its class type-flag link
      expect(pl.links.some((l) => l.predicate === "soa://type")).to.be.true;
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
    const placements = await p.runInterpretation(
      [{ speaker: "Nico", text: `Reminder: ${before[0].title}.` }],
      BASE_PREFIX,
    );

    // None of the newly placed instances may duplicate a known task title.
    for (const pl of placements) {
      const title = pl.links.find((l) => l.predicate === "soa://title")?.target ?? "";
      const decoded = decodeURIComponent(title.replace(/^literal:string:/, "")).toLowerCase();
      expect(knownTitles.has(decoded), `must not recreate existing task "${decoded}"`).to.be.false;
    }
  });

  it("extracts only into the selected classes", async () => {
    // Transcript has a task, a belief, and a question — but select ONLY ExtTask.
    const transcript = [
      { speaker: "Nico", text: "James, please add docs for the WS runInterpretation endpoint." },
      { speaker: "James", text: "Will do. I think good docs are underrated." },
      { speaker: "Nico", text: "Should we version the WS API before release?" },
    ];

    const placements = await p.runInterpretation(transcript, BASE_PREFIX, ["ExtTask"]);

    expect(placements.length, "expected at least the task").to.be.greaterThan(0);
    // Every placement must be an ExtTask — no Belief/Question leaked in.
    for (const pl of placements) {
      const typeTarget = pl.links.find((l) => l.predicate === "soa://type")?.target;
      expect(typeTarget, `unexpected non-task type ${typeTarget}`).to.equal("soa://task");
    }
  });
});
