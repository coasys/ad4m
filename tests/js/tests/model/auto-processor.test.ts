/**
 * perspective.addAutoProcessor — high-level, signal-driven integration test
 * over the real WS API (single executor).
 *
 * The clean Flux flow: register a processor + post a channel's messages, then
 * let the executor's watch loop do everything — gather the transcript, debounce,
 * claim, run the LLM, write the ConversationSubgroup — while the test only
 * *observes* it through `auto-processor-event` signals and asserts the outcome.
 * No manual runInterpretation, no manual transcript wrangling.
 *
 * Requires a reachable LLM (Ollama, OpenAI-compatible). Endpoint + model are
 * env-overridable to match the Rust e2e suite:
 *   INTERPRETATION_E2E_BASE_URL (default http://localhost:11434/v1)
 *   INTERPRETATION_E2E_MODEL    (default qwen3.5-27b-opus:latest)
 *
 * Run (from tests/js, executor built + `pnpm run prepare-test` once):
 *   pnpm ts-mocha -p tsconfig.json --timeout 1200000 --exit tests/model/auto-processor.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, Link, PerspectiveProxy } from "@coasys/ad4m";
import type { AutoProcessorEvent } from "@coasys/ad4m";
import { startAgent } from "../../helpers/index.js";
import { ConversationSubgroup } from "./auto-processor-models.js";

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "qwen3.5-27b-opus:latest";

const SCOPE_QUERY =
  "SELECT ?speaker ?text WHERE { ?m <ns://body> ?text . ?m <ns://author> ?speaker . } ORDER BY ?m";

function withTimeout<T>(pr: Promise<T>, ms: number, msg: string): Promise<T> {
  return Promise.race([
    pr,
    new Promise<T>((_, rej) => setTimeout(() => rej(new Error(msg)), ms)),
  ]);
}

describe("perspective.addAutoProcessor (WS + real LLM)", function () {
  this.timeout(1_200_000);

  let ad4m: Ad4mClient;
  let stop: (() => Promise<void>) | null = null;
  let p: PerspectiveProxy;

  before(async () => {
    const agent = await startAgent("auto-processor");
    ad4m = agent.client;
    stop = agent.stop;

    const modelId = await ad4m.ai.addModel({
      name: "interpretation-llm",
      api: { baseUrl: BASE_URL, apiKey: "ollama", model: MODEL, apiType: "OPEN_AI" },
      modelType: "LLM",
    } as any);
    await ad4m.ai.setDefaultModel("LLM", modelId);

    p = await ad4m.perspective.add("auto-processor-test");
    await ConversationSubgroup.register(p);
  });

  after(async () => {
    if (stop) await stop();
  });

  it("runs the whole loop automatically over channel messages and signals each step", async () => {
    // Observe the pass purely through signals; resolve when it completes.
    const steps: string[] = [];
    let resolveProcessed!: (ev: AutoProcessorEvent) => void;
    const processed = new Promise<AutoProcessorEvent>((res) => {
      resolveProcessed = res;
    });
    await p.addAutoProcessorEventListener((ev) => {
      steps.push(ev.step);
      if (ev.step === "processed") resolveProcessed(ev);
    });

    // Register the processor — the executor's watch loop now runs it automatically.
    await p.addAutoProcessor({
      processorId: "flux-channel",
      sourceScopeQuery: SCOPE_QUERY,
      interpretationClasses: ["ns://ConversationSubgroup"],
      debounceMs: 50,
      batchMin: 2,
      batchMax: 32,
      claimTtlMs: 60_000,
    });

    // Post a channel's messages (link pairs) — no manual interpretation call.
    const msgs: [string, string, string][] = [
      ["msg://c1", "did:key:ana", "Our webhook retries keep dropping during payment outages — we lose the failed events."],
      ["msg://c2", "did:key:ben", "Right, the payments queue has no way to replay what got dropped last time."],
    ];
    for (const [uri, author, body] of msgs) {
      await p.add(new Link({ source: uri, predicate: "ns://body", target: `literal:string:${body}` }), "local");
      await p.add(new Link({ source: uri, predicate: "ns://author", target: author }), "local");
    }

    // Await the terminal signal (the loop ticks every 500ms).
    const ev = await withTimeout(processed, 180_000, "auto-processor did not signal `processed`");
    expect(ev.bases.length, "processed signal must carry written bases").to.be.greaterThan(0);
    expect(steps, "must have signalled the batch was ready").to.include("batchReady");
    expect(steps, "must have signalled the LLM was run").to.include("runningInterpretation");

    // The subgroup was created automatically by the loop.
    const subgroups = await ConversationSubgroup.findAll(p);
    expect(subgroups.length, "the channel's subgroup must have been created").to.be.greaterThan(0);
  });
});
