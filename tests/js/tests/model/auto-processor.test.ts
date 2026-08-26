/**
 * perspective.addAutoProcessor — the executor's watch loop end to end over the
 * real WS API, single executor.
 *
 * The clean Flux flow: register a processor + post a channel's messages, then
 * let the executor do everything — gather the transcript, debounce, batch,
 * claim, interpret, write — while the test only *observes* it through
 * `auto-processor-event` signals and the graph it produced. No manual
 * runInterpretation, no transcript wrangling.
 *
 * The batching assertions read `itemIds` off the signals rather than inspecting
 * what the model wrote, so they hold regardless of how the model interprets a
 * batch: `batchReady.itemIds` is the batch the loop assembled, and
 * `processed.itemIds` is the batch a pass retired. Only the smoke test at the
 * top depends on the model producing anything in particular.
 *
 * Requires a reachable LLM (Ollama, OpenAI-compatible). Endpoint + model are
 * env-overridable to match the Rust e2e suite:
 *   INTERPRETATION_E2E_BASE_URL (default http://localhost:11434/v1)
 *   INTERPRETATION_E2E_MODEL    (default gemma3:12b)
 *
 * Run (from tests/js, executor built + `pnpm run prepare-test` once):
 *   pnpm ts-mocha -p tsconfig.json --timeout 1200000 --exit tests/model/auto-processor.test.ts
 */

import { expect } from "chai";
import { Ad4mClient, Link, PerspectiveProxy } from "@coasys/ad4m";
import type { AutoProcessorEvent, AutoProcessorStep } from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { ConversationSubgroup } from "./auto-processor-models.js";

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "gemma3:12b";

// Speaker and timestamp come off the body link's reifier, not an app-level
// `ns://author` predicate: `?timestamp` is required (it is what makes a turn
// identifiable to the processed-turn cursor) and only the reifier carries it.
// So `?speaker` is the DID that *signed* the link, not the `ns://author` target.
const SCOPE_QUERY = `SELECT ?speaker ?text ?timestamp WHERE {
  ?m <ns://body> ?text .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
  ?r <ad4m://ontology/author> ?speaker .
  ?r <ad4m://ontology/timestamp> ?timestamp .
}
ORDER BY ?timestamp`;

const SUBGROUP_CLASS = "ns://ConversationSubgroup";

describe("perspective.addAutoProcessor (WS + LLM)", function () {
  this.timeout(600_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;

  // A fresh perspective per test, so each starts with an empty channel and a
  // clean processed-turn cursor. Removed afterwards so its watch loop stops
  // ticking (and stops calling the model) while later tests run.
  let seq = 0;
  let p: PerspectiveProxy;
  let events: AutoProcessorEvent[];

  before(async () => {
    const agent = await startAgent("auto-processor");
    ad4m = agent.client;
    stopAgent = agent.stop;

    const modelId = await ad4m.ai.addModel({
      name: "interpretation-llm",
      api: { baseUrl: BASE_URL, apiKey: "ollama", model: MODEL, apiType: "OPEN_AI" },
      modelType: "LLM",
    } as any);
    await ad4m.ai.setDefaultModel("LLM", modelId);
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  beforeEach(async () => {
    seq += 1;
    p = await ad4m.perspective.add(`auto-processor-${seq}`);
    await ConversationSubgroup.register(p);
    events = [];
    await p.addAutoProcessorEventListener((e) => events.push(e));
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  // ── helpers ───────────────────────────────────────────────────────────────

  /** Post one channel message. The body link's reifier supplies speaker+timestamp. */
  async function say(text: string) {
    const uri = `msg://${seq}/${Math.random().toString(36).slice(2)}`;
    await p.add(
      new Link({ source: uri, predicate: "ns://body", target: `literal:string:${text}` }),
      "local",
    );
  }

  async function addProcessor(overrides: Record<string, unknown> = {}) {
    await p.addAutoProcessor({
      processorId: `proc-${seq}`,
      sourceScopeQuery: SCOPE_QUERY,
      interpretationClasses: [SUBGROUP_CLASS],
      debounceMs: 100,
      batchMin: 1,
      batchMax: 32,
      claimTtlMs: 60_000,
      ...overrides,
    } as any);
  }

  const steps = () => events.map((e) => e.step);
  /** The batches the loop assembled (before any model call), as turn-id lists. */
  const batches = () => events.filter((e) => e.step === "batchReady").map((e) => e.itemIds);
  /** The batches a pass actually retired. */
  const retired = () => events.filter((e) => e.step === "processed").map((e) => e.itemIds);

  const waitForStep = (step: AutoProcessorStep, timeoutMs = 180_000) =>
    waitUntil(() => steps().includes(step), timeoutMs, `a \`${step}\` signal`);

  // ── the whole loop, end to end ────────────────────────────────────────────

  it("runs the whole loop over channel messages, signals each step, and writes the instance", async () => {
    await addProcessor({ batchMin: 2 });

    await say("Our webhook retries keep dropping during payment outages — we lose the failed events.");
    await say("Right, the payments queue has no way to replay what got dropped last time.");

    await waitForStep("processed");

    // The signals are the loop's own account of what it did.
    expect(steps(), "must have signalled the batch was ready").to.include("batchReady");
    expect(steps(), "must have signalled the claim was won").to.include("claimed");
    expect(steps(), "must have signalled the model call").to.include("runningInterpretation");

    const processed = events.find((e) => e.step === "processed")!;
    expect(processed.itemIds.length, "the pass retired both turns").to.equal(2);
    expect(processed.bases.length, "processed must carry the written bases").to.be.greaterThan(0);

    const subgroups = await ConversationSubgroup.findAll(p);
    expect(subgroups.length, "the channel's subgroup must have been created").to.be.greaterThan(0);
  });

  // ── batching ──────────────────────────────────────────────────────────────

  it("holds a batch below batchMin, then runs it once the threshold is reached", async () => {
    await addProcessor({ batchMin: 3 });

    await say("Our webhook retries keep dropping during payment outages.");
    await say("Right, the payments queue cannot replay what got dropped.");

    // Several ticks and debounce windows pass with the batch still short.
    await new Promise((r) => setTimeout(r, 3_000));
    expect(batches(), "a sub-batchMin batch must not run").to.deep.equal([]);

    await say("Let's add a dead-letter queue and a replay endpoint.");

    await waitForStep("batchReady", 30_000);
    expect(batches()[0].length, "the pass gets all three turns at once").to.equal(3);
  });

  it("flushes a sub-batchMin batch once maxWaitMs elapses", async () => {
    await addProcessor({ batchMin: 5, maxWaitMs: 1_500 });

    await say("Our webhook retries keep dropping during payment outages.");
    await say("Right, the payments queue cannot replay what got dropped.");

    // Same setup as above, except the oldest turn's deadline now rescues the
    // batch instead of it waiting for a third turn that never comes.
    await waitForStep("batchReady", 30_000);
    expect(batches()[0].length).to.equal(2);
  });

  it("never hands a pass more than batchMax turns, and retires each turn exactly once", async () => {
    await addProcessor({ batchMax: 2 });

    const texts = [
      "Our webhook retries keep dropping during payment outages.",
      "Right, the payments queue cannot replay what got dropped.",
      "Let's add a dead-letter queue and a replay endpoint.",
      "Separately, the retro is moved to Thursday morning.",
      "I'll book the room and send the invite.",
    ];
    for (const t of texts) await say(t);

    // Five turns at batchMax 2 takes at least three passes to work through.
    await waitUntil(
      () => retired().flat().length >= texts.length,
      300_000,
      "every turn to be retired by some pass",
    );

    for (const batch of batches()) {
      expect(batch.length, `batch of ${batch.length} exceeds batchMax`).to.be.at.most(2);
    }
    const ids = retired().flat();
    expect(
      new Set(ids).size,
      "no turn may be retired twice — the cursor must exclude what earlier passes consumed",
    ).to.equal(ids.length);
  });

  // ── the processed-turn cursor ─────────────────────────────────────────────

  it("carries only the new turns into a later pass (the processed-turn cursor)", async () => {
    await addProcessor({ batchMin: 2 });

    await say("Our webhook retries keep dropping during payment outages.");
    await say("Right, the payments queue cannot replay what got dropped.");
    await waitUntil(() => retired().length >= 1, 180_000, "the first pass");
    const firstWave = retired()[0];
    expect(firstWave.length).to.equal(2);

    // The scope query re-gathers the whole channel on every tick, so without the
    // cursor this second pass would re-interpret the first wave along with it.
    await say("Separately, the retro is moved to Thursday morning.");
    await say("I'll book the room and send the invite.");
    await waitUntil(() => retired().length >= 2, 180_000, "the second pass");

    const secondWave = retired()[1];
    expect(secondWave.length, "the second pass sees only what arrived after the first").to.equal(2);
    expect(
      secondWave.filter((id) => firstWave.includes(id)),
      "no turn from the first wave may appear in the second",
    ).to.deep.equal([]);
  });

  // ── failure path ──────────────────────────────────────────────────────────

  it("stops before the model when the processor names a class that is not registered", async () => {
    await addProcessor({ interpretationClasses: ["ns://NeverRegistered"] });

    await say("Our webhook retries keep dropping during payment outages.");

    await waitForStep("shapesMissing", 30_000);
    expect(steps(), "the batch was still assembled and claimed").to.include("batchReady");
    expect(steps(), "but interpretation must never be attempted").to.not.include(
      "runningInterpretation",
    );
  });
});
