/**
 * PR #903 — auto-processor observability surface, end to end over the real WS
 * API + real LLM.
 *
 * A single `emitDebugEvents` flag on `AutoProcessorConfig`:
 *
 *   `emitDebugEvents: true` → the pass fires `llmRequestSent` +
 *                             `llmResponseReceived` `auto-processor-event`s
 *                             carrying the raw prompt / response, AND persists
 *                             `debugPrompt` + `debugResponse` on the
 *                             `InterpretationRun` node.
 *
 * Also covers the full step-signal lifecycle (`batchReady` → `claimed` →
 * `gatheringTranscript` → `runningInterpretation` → `processed`) and the
 * perspective-scoped `auto-processor-neighbourhood-state` stream (`claimed`
 * → `finished`), which #903 rides.
 *
 * Requires a reachable LLM (Ollama over the OpenAI-compatible API). Endpoint
 * + model env-overridable to match the Rust e2e suite:
 *   INTERPRETATION_E2E_BASE_URL (default http://localhost:11434/v1)
 *   INTERPRETATION_E2E_MODEL    (default gemma3:12b)
 *
 * Run (from tests/js, executor built + `pnpm run prepare-test` once):
 *   pnpm ts-mocha -p tsconfig.json --timeout 1200000 --exit \
 *     tests/model/auto-processor-observability.test.ts
 */

import { expect } from "chai";
import {
  Ad4mClient,
  InterpretationRun,
  Link,
  PerspectiveProxy,
} from "@coasys/ad4m";
import type {
  AutoProcessorEvent,
  AutoProcessorNeighbourhoodStateEvent,
  AutoProcessorStep,
} from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";
import { ConversationSubgroup } from "./auto-processor-models.js";

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "gemma3:12b";

// Same scope query the other WS auto-processor tests use — reifier carries
// author + timestamp, `?timestamp` is what makes a turn identifiable to the
// processed-turn cursor.
const SCOPE_QUERY = `SELECT ?speaker ?text ?timestamp WHERE {
  ?m <ns://body> ?text .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
  ?r <ad4m://ontology/author> ?speaker .
  ?r <ad4m://ontology/timestamp> ?timestamp .
}
ORDER BY ?timestamp`;

const SUBGROUP_CLASS = "ns://ConversationSubgroup";

describe("AutoProcessor observability — events + debug output (PR #903)", function () {
  this.timeout(900_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;

  // Fresh perspective per test so each starts with an empty channel + a clean
  // processed-turn cursor, and removed afterwards so its watch loop stops
  // ticking (and stops calling the model) while later tests run.
  let seq = 0;
  let p: PerspectiveProxy;
  let events: AutoProcessorEvent[];
  let nbEvents: AutoProcessorNeighbourhoodStateEvent[];

  before(async function () {
    // LLM availability gate — this suite needs a reachable OpenAI-compatible
    // endpoint at BASE_URL hosting MODEL. Skip cleanly on runners without it
    // (mirrors the gate in run-interpretation-harness.test.ts).
    try {
      const probe = await fetch(BASE_URL.replace(/\/v1\/?$/, "") + "/v1/models", {
        signal: AbortSignal.timeout(3000),
      });
      if (!probe.ok) throw new Error(`probe ${probe.status}`);
      const body = (await probe.json()) as { data?: Array<{ id?: string }> };
      const ids = (body.data ?? []).map((m) => m.id).filter((id): id is string => !!id);
      if (!ids.includes(MODEL)) {
        throw new Error(`model ${MODEL} not present in /v1/models (have: ${ids.join(", ") || "none"})`);
      }
    } catch (e) {
      console.log(`Skipping auto-processor observability e2e — LLM endpoint ${BASE_URL} unreachable: ${(e as Error).message}`);
      this.skip();
    }

    const agent = await startAgent("auto-processor-observability");
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
    p = await ad4m.perspective.add(`auto-processor-obs-${seq}`);
    // InterpretationRun is registered so `InterpretationRun.findAll(p)` can
    // observe what the pass persisted (or didn't). ConversationSubgroup is
    // the class the pass materialises.
    await ConversationSubgroup.register(p);
    await InterpretationRun.register(p);
    events = [];
    nbEvents = [];
    await p.addAutoProcessorEventListener((e) => events.push(e));
    await p.addAutoProcessorNeighbourhoodStateListener((e) => nbEvents.push(e));
  });

  afterEach(async () => {
    if (p) await ad4m.perspective.remove(p.uuid);
  });

  // ── helpers ────────────────────────────────────────────────────────────────

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
      batchMin: 2,
      batchMax: 32,
      claimTtlMs: 60_000,
      ...overrides,
    } as any);
  }

  /** Seed one full pass (two turns, `batchMin: 2`) and wait until it retires. */
  async function driveOnePass() {
    await say("Our webhook retries keep dropping during payment outages — we lose the failed events.");
    await say("Right, the payments queue has no way to replay what got dropped last time.");
    await waitUntil(
      () => events.some((e) => e.step === "processed"),
      600_000,
      "a `processed` step signal from the pass",
    );
  }

  const steps = () => events.map((e) => e.step);
  const stepsOfKind = (k: AutoProcessorStep) => events.filter((e) => e.step === k);

  /** Order-preserving check: `subseq` appears in `steps()` in that order (contiguity not required). */
  function includesInOrder(subseq: AutoProcessorStep[]) {
    const seen = steps();
    let i = 0;
    for (const s of seen) {
      if (s === subseq[i]) i += 1;
      if (i === subseq.length) return true;
    }
    return false;
  }

  // ── 1. Full lifecycle + neighbourhood-state (no debug) ─────────────────────

  it("emits every step in order, publishes neighbourhood claimed→finished, and persists no LLM I/O when debug is off", async () => {
    await addProcessor();

    await driveOnePass();

    expect(
      includesInOrder([
        "batchReady",
        "claimed",
        "gatheringTranscript",
        "runningInterpretation",
        "processed",
      ]),
      `expected the full winning-peer lifecycle in order; got: ${steps().join(" → ")}`,
    ).to.equal(true);

    expect(
      stepsOfKind("llmRequestSent"),
      "no `llmRequestSent` event may fire when `emitDebugEvents` is off",
    ).to.have.length(0);
    expect(
      stepsOfKind("llmResponseReceived"),
      "no `llmResponseReceived` event may fire when `emitDebugEvents` is off",
    ).to.have.length(0);

    const processed = events.find((e) => e.step === "processed")!;
    expect(processed.itemIds.length, "the pass retired both turns").to.equal(2);
    expect(processed.bases.length, "processed must carry the written bases").to.be.greaterThan(0);

    // Neighbourhood-state stream — perspective-scoped observability.
    await waitUntil(
      () => nbEvents.some((e) => e.phase === "finished"),
      30_000,
      "a `finished` neighbourhood-state event",
    );
    const nbPhases = nbEvents.map((e) => e.phase);
    expect(nbPhases, `expected claimed then finished; got: ${nbPhases.join(", ")}`).to.include(
      "claimed",
    );
    expect(nbPhases).to.include("finished");
    expect(
      nbPhases.indexOf("claimed"),
      "claimed must be emitted before finished",
    ).to.be.lessThan(nbPhases.indexOf("finished"));

    // No debug → no persisted LLM I/O.
    const runs = await InterpretationRun.findAll(p);
    expect(runs.length, "the pass wrote at least one InterpretationRun").to.be.greaterThan(0);
    for (const r of runs) {
      expect(r.debugPrompt, "debugPrompt must be absent when debug is off").to.be.undefined;
      expect(r.debugResponse, "debugResponse must be absent when debug is off").to.be.undefined;
    }
  });

  // ── 2. emitDebugEvents: true ───────────────────────────────────────────────

  it("with `emitDebugEvents: true`, emits llmRequestSent+llmResponseReceived AND persists LLM I/O to the InterpretationRun", async () => {
    await addProcessor({ emitDebugEvents: true });

    await driveOnePass();

    const req = events.find((e) => e.step === "llmRequestSent");
    const res = events.find((e) => e.step === "llmResponseReceived");
    expect(req, "`emitDebugEvents: true` must emit `llmRequestSent`").to.exist;
    expect(res, "`emitDebugEvents: true` must emit `llmResponseReceived`").to.exist;

    expect(req!.llmInput, "llmRequestSent must carry the raw prompt on llmInput").to.be.a("string");
    expect(req!.llmInput!.length, "llmInput must be non-empty").to.be.greaterThan(0);
    expect(res!.llmOutput, "llmResponseReceived must carry the raw response on llmOutput").to.be.a(
      "string",
    );
    expect(res!.llmOutput!.length, "llmOutput must be non-empty").to.be.greaterThan(0);

    expect(
      includesInOrder([
        "runningInterpretation",
        "llmRequestSent",
        "llmResponseReceived",
        "processed",
      ]),
      `llmRequestSent must precede llmResponseReceived and both sit inside the pass; got: ${steps().join(" → ")}`,
    ).to.equal(true);

    // Persists debug output on the InterpretationRun.
    const runs = await InterpretationRun.findAll(p);
    expect(runs.length, "the pass wrote at least one InterpretationRun").to.be.greaterThan(0);
    const run = runs[0];
    expect(run.debugPrompt, "debugPrompt must be persisted").to.be.a("string");
    expect(run.debugPrompt!.length, "debugPrompt must be non-empty").to.be.greaterThan(0);
    expect(run.debugResponse, "debugResponse must be persisted").to.be.a("string");
    expect(run.debugResponse!.length, "debugResponse must be non-empty").to.be.greaterThan(0);

    expect(
      run.debugPrompt!.includes(req!.llmInput!.slice(0, 200)),
      "persisted debugPrompt must contain the same prompt that went out over the event stream",
    ).to.equal(true);
  });

});
