/**
 * PR #903 — auto-processor observability surface, end to end over the real WS
 * API + real LLM.
 *
 * Two independent switches on `AutoProcessorConfig`:
 *
 *   `emitDebugEvents`  → the pass fires `llmRequestSent` + `llmResponseReceived`
 *                        `auto-processor-event`s carrying the raw prompt /
 *                        response, so a UI can render "waiting on LLM" between
 *                        prompt-send and response-receive.
 *
 *   `persistDebug`     → the pass's `InterpretationRun` node gets
 *                        `debugPrompt` + `debugResponse` scalars written
 *                        (local-only links) for post-hoc inspection.
 *
 * These were coupled behind one `debugMode` scalar pre-#903 (retained here as
 * a legacy alias). The split lets a caller emit without persisting (live
 * observability, no graph-sync payload) or persist without emitting
 * (retrospective only) — the assertion table below is what the split promises.
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

  before(async () => {
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

  it("emits every step in order, publishes neighbourhood claimed→finished, and persists no LLM I/O when both flags are off", async () => {
    await addProcessor();

    await driveOnePass();

    // Every step of a winning-peer pass, in order. `llmRequestSent` and
    // `llmResponseReceived` are intentionally NOT in this subsequence — with
    // `emitDebugEvents: false` (default) they must not fire, and the next
    // assertion pins that.
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
      "no `llmRequestSent` event may fire when both `emitDebugEvents` + `debugMode` are off",
    ).to.have.length(0);
    expect(
      stepsOfKind("llmResponseReceived"),
      "no `llmResponseReceived` event may fire when both `emitDebugEvents` + `debugMode` are off",
    ).to.have.length(0);

    const processed = events.find((e) => e.step === "processed")!;
    expect(processed.itemIds.length, "the pass retired both turns").to.equal(2);
    expect(processed.bases.length, "processed must carry the written bases").to.be.greaterThan(0);

    // Neighbourhood-state stream — perspective-scoped observability. Won pass
    // → claimed then finished, in that order (a Holochain-synced peer's
    // claim would arrive via `link-added`, which this stream deliberately
    // does not cover).
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

    // No `persistDebug` → the run's `debugPrompt` / `debugResponse` must be
    // absent (undefined; the fields simply have no links, not empty strings).
    const runs = await InterpretationRun.findAll(p);
    expect(runs.length, "the pass wrote exactly one InterpretationRun").to.be.greaterThan(0);
    for (const r of runs) {
      expect(r.debugPrompt, "no `persistDebug` → debugPrompt must be absent").to.be.undefined;
      expect(r.debugResponse, "no `persistDebug` → debugResponse must be absent").to.be.undefined;
    }
  });

  // ── 2. persistDebug alone ──────────────────────────────────────────────────

  it("with `persistDebug: true` alone, persists LLM I/O on the InterpretationRun but emits no mid-pass debug events", async () => {
    await addProcessor({ persistDebug: true });

    await driveOnePass();

    // Split promise: persist ≠ emit. Neither of the two live debug events may
    // fire when only `persistDebug` is on.
    expect(
      stepsOfKind("llmRequestSent"),
      "`persistDebug: true` alone must NOT emit `llmRequestSent`",
    ).to.have.length(0);
    expect(
      stepsOfKind("llmResponseReceived"),
      "`persistDebug: true` alone must NOT emit `llmResponseReceived`",
    ).to.have.length(0);

    const runs = await InterpretationRun.findAll(p);
    expect(runs.length, "the pass wrote exactly one InterpretationRun").to.be.greaterThan(0);
    // findAll returns runs in unspecified order across passes; the run this
    // pass wrote is the first (and only) one on this fresh perspective.
    const run = runs[0];
    expect(run.debugPrompt, "`persistDebug: true` → debugPrompt must be persisted").to.be.a(
      "string",
    );
    expect(run.debugPrompt!.length, "debugPrompt must be non-empty").to.be.greaterThan(0);
    expect(run.debugResponse, "`persistDebug: true` → debugResponse must be persisted").to.be.a(
      "string",
    );
    expect(run.debugResponse!.length, "debugResponse must be non-empty").to.be.greaterThan(0);
  });

  // ── 3. emitDebugEvents alone ───────────────────────────────────────────────

  it("with `emitDebugEvents: true` alone, emits llmRequestSent+llmResponseReceived with payloads but does not persist to the InterpretationRun", async () => {
    await addProcessor({ emitDebugEvents: true });

    await driveOnePass();

    const req = events.find((e) => e.step === "llmRequestSent");
    const res = events.find((e) => e.step === "llmResponseReceived");
    expect(req, "`emitDebugEvents: true` must emit `llmRequestSent`").to.exist;
    expect(res, "`emitDebugEvents: true` must emit `llmResponseReceived`").to.exist;

    // Payload placement is asymmetric on purpose (see AutoProcessor.ts):
    // `llmRequestSent` carries `llmInput`, `llmResponseReceived` carries
    // `llmOutput`. Prompt is the pass's raw string fed to the model;
    // response is the raw model output.
    expect(req!.llmInput, "llmRequestSent must carry the raw prompt on llmInput").to.be.a("string");
    expect(req!.llmInput!.length, "llmInput must be non-empty").to.be.greaterThan(0);
    expect(res!.llmOutput, "llmResponseReceived must carry the raw response on llmOutput").to.be.a(
      "string",
    );
    expect(res!.llmOutput!.length, "llmOutput must be non-empty").to.be.greaterThan(0);

    // Order: request before response, and both between `runningInterpretation`
    // and `processed` in the step stream (they wrap the LLM call).
    expect(
      includesInOrder([
        "runningInterpretation",
        "llmRequestSent",
        "llmResponseReceived",
        "processed",
      ]),
      `llmRequestSent must precede llmResponseReceived and both sit inside the pass; got: ${steps().join(" → ")}`,
    ).to.equal(true);

    // No `persistDebug` → the run must NOT carry debugPrompt / debugResponse
    // even though the exact same prompt / response just went out over the
    // event stream. That is the whole point of the split.
    const runs = await InterpretationRun.findAll(p);
    expect(runs.length, "the pass wrote exactly one InterpretationRun").to.be.greaterThan(0);
    for (const r of runs) {
      expect(
        r.debugPrompt,
        "`emitDebugEvents: true` alone must NOT persist debugPrompt to the InterpretationRun",
      ).to.be.undefined;
      expect(
        r.debugResponse,
        "`emitDebugEvents: true` alone must NOT persist debugResponse to the InterpretationRun",
      ).to.be.undefined;
    }
  });

  // ── 4. Both switches on ────────────────────────────────────────────────────

  it("with both `persistDebug` + `emitDebugEvents` true, emits the live events AND persists LLM I/O to the InterpretationRun", async () => {
    await addProcessor({ persistDebug: true, emitDebugEvents: true });

    await driveOnePass();

    const req = events.find((e) => e.step === "llmRequestSent");
    const res = events.find((e) => e.step === "llmResponseReceived");
    expect(req, "must emit `llmRequestSent`").to.exist;
    expect(res, "must emit `llmResponseReceived`").to.exist;
    expect(req!.llmInput!.length).to.be.greaterThan(0);
    expect(res!.llmOutput!.length).to.be.greaterThan(0);

    const runs = await InterpretationRun.findAll(p);
    expect(runs.length).to.be.greaterThan(0);
    const run = runs[0];
    expect(run.debugPrompt, "debugPrompt must be persisted").to.be.a("string");
    expect(run.debugPrompt!.length).to.be.greaterThan(0);
    expect(run.debugResponse, "debugResponse must be persisted").to.be.a("string");
    expect(run.debugResponse!.length).to.be.greaterThan(0);

    // The persisted prompt is a strict superset of / identical to the
    // dispatched prompt — same code path emits both. Assert the run's
    // debugPrompt contains the event's llmInput as a cheap consistency
    // check without pinning byte-exact equality (framing may differ across
    // wire encodings).
    expect(
      run.debugPrompt!.includes(req!.llmInput!.slice(0, 200)),
      "persisted debugPrompt must contain the same prompt that went out over the event stream",
    ).to.equal(true);
  });

  // ── 5. Legacy `debugMode: true` alias ──────────────────────────────────────

  it("with legacy `debugMode: true` alone, both effects light up via the pre-split fallback", async () => {
    // Neither split field is set — the loader must fall back to `debugMode`
    // and expand it into both switches (so pre-split clients still get the
    // coupled behaviour they originally requested).
    await addProcessor({ debugMode: true });

    await driveOnePass();

    // Live events fire (emit fallback).
    const req = events.find((e) => e.step === "llmRequestSent");
    const res = events.find((e) => e.step === "llmResponseReceived");
    expect(req, "legacy `debugMode: true` must still emit `llmRequestSent`").to.exist;
    expect(res, "legacy `debugMode: true` must still emit `llmResponseReceived`").to.exist;
    expect(req!.llmInput!.length).to.be.greaterThan(0);
    expect(res!.llmOutput!.length).to.be.greaterThan(0);

    // And the run persists (persist fallback).
    const runs = await InterpretationRun.findAll(p);
    expect(runs.length).to.be.greaterThan(0);
    const run = runs[0];
    expect(
      run.debugPrompt,
      "legacy `debugMode: true` must still persist debugPrompt on the run",
    ).to.be.a("string");
    expect(run.debugPrompt!.length).to.be.greaterThan(0);
    expect(
      run.debugResponse,
      "legacy `debugMode: true` must still persist debugResponse on the run",
    ).to.be.a("string");
    expect(run.debugResponse!.length).to.be.greaterThan(0);
  });
});
