/**
 * ai.session* — persistent AI sessions over the real WS API and a real LLM.
 *
 * Covers the whole client surface (`sessionCreate`/`session`/`sessionList`/
 * `sessionUpdate`/`sessionDelete`, `sendMessage`/`cancelTurn`/`sessionMessages`,
 * `onSessionChanged`) plus four user-story scenarios through the session's
 * MCP tool surface:
 *
 *   - "what are my perspectives?"      → a `list_perspectives` tool call
 *   - "what is in my perspective?"     → tools aimed at `defaultPerspective`
 *   - "add this link / read it back"   → `add_link`, then `query_links`
 *   - "this perspective only"          → a session scoped to one perspective
 *     lists only it and is refused on another
 *
 * Tool steps land as `role: "tool"` transcript entries (`content.name`,
 * `content.arguments`, `content.target`, `content.result`), so the scenarios
 * assert on the steps the turn took and on the perspective state through the
 * TS client — not on the model's wording (the one exception is the
 * list_perspectives reply, where the two distinctive names are checked).
 *
 * Requires a reachable LLM endpoint. Endpoint + model are env-overridable to
 * match the Rust e2e suite:
 *   INTERPRETATION_E2E_BASE_URL (default http://localhost:11434/v1)
 *   INTERPRETATION_E2E_MODEL    (default qwen3.8-27b-48k)
 *   INTERPRETATION_E2E_MAX_NUM_CTX (default 49152, the context the model
 *     was created with; matching it keeps Ollama from reloading the model)
 *   INTERPRETATION_E2E_API_TYPE (default OLLAMA — the native /api/chat wire
 *     carries tools as data. OPEN_AI takes the prompt-injection path, which
 *     Ollama 0.40 + qwen3.8 crashes on mid-generation: HTTP 500 "EOF")
 *   INTERPRETATION_E2E_API_KEY  (only if the endpoint requires it; omitted
 *     otherwise, so plain-HTTP endpoints are not refused the key)
 *
 * Run (from tests/js, executor built + `pnpm run prepare-test` once):
 *   LLM_E2E=1 pnpm ts-mocha -p tsconfig.json --timeout 1200000 --exit \
 *     --require tests/model/hooks.ts tests/model/ai-sessions.test.ts
 */

import * as chai from "chai";
import chaiAsPromised from "chai-as-promised";
import { Ad4mClient, LinkQuery } from "@coasys/ad4m";
import type { AISession, AISessionChanged, AISessionMessage } from "@coasys/ad4m";
import { startAgent, waitUntil } from "../../helpers/index.js";

const expect = chai.expect;
chai.use(chaiAsPromised);

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "qwen3.8-27b-48k";
const MAX_NUM_CTX = Number(process.env.INTERPRETATION_E2E_MAX_NUM_CTX) || 49152;
const API_TYPE = process.env.INTERPRETATION_E2E_API_TYPE || "OLLAMA";
const API_KEY = process.env.INTERPRETATION_E2E_API_KEY;

// A turn is a detached task; the reply row settles whenever the model is done.
// Ollama needs a cold model load (~15 s) plus generation time per turn.
const TURN_TIMEOUT_MS = 300_000;

/** The fields the test reads off a transcript entry's `content`. */
type StepContent = {
  name?: string;
  arguments?: Record<string, unknown>;
  target?: string;
  result?: string;
  error?: string;
  text?: string;
};

const stepContent = (m: AISessionMessage): StepContent =>
  (m.content ?? {}) as StepContent;

const toolSteps = (msgs: AISessionMessage[], name?: string) =>
  msgs.filter(
    (m) => m.role === "tool" && (!name || stepContent(m).name === name),
  );

// LLM E2E gate — this suite drives the executor's real LLM path. Skipped by
// default (unset `LLM_E2E`); the nightly `llm-e2e` workflow on `dev` runs it
// against Marvin's Ollama. Run locally with `LLM_E2E=1`, or the umbrella
// `./scripts/run-llm-e2e.sh` at the repo root.
const describeIfLLM: Mocha.SuiteFunction = (process.env.LLM_E2E === "1"
  ? describe
  : (describe.skip as unknown as Mocha.SuiteFunction));

describeIfLLM("ai.session* — persistent AI sessions (WS + real LLM)", function () {
  this.timeout(900_000);

  let ad4m: Ad4mClient;
  let stopAgent: (() => Promise<void>) | null = null;

  // Perspectives and sessions each test leaves behind are removed afterwards,
  // so list_perspectives and sessionList answers stay small and one failing
  // test does not poison the next.
  const perspectiveIds: string[] = [];
  const sessionIds: string[] = [];

  before(async () => {
    const agent = await startAgent("ai-sessions");
    ad4m = agent.client;
    stopAgent = agent.stop;

    const modelId = await ad4m.ai.addModel({
      name: "ai-sessions-llm",
      api: {
        baseUrl: BASE_URL,
        apiKey: API_KEY,
        model: MODEL,
        apiType: API_TYPE,
        maxNumCtx: MAX_NUM_CTX,
      },
      modelType: "LLM",
    } as any);
    await ad4m.ai.setDefaultModel("LLM", modelId);
  });

  after(async () => {
    if (stopAgent) await stopAgent();
  });

  afterEach(async () => {
    for (const id of sessionIds.splice(0)) {
      await ad4m.ai.sessionDelete(id).catch(() => {});
    }
    for (const uuid of perspectiveIds.splice(0)) {
      await ad4m.perspective.remove(uuid).catch(() => {});
    }
  });

  // ── helpers ───────────────────────────────────────────────────────────────

  async function newSession(input: Parameters<typeof ad4m.ai.sessionCreate>[0] = {}) {
    const s = await ad4m.ai.sessionCreate(input);
    sessionIds.push(s.id);
    return s;
  }

  async function newPerspective(name: string) {
    const p = await ad4m.perspective.add(name);
    perspectiveIds.push(p.uuid);
    return p;
  }

  /** The transcript once `replyId` has left `running` — polls the read API. */
  async function settledMessages(sessionId: string, replyId: string) {
    let msgs: AISessionMessage[] = [];
    await waitUntil(
      async () => {
        msgs = await ad4m.ai.sessionMessages(sessionId);
        const reply = msgs.find((m) => m.id === replyId);
        return !!reply && !!reply.status && reply.status !== "running";
      },
      TURN_TIMEOUT_MS,
      `turn ${replyId} to settle`,
    );
    return msgs;
  }

  /** The reply row of a settled turn. */
  async function waitForReply(sessionId: string, replyId: string) {
    const msgs = await settledMessages(sessionId, replyId);
    const reply = msgs.find((m) => m.id === replyId)!;
    expect(
      reply.status,
      `the turn failed: ${reply.error ?? "no error recorded"}`,
    ).to.equal("done");
    return { msgs, reply };
  }

  /** `message` events a session produced, in arrival order. */
  const messageEventsFor = (events: AISessionChanged[], sessionId: string) =>
    events.filter((e) => e.sessionId === sessionId && e.kind === "message" && e.message);

  // ── lifecycle ─────────────────────────────────────────────────────────────

  it("creates, reads, updates and deletes a session", async () => {
    const p = await newPerspective("ai-sessions-crud-anchor");

    const s = await newSession({
      title: "first session",
      defaultPerspective: p.uuid,
      model: "not-a-real-model",
    });
    expect(s.title).to.equal("first session");
    expect(s.defaultPerspective).to.equal(p.uuid);
    expect(s.model).to.equal("not-a-real-model");

    // Reads: by id and in the list.
    expect((await ad4m.ai.session(s.id)).id).to.equal(s.id);
    expect((await ad4m.ai.sessionList()).map((x) => x.id)).to.include(s.id);

    // An update touches only the fields it names.
    const renamed = await ad4m.ai.sessionUpdate(s.id, { title: "renamed session" });
    expect(renamed.title).to.equal("renamed session");
    expect(renamed.defaultPerspective).to.equal(p.uuid);

    // `null` clears a nullable field; an absent key keeps its value.
    const cleared = await ad4m.ai.sessionUpdate(s.id, {
      defaultPerspective: null,
      model: null,
    });
    expect(cleared.defaultPerspective).to.be.null;
    expect(cleared.model).to.be.null;
    expect(cleared.title).to.equal("renamed session");

    expect(await ad4m.ai.sessionDelete(s.id)).to.equal(true);
    expect((await ad4m.ai.sessionList()).map((x) => x.id)).to.not.include(s.id);
    await expect(ad4m.ai.session(s.id)).to.be.rejectedWith(/404|not found/i);
  });

  it("streams each change as an ai-session-changed event and stops on unsubscribe", async () => {
    const events: AISessionChanged[] = [];
    const unsub = ad4m.ai.onSessionChanged((e) => events.push(e));

    const s = await newSession({ title: "watched session" });
    await waitUntil(
      () => events.some((e) => e.sessionId === s.id && e.kind === "created"),
      10_000,
      "the `created` event",
    );

    // The events that matter during a turn: the person's message, the reply
    // opening `running`, each tool step, the reply settling.
    const started = await ad4m.ai.sendMessage(s.id, "Reply with exactly one word: ready");
    await waitUntil(
      () =>
        messageEventsFor(events, s.id).some(
          (e) => e.message!.id === started.replyId && e.message!.status === "done",
        ),
      TURN_TIMEOUT_MS,
      "the reply's `done` event",
    );

    const ids = messageEventsFor(events, s.id).map((e) => e.message!.id);
    expect(ids, "the person's message was announced").to.include(started.messageId);
    const replyEvents = messageEventsFor(events, s.id).filter(
      (e) => e.message!.id === started.replyId,
    );
    expect(
      replyEvents.map((e) => e.message!.status),
      "the reply was announced running, then settled done",
    ).to.include.members(["running", "done"]);

    unsub();

    // After unsubscribing nothing more arrives — a fresh session's `created`
    // event would land here within the wait if delivery had not stopped.
    const seen = events.length;
    await newSession({ title: "unwatched session" });
    await new Promise((r) => setTimeout(r, 800));
    expect(events.length, "no events after unsubscribe").to.equal(seen);
  });

  it("stores a turn: the user's message and the assistant's reply, in order", async () => {
    const s = await newSession({ title: "one turn" });
    const prompt = "Reply with exactly one word: ready";

    const started = await ad4m.ai.sendMessage(s.id, prompt);
    const { msgs, reply } = await waitForReply(s.id, started.replyId);

    // seq is the transcript's write order: person first, then the turn's row.
    const seqs = msgs.map((m) => m.seq);
    expect(seqs).to.deep.equal([...seqs].sort((a, b) => a - b));

    const user = msgs.find((m) => m.id === started.messageId)!;
    expect(user.role).to.equal("user");
    expect(stepContent(user).text).to.equal(prompt);

    expect(reply.role).to.equal("assistant");
    expect(reply.seq, "the reply follows the message").to.be.greaterThan(user.seq);
    const text = stepContent(reply).text ?? "";
    expect(text.trim().length, "the reply is not empty").to.be.greaterThan(0);
  });

  it("answers false for a cancel with nothing running, and cancels a running turn", async () => {
    const s = await newSession({ title: "cancel turn" });

    // Idle: nothing to end, and nothing was interrupted either.
    expect(await ad4m.ai.cancelTurn(s.id)).to.equal(false);

    // Running: send a prompt the model cannot finish in one word, then cancel.
    const started = await ad4m.ai.sendMessage(
      s.id,
      "Write a long detailed essay about the history of computing, at least 400 words.",
    );
    expect(await ad4m.ai.cancelTurn(s.id)).to.equal(true);

    const msgs = await settledMessages(s.id, started.replyId);
    const reply = msgs.find((m) => m.id === started.replyId)!;
    expect(reply.status).to.equal("cancelled");
  });

  // ── user stories through the session's tools ──────────────────────────────

  it("lists the user's perspectives when asked (list_perspectives)", async () => {
    // Two distinctive names the reply must be able to read back.
    const nameA = `quokka-notes-${Date.now().toString(36)}`;
    const nameB = `zephyr-ideas-${Date.now().toString(36)}`;
    await newPerspective(nameA);
    await newPerspective(nameB);

    const s = await newSession({ title: "list my perspectives" });
    const started = await ad4m.ai.sendMessage(
      s.id,
      "Use the list_perspectives tool to list my perspectives, then tell me the names you see.",
    );
    const { msgs, reply } = await waitForReply(s.id, started.replyId);

    const calls = toolSteps(msgs, "list_perspectives");
    expect(calls.length, "the turn called list_perspectives").to.be.greaterThan(0);
    expect(calls[0].status).to.equal("done");

    // The reply says what the tool returned: both names are in it.
    const text = stepContent(reply).text ?? "";
    expect(text, "the reply mentions the first perspective").to.include(nameA);
    expect(text, "the reply mentions the second perspective").to.include(nameB);
  });

  it("works on the session's defaultPerspective without being given the uuid", async () => {
    const p = await newPerspective("ai-sessions-default-target");
    const s = await newSession({ title: "current perspective" });

    // The user story: pick the perspective once on the session, then talk
    // about "my current perspective" — never the uuid.
    await ad4m.ai.sessionUpdate(s.id, { defaultPerspective: p.uuid });

    const started = await ad4m.ai.sendMessage(
      s.id,
      "What is in my current perspective? Use the query_links tool to look.",
    );
    const { msgs } = await waitForReply(s.id, started.replyId);

    const calls = toolSteps(msgs).filter((m) =>
      ["query_links", "describe_perspective"].includes(stepContent(m).name ?? ""),
    );
    expect(
      calls.length,
      "the turn inspected a perspective (query_links or describe_perspective)",
    ).to.be.greaterThan(0);
    expect(
      calls.some(
        (m) => stepContent(m).arguments?.perspective_id === p.uuid,
      ),
      "the tool call targeted the defaultPerspective's uuid",
    ).to.equal(true);
  });

  it("adds a link to the perspective and reads it back (add_link + query_links)", async () => {
    const p = await newPerspective("ai-sessions-write-target");
    const s = await newSession({
      title: "write and read",
      defaultPerspective: p.uuid,
    });

    // The user story, said as a user would: an exact triple into the session's
    // perspective, then read it back.
    const started = await ad4m.ai.sendMessage(
      s.id,
      'Use the add_link tool to add this link to my current perspective: ' +
        'source "songbird://first-note", predicate "ad4m://title", ' +
        'target "literal:string:Ruby Kookaburra".',
    );
    const { msgs } = await waitForReply(s.id, started.replyId);

    expect(
      toolSteps(msgs, "add_link").length,
      "the turn called add_link",
    ).to.be.greaterThan(0);

    // The write, checked on the graph — not on what the model said it did.
    const links = await p.get(new LinkQuery({ source: "songbird://first-note" }));
    expect(links.length, "the link is in the perspective").to.be.greaterThan(0);
    const link = links.find((l) => l.data.predicate === "ad4m://title");
    expect(link, "a ad4m://title link from that source").to.not.be.undefined;
    expect(link!.data.target).to.include("Ruby Kookaburra");

    const read = await ad4m.ai.sendMessage(
      s.id,
      'Use the query_links tool to read back the links with source "songbird://first-note" ' +
        "from the same perspective.",
    );
    const { msgs: msgs2 } = await waitForReply(s.id, read.replyId);

    // Only the second turn's steps: the first turn may have queried too.
    const readSeq = msgs2.find((m) => m.id === read.messageId)!.seq;
    const queries = toolSteps(msgs2, "query_links").filter((m) => m.seq > readSeq);
    expect(queries.length, "the turn called query_links").to.be.greaterThan(0);
    expect(
      queries.some(
        (m) => stepContent(m).arguments?.perspective_id === p.uuid,
      ),
      "query_links targeted the session's perspective",
    ).to.equal(true);
  });

  it("reaches a perspective named in the message (list_perspectives → query_links)", async () => {
    await newPerspective("ai-sessions-decoy");
    const named = `kestrel-board-${Date.now().toString(36)}`;
    const p = await newPerspective(named);

    const s = await newSession({ title: "pick by name" });
    const started = await ad4m.ai.sendMessage(
      s.id,
      `In the perspective called "${named}", what links does it hold? ` +
        "Use the query_links tool on it — find its uuid first if you need it.",
    );
    const { msgs } = await waitForReply(s.id, started.replyId);

    const queries = toolSteps(msgs, "query_links");
    expect(queries.length, "the turn called query_links").to.be.greaterThan(0);
    expect(
      queries.some(
        (m) => stepContent(m).arguments?.perspective_id === p.uuid,
      ),
      "query_links targeted the perspective the user named",
    ).to.equal(true);
  });

  // ── a session scoped to one perspective ───────────────────────────────────

  it("a session scoped to one perspective lists and reads only that one", async () => {
    const inside = await newPerspective(`otter-inside-${Date.now().toString(36)}`);
    const outside = await newPerspective(`heron-outside-${Date.now().toString(36)}`);

    // The scope a client gives a "this space only" chat: read and write on one
    // perspective, nothing on any other.
    const s = await newSession({
      title: "one perspective only",
      defaultPerspective: inside.uuid,
      capabilities: [
        {
          with: { domain: "perspective", pointers: [inside.uuid] },
          can: ["READ", "UPDATE"],
        },
      ],
    });

    // Listing is admitted and narrowed: the scoped session sees its own
    // perspective, not the other one (before the fix it was refused outright).
    const listed = await ad4m.ai.sendMessage(
      s.id,
      "Use the list_perspectives tool to list my perspectives, then tell me the names you see.",
    );
    const { msgs } = await waitForReply(s.id, listed.replyId);
    const lists = toolSteps(msgs, "list_perspectives");
    expect(lists.length, "the turn called list_perspectives").to.be.greaterThan(0);
    expect(lists[0].status, `list_perspectives: ${stepContent(lists[0]).error}`).to.equal("done");
    // Parsed, not substring-matched: a refusal's text echoes the caller's own
    // capabilities, which name `inside` too.
    const handles = JSON.parse(stepContent(lists[0]).result ?? "null");
    expect(handles, "list_perspectives returned a list").to.be.an("array");
    const uuids = (handles as { uuid: string }[]).map((h) => h.uuid);
    expect(uuids, "the scoped perspective is listed").to.include(inside.uuid);
    expect(uuids, "the other perspective is not listed").to.not.include(outside.uuid);

    // Reading the other perspective, even by its exact uuid, is refused.
    const probe = await ad4m.ai.sendMessage(
      s.id,
      `Use the query_links tool with perspective_id "${outside.uuid}" and no other filters.`,
    );
    const { msgs: msgs2 } = await waitForReply(s.id, probe.replyId);
    const probeSeq = msgs2.find((m) => m.id === probe.messageId)!.seq;
    const outsideQueries = toolSteps(msgs2, "query_links").filter(
      (m) => m.seq > probeSeq && stepContent(m).arguments?.perspective_id === outside.uuid,
    );
    expect(outsideQueries.length, "the turn tried the other perspective").to.be.greaterThan(0);
    for (const q of outsideQueries) {
      expect(q.status, "query_links on the other perspective is refused").to.equal("failed");
      expect(stepContent(q).error ?? "").to.include("Capability is not matched");
      expect(stepContent(q).result, "a refused step carries no result").to.be.undefined;
    }
  });
});
