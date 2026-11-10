// Run with `node --test src/components/aiSetup.test.mjs` (Node 23.6+ strips aiSetup.ts's
// types). The launcher has no test runner of its own; this covers the onboarding choice.
import assert from "node:assert/strict";
import { test } from "node:test";
import {
  basisNote,
  chatModels,
  endpointFor,
  endpointLabel,
  localAction,
  localChatReady,
  parseMaxNumCtx,
  preselectedChat,
  withMaxNumCtx,
} from "./aiSetup.ts";

const ollama = { kind: "OLLAMA", baseUrl: "http://127.0.0.1:11434" };
const speaches = { kind: "OPEN_AI_AUDIO", baseUrl: "http://127.0.0.1:8000" };
const llama = { kind: "LLAMA_SERVER", baseUrl: "http://127.0.0.1:9931" };

test("a server's chat models are the ones discovery says chat", () => {
  const discovered = [
    { id: "llama3.2:3b", capabilities: ["chat"] },
    { id: "nomic-embed-text", capabilities: ["embeddings"] },
    { id: "qwen3:4b", capabilities: ["chat", "embeddings"] },
  ];
  assert.deepEqual(chatModels(discovered), ["llama3.2:3b", "qwen3:4b"]);
});

test("onboarding preselects the first chat model a detected server already serves", () => {
  const servedChat = {
    [speaches.baseUrl]: [],
    [ollama.baseUrl]: ["llama3.2:3b", "qwen3:4b"],
    [llama.baseUrl]: ["gemma"],
  };
  assert.deepEqual(preselectedChat([speaches, ollama, llama], servedChat), {
    server: ollama,
    remoteModel: "llama3.2:3b",
  });
});

test("nothing is preselected when no server serves a chat model yet", () => {
  assert.equal(preselectedChat([ollama, speaches], { [ollama.baseUrl]: [] }), null);
  assert.equal(preselectedChat([], {}), null);
});

test("a managed endpoint is named by the runtime local AI runs, a user's by its kind", () => {
  assert.equal(endpointLabel({ kind: "OPEN_AI_AUDIO", managed: true }), "nemo-speech (Parakeet)");
  assert.equal(endpointLabel({ kind: "LLAMA_SERVER", managed: true }), "llama-server");
  assert.equal(endpointLabel({ kind: "OPEN_AI_AUDIO", managed: false }), "Transcription server (speaches, Parakeet)");
  assert.equal(endpointLabel({ kind: "OLLAMA", managed: false }), "Ollama");
});

test("a context-window ceiling is a whole number that fits a u32, or nothing", () => {
  assert.equal(parseMaxNumCtx(""), undefined);
  assert.equal(parseMaxNumCtx("  "), undefined);
  assert.equal(parseMaxNumCtx("131072"), 131072);
  assert.equal(parseMaxNumCtx("4294967295"), 4294967295);
  for (const bad of ["0", "-1", "1.5", "4294967296", "lots"]) {
    assert.equal(parseMaxNumCtx(bad), null, bad);
  }
});

test("setting the ceiling keeps the endpoint's other settings and its stored key", () => {
  const endpoint = {
    id: "e1",
    name: "ollama-11434",
    kind: "OLLAMA",
    baseUrl: "http://127.0.0.1:11434",
    hasApiKey: true,
    capabilities: ["chat", "embeddings"],
    maxNumCtx: null,
    requestTimeoutSecs: 600,
    maxConcurrent: null,
    health: { state: "REACHABLE", servedModels: [] },
  };
  const input = withMaxNumCtx(endpoint, 32768);
  assert.deepEqual(input, {
    name: "ollama-11434",
    kind: "OLLAMA",
    baseUrl: "http://127.0.0.1:11434",
    capabilities: ["chat", "embeddings"],
    maxNumCtx: 32768,
    requestTimeoutSecs: 600,
    maxConcurrent: undefined,
  });
  assert.equal("apiKey" in input, false);
});

test("a model the wind tunnel did not measure on this hardware says so; a measured one says nothing", () => {
  assert.equal(basisNote({ basis: "default" }), "not measured on this hardware");
  assert.equal(basisNote({ basis: "measured" }), null);
});

test("a key typed again for a saved endpoint updates that endpoint, not a new one", async () => {
  const saved = {
    id: "e1",
    name: "openai",
    kind: "OPEN_AI",
    baseUrl: "https://api.openai.com/v1/",
    hasApiKey: true,
    capabilities: ["chat", "embeddings"],
    maxNumCtx: null,
    requestTimeoutSecs: 600,
    maxConcurrent: null,
    health: { state: "REACHABLE", servedModels: [] },
  };
  const calls = [];
  const client = {
    ai: {
      endpoints: async () => [saved],
      addEndpoint: async (input) => {
        calls.push(["add", input]);
        return { ...saved, id: "e2", name: input.name };
      },
      updateEndpoint: async (id, input) => {
        calls.push(["update", id, input]);
        return saved;
      },
    },
  };
  const endpoint = await endpointFor(client, "OPEN_AI", "https://api.openai.com/v1", "sk-new");
  assert.equal(endpoint.id, "e1");
  assert.deepEqual(calls, [
    [
      "update",
      "e1",
      {
        name: "openai",
        kind: "OPEN_AI",
        baseUrl: "https://api.openai.com/v1/",
        capabilities: ["chat", "embeddings"],
        maxNumCtx: undefined,
        requestTimeoutSecs: 600,
        maxConcurrent: undefined,
        apiKey: "sk-new",
      },
    ],
  ]);
});

const runtime = (provider, services, phase) => ({ provider, services, phase });
const llamaUp = runtime("llama-server", ["llm", "embeddings"], "running");
const chatModel = { service: "llm", modelId: "qwen", model: "m1" };

test("local chat is ready only with its model registered and its runtime running", () => {
  assert.equal(localChatReady(null), false);
  assert.equal(localChatReady({ models: [chatModel], runtimes: [llamaUp] }), true);
  for (const phase of ["failed", "stopped", "restarting", null]) {
    const runtimes = [runtime("llama-server", ["llm"], phase)];
    assert.equal(localChatReady({ models: [chatModel], runtimes }), false, `${phase}`);
  }
  const unregistered = { ...chatModel, model: null };
  assert.equal(localChatReady({ models: [unregistered], runtimes: [llamaUp] }), false);
});

const planWith = (services, downloadBytes = 0) => ({
  models: services.map((service) => ({ service })),
  downloadBytes,
});

test("the local panel offers start, install, or says local AI runs", () => {
  const installed = (runtimes, models) => ({ installed: true, runtimes, models });
  const all = planWith(["llm", "embeddings", "stt"]);
  // Everything of the selection runs, chat among it.
  assert.equal(localAction(installed([llamaUp], [chatModel]), all, false), "running");
  // Stopped, and the files are in: start.
  const stopped = runtime("llama-server", ["llm"], "stopped");
  assert.equal(localAction(installed([stopped], [chatModel]), all, false), "start");
  // Only embeddings run (first-use setup): the planned chat model is offered for install,
  // also when its files are downloaded already.
  const embeddingsOnly = installed(
    [runtime("llama-server", ["embeddings"], "running")],
    [{ service: "embeddings", modelId: "nomic", model: "m2" }]
  );
  assert.equal(localAction(embeddingsOnly, all, false), "install");
  // The chat model's runtime runs but its model row is not registered: install repairs it.
  const unregistered = installed([llamaUp], [{ ...chatModel, model: null }]);
  assert.equal(localAction(unregistered, all, false), "install");
  // Nothing installed, or files missing: install.
  assert.equal(localAction({ installed: false, runtimes: [], models: [] }, all, false), "install");
  assert.equal(localAction(installed([stopped], [chatModel]), planWith(["llm"], 10), false), "install");
});
