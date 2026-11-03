// Run with `node --test src/components/aiSetup.test.mjs` (Node 23.6+ strips aiSetup.ts's
// types). The launcher has no test runner of its own; this covers the onboarding choice.
import assert from "node:assert/strict";
import { test } from "node:test";
import { chatModels, parseMaxNumCtx, preselectedChat, withMaxNumCtx } from "./aiSetup.ts";

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
