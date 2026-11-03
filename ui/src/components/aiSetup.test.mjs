// Run with `node --test src/components/aiSetup.test.mjs` (Node 23.6+ strips aiSetup.ts's
// types). The launcher has no test runner of its own; this covers the onboarding choice.
import assert from "node:assert/strict";
import { test } from "node:test";
import { chatModels, endpointLabel, preselectedChat } from "./aiSetup.ts";

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
