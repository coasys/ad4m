/**
 * Two-executor auto-processor integration: the `ProcessingClaim` must stop the
 * SAME channel batch from being processed twice across two real ad4m executors
 * sharing a neighbourhood (link language). This is the full-integration mirror
 * of the Rust `auto_processor_two_users_one_executor_no_double_processing` test.
 *
 * Flow (mirrors ./neighbourhood.ts's publish/join pattern):
 *   1. Alice creates a perspective and publishes it as a neighbourhood.
 *   2. Bob joins → both share the graph, so claim + subgroup links sync.
 *   3. Both register the ConversationSubgroup class + the LLM; Alice registers
 *      one auto-processor (its config syncs to Bob).
 *   4. Alice posts a channel's messages (they sync to Bob).
 *   5. Both executors' watch loops run. The claim coordinates: exactly one
 *      executor processes the batch; the other backs off.
 *   6. Assert exactly ONE ConversationSubgroup exists (no duplicate), and the
 *      `auto-processor-event` stream shows one `processed` and one `backedOff`,
 *      each tagged with its agentDid.
 *
 * NOTE: requires two executors + Holochain + a reachable LLM (Marvin/Ollama).
 * Model/endpoint via INTERPRETATION_E2E_MODEL / INTERPRETATION_E2E_BASE_URL.
 */
import { PerspectiveProxy, Perspective, Link } from "@coasys/ad4m";
import type { AutoProcessorEvent } from "@coasys/ad4m";
import { TestContext } from "./integration.test";
import { sleep } from "../utils/utils";
import fs from "fs";
import { v4 as uuidv4 } from "uuid";
import { expect } from "chai";
import { ConversationSubgroup } from "./model/auto-processor-models";

const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "qwen3.5-27b-opus:latest";
const SCOPE_QUERY =
  "SELECT ?speaker ?text WHERE { ?m <ns://body> ?text . ?m <ns://author> ?speaker . } ORDER BY ?m";

async function registerLlm(ad4m: any): Promise<void> {
  const modelId = await ad4m.ai.addModel({
    name: "interpretation-llm",
    api: { baseUrl: BASE_URL, apiKey: "ollama", model: MODEL, apiType: "OPEN_AI" },
    modelType: "LLM",
  });
  await ad4m.ai.setDefaultModel("LLM", modelId);
}

export default function autoProcessorNeighbourhoodTests(testContext: TestContext) {
  return () => {
    describe("Auto-processor across two executors", () => {
      it("processes a shared channel exactly once (claim coordinates)", async () => {
        const alice = testContext.alice;
        const bob = testContext.bob;

        await registerLlm(alice);
        await registerLlm(bob);

        // 1. Alice creates + publishes a neighbourhood.
        const aliceHandle = await alice.perspective.add("ap-channel");
        const socialContext = await alice.languages.applyTemplateAndPublish(
          DIFF_SYNC_OFFICIAL,
          JSON.stringify({ uid: uuidv4(), name: "auto-processor neighbourhood" }),
        );
        const url = await alice.neighbourhood.publishFromPerspective(
          aliceHandle.uuid,
          socialContext.address,
          new Perspective(),
        );

        // 2. Bob joins; make nodes known + let the link language connect.
        const bobHandle = await bob.neighbourhood.joinFromUrl(url);
        await testContext.makeAllNodesKnown();
        await sleep(2000);

        const aliceP = (await alice.perspective.byUUID(aliceHandle.uuid)) as PerspectiveProxy;
        const bobP = (await bob.perspective.byUUID(bobHandle.uuid)) as PerspectiveProxy;

        // 3. Both register the class; collect signals from both executors.
        await ConversationSubgroup.register(aliceP);
        await ConversationSubgroup.register(bobP);

        const events: AutoProcessorEvent[] = [];
        await aliceP.addAutoProcessorEventListener((e) => events.push(e));
        await bobP.addAutoProcessorEventListener((e) => events.push(e));

        // Alice registers one processor; the config syncs to Bob.
        await aliceP.addAutoProcessor({
          processorId: "flux-channel",
          sourceScopeQuery: SCOPE_QUERY,
          interpretationClasses: ["ns://ConversationSubgroup"],
          debounceMs: 200,
          batchMin: 2,
          batchMax: 32,
          claimTtlMs: 60_000,
        });
        await sleep(2000);

        // 4. Alice posts the channel's messages (shared → sync to Bob).
        const msgs: [string, string, string][] = [
          ["msg://c1", "did:key:ana", "Our webhook retries keep dropping during payment outages — we lose the failed events."],
          ["msg://c2", "did:key:ben", "Right, the payments queue has no way to replay what got dropped last time."],
        ];
        for (const [uri, author, body] of msgs) {
          await aliceP.add(new Link({ source: uri, predicate: "ns://body", target: `literal:string:${body}` }));
          await aliceP.add(new Link({ source: uri, predicate: "ns://author", target: author }));
        }

        // 5. Let both watch loops run + the writes sync.
        // Poll until exactly one subgroup is visible (both executors' views must agree).
        let subgroups: ConversationSubgroup[] = [];
        for (let i = 0; i < 60; i++) {
          await sleep(2000);
          subgroups = await ConversationSubgroup.findAll(aliceP);
          const processedCount = events.filter((e) => e.step === "processed").length;
          if (processedCount >= 1 && subgroups.length >= 1) break;
        }

        // 6. The load-bearing assertion: exactly ONE subgroup — the claim stopped
        // the two executors from both minting one for the same batch.
        expect(subgroups.length, `expected exactly 1 subgroup, got ${subgroups.length}`).to.equal(1);

        // Corroborate the coordination via signals: exactly one executor
        // `processed`; the other `backedOff` (or was never a candidate).
        const processedDids = new Set(
          events.filter((e) => e.step === "processed" && e.agentDid).map((e) => e.agentDid),
        );
        const backedOffDids = new Set(
          events.filter((e) => e.step === "backedOff" && e.agentDid).map((e) => e.agentDid),
        );
        expect(processedDids.size, "exactly one executor should have processed").to.equal(1);
        expect(
          backedOffDids.size >= 1 ||
            events.some((e) => e.step === "notCandidate"),
          "the other executor must have backed off / stood down",
        ).to.be.true;
      });
    });
  };
}
