/**
 * Two-executor auto-processor integration: two real ad4m executors sharing a
 * neighbourhood must divide the work rather than duplicate it. This is the
 * full-integration mirror of the Rust
 * `auto_processor_two_users_one_executor_no_double_processing`, which runs both
 * peers in one process; here they are separate executors syncing over a link
 * language, so the claim and the processed-turn cursor have to coordinate
 * through the shared graph with real sync latency in between.
 *
 * Each `it` spins up its OWN fresh neighbourhood (new social-context language,
 * new publish, new join) — clean-slate coordination per test, no cross-test
 * carry-over.
 *
 * Model/endpoint are overridable via INTERPRETATION_E2E_MODEL /
 * INTERPRETATION_E2E_BASE_URL.
 */
import { PerspectiveProxy, Perspective, Link, LinkQuery, InterpretationRun } from "@coasys/ad4m";
import type { AutoProcessorEvent } from "@coasys/ad4m";
import { TestContext } from "./integration.test";
import { pollUntil, sleep } from "../utils/utils";
import { waitUntil } from "../helpers/index";
import fs from "fs";
import { v4 as uuidv4 } from "uuid";
import { expect } from "chai";
import { ConversationSubgroup } from "./model/auto-processor-models";

const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "gemma3:12b";

// Speaker and timestamp come off the body link's reifier, not an app-level
// `ns://author` predicate: `?timestamp` is required (it is what makes a turn
// identifiable to the processed-turn cursor) and only the reifier carries it.
// Note the consequence here: `?speaker` is the DID that *signed* the link, so
// each peer's own messages carry that peer as the speaker — which is what the
// authorship election runs on.
const SCOPE_QUERY = `SELECT ?speaker ?text ?timestamp WHERE {
  ?m <ns://body> ?text .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
  ?r <ad4m://ontology/author> ?speaker .
  ?r <ad4m://ontology/timestamp> ?timestamp .
}
ORDER BY ?timestamp`;

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
    describe("Auto-processor across two executors", function () {
      // Cumulative wait budget in the slowest test:
      //   sharedChannel gossip warm-up (up to 180s)
      // + wave-1 processed (up to 240s)
      // + wave-1 InterpretationRun synced to the other peer (up to 240s)
      // + wave-2 processed (up to 240s)
      // + subgroups converge (up to 120s)
      // = up to 1_060s. Suite timeout 1_500s gives every waitUntil budget
      // enough head-room to report its own diagnostic before Mocha times
      // out the whole test.
      this.timeout(1_500_000);

      /**
       * Fresh neighbourhood per test. Alice publishes, Bob joins, both
       * register the class + the LLM, and Alice registers one processor
       * whose config syncs to Bob. Explicit gossip warm-up verifies the
       * sync path is live before anything critical runs. Returns both
       * proxies plus the merged event stream from both executors.
       */
      async function sharedChannel(processorId: string, config: Record<string, unknown> = {}) {
        const alice = testContext.alice;
        const bob = testContext.bob;

        await registerLlm(alice);
        await registerLlm(bob);

        const aliceHandle = await alice.perspective.add(`ap-channel-${processorId}`);
        const socialContext = await alice.languages.applyTemplateAndPublish(
          DIFF_SYNC_OFFICIAL,
          JSON.stringify({ uid: uuidv4(), name: "auto-processor neighbourhood" }),
        );
        const url = await alice.neighbourhood.publishFromPerspective(
          aliceHandle.uuid,
          socialContext.address,
          new Perspective(),
        );

        const bobHandle = await bob.neighbourhood.joinFromUrl(url);
        await testContext.makeAllNodesKnown();
        await sleep(2000); // K2 space initialization after join + agent info exchange

        const aliceP = (await alice.perspective.byUUID(aliceHandle.uuid)) as PerspectiveProxy;
        const bobP = (await bob.perspective.byUUID(bobHandle.uuid)) as PerspectiveProxy;

        // Explicit gossip warm-up: prove the sync path is up BEFORE the
        // watchers start firing. Fresh neighbourhood, cold gossip peers —
        // without this, later cross-peer expectations time out on cold-start
        // sync rather than reporting the actual state.
        const marker = `warmup://${uuidv4()}`;
        await aliceP.add(
          new Link({ source: marker, predicate: "ns://ping", target: "literal:string:ok" }),
        );
        await waitUntil(
          async () => {
            const links = await bobP.get(new LinkQuery({ source: marker }));
            return links.length > 0;
          },
          180_000,
          "gossip warm-up: Alice's marker link to reach Bob",
        );

        await ConversationSubgroup.register(aliceP);
        await ConversationSubgroup.register(bobP);

        // One merged stream: each executor reports its own passes, tagged with
        // the DID that ran them, so "who did what" is readable from one list.
        const events: AutoProcessorEvent[] = [];
        await aliceP.addAutoProcessorEventListener((e) => events.push(e));
        await bobP.addAutoProcessorEventListener((e) => events.push(e));

        await aliceP.addAutoProcessor({
          processorId,
          sourceScopeQuery: SCOPE_QUERY,
          interpretationClasses: ["ns://ConversationSubgroup"],
          debounceMs: 200,
          batchMin: 2,
          batchMax: 32,
          claimTtlMs: 60_000,
          ...config,
        } as any);
        // Wait for AutoProcessorConfig to sync to Bob so his watcher can load it.
        // The config lives as a subject-class instance at node
        // `ad4m://autoprocessor/{processorId}` — poll for its links on Bob's side.
        await pollUntil(async () => {
            try {
                const bobLinks = await bobP.get(new LinkQuery({ source: `ad4m://autoprocessor/${processorId}` }));
                // The config has 6+ required properties (processor_id, source_scope_query,
                // interpretation_class, debounce_ms, batch_max, claim_ttl_ms). Poll until
                // all required links sync — passing on the first link lets the watcher
                // load a partial config and race the claim mechanism.
                return bobLinks.length >= 6;
            } catch { return false; }
        }, { timeoutMs: 180000, intervalMs: 500, label: "AutoProcessorConfig fully syncs to Bob" });

        return { aliceP, bobP, events };
      }

      async function say(p: PerspectiveProxy, uri: string, body: string) {
        await p.add(new Link({ source: uri, predicate: "ns://body", target: `literal:string:${body}` }));
      }

      /** DID of the agent behind a given executor client. */
      async function agentDid(ad4m: any): Promise<string> {
        const status = await ad4m.agent.status();
        return status?.did || "unknown";
      }

      it("processes a shared channel exactly once (claim coordinates)", async () => {
        const { aliceP, events } = await sharedChannel("flux-channel");

        await say(
          aliceP,
          "msg://c1",
          "Our webhook retries keep dropping during payment outages — we lose the failed events.",
        );
        await say(
          aliceP,
          "msg://c2",
          "Right, the payments queue has no way to replay what got dropped last time.",
        );

        // Wait until at least one subgroup is visible — same 4-minute budget as
        // the wave-division test uses.
        let subgroups: ConversationSubgroup[] = [];
        await waitUntil(
          async () => {
            subgroups = await ConversationSubgroup.findAll(aliceP);
            const processedCount = events.filter((e) => e.step === "processed").length;
            return processedCount >= 1 && subgroups.length >= 1;
          },
          240_000,
          "a processed event and at least one subgroup",
        );

        // The load-bearing assertion: exactly ONE subgroup — the claim stopped
        // the two executors from both minting one for the same batch.
        expect(subgroups.length, `expected exactly 1 subgroup, got ${subgroups.length}`).to.equal(1);

        // Corroborate the coordination via signals: exactly one executor
        // `processed`. The exactly-one-subgroup outcome above is the
        // load-bearing guarantee; the coordination path is any of:
        //   (a) Bob's fast-path candidacy stood down → `notCandidate`.
        //   (b) Bob's claim raced Alice's and lost → `backedOff`.
        //   (c) Alice's `InterpretationRun.sources` cursor synced to Bob
        //       before Bob's watch loop drained a batch → Bob emits no
        //       candidacy event because there is nothing to batch.
        // (c) is a real, correct coordination path (via the cursor rather
        // than the claim) — asserting one of (a)/(b) fires would over-specify
        // the mechanism and turn a legitimate flow into a flake.
        const processedDids = new Set(
          events.filter((e) => e.step === "processed" && e.agentDid).map((e) => e.agentDid),
        );
        expect(processedDids.size, "exactly one executor should have processed").to.equal(1);
      });

      it("divides successive waves between the executors without re-processing a turn", async () => {
        const { aliceP, bobP, events } = await sharedChannel("flux-waves", { batchMin: 2 });

        const aliceDid = await agentDid(testContext.alice);
        const bobDid = await agentDid(testContext.bob);

        const retired = () =>
          events.filter((e) => e.step === "processed").flatMap((e) => e.itemIds);

        // Wave 1 authored by Alice, wave 2 by Bob. Both land in the same shared
        // channel, so each peer re-gathers the other's turns on every tick —
        // only the claim and the cursor keep a turn from being interpreted
        // twice, once per executor.
        await say(aliceP, "msg://w1a", "Our webhook retries keep dropping during payment outages.");
        await say(aliceP, "msg://w1b", "Right, the payments queue cannot replay what got dropped.");
        await waitUntil(() => retired().length >= 2, 240_000, "the first wave to be processed");

        const firstWave = retired();

        // Wave-1 must be attributable to exactly one peer. Failing here (not
        // downstream) points at the claim mechanism directly.
        const wave1Dids = new Set(
          events
            .filter((e) => e.step === "processed" && (e.itemIds ?? []).some((id) => firstWave.includes(id)))
            .map((e) => e.agentDid),
        );
        expect(
          wave1Dids.size,
          `wave 1 must be processed by exactly ONE peer, got dids=${JSON.stringify(
            [...wave1Dids],
          )} (alice=${aliceDid} bob=${bobDid})`,
        ).to.equal(1);

        // Wait for wave-1's InterpretationRun to sync ACROSS to the OTHER
        // peer — the one who didn't process wave 1. Only then does that
        // peer's cursor know wave-1 is retired and can filter it out of a
        // wave-2 batch.
        //
        // Two engine fixes make this a straight `findAll` on the other
        // peer:
        //   (a) `get_shape_or_wait` — `model_query` polls for
        //       `InterpretationRun`'s SHACL to arrive on the peer that
        //       never ran a pass, so `findAll` no longer throws
        //       "No SHACL shape stored".
        //   (b) `sh:datatype`-gated hydration — `InterpretationRun.sources`
        //       declares `xsd:string`, so `r.sources` comes back as the
        //       plain turn-hex, matching `firstWave` directly.
        const wave1Author = [...wave1Dids][0];
        const otherPeer = wave1Author === aliceDid ? bobP : aliceP;
        const otherLabel = wave1Author === aliceDid ? "Bob" : "Alice";

        await waitUntil(
          async () => {
            try {
              const runs = await InterpretationRun.findAll(otherPeer);
              return runs.some(
                (r) =>
                  Array.isArray(r.sources) &&
                  firstWave.every((id) => r.sources!.includes(id)),
              );
            } catch {
              // `findAll` can throw while the InterpretationRun SHACL is
              // still syncing to the other peer. Swallow and keep polling
              // against the 240s budget instead of aborting on first error.
              return false;
            }
          },
          240_000,
          `wave-1 InterpretationRun to sync to ${otherLabel} with all firstWave sources`,
        );

        await say(bobP, "msg://w2a", "Separately, the retro is moved to Thursday morning.");
        await say(bobP, "msg://w2b", "I'll book the room and send the invite.");
        await waitUntil(() => retired().length >= 4, 240_000, "the second wave to be processed");

        // The whole point: four turns, four retirements, no turn twice — across
        // two executors that both saw all four.
        const ids = retired();
        expect(
          new Set(ids).size,
          `every turn must be retired exactly once across both executors, got ${JSON.stringify(ids)}`,
        ).to.equal(ids.length);
        expect(
          ids.filter((id) => firstWave.includes(id)).length,
          "the second wave must not re-process the first",
        ).to.equal(firstWave.length);

        // Both executors converge on the same graph.
        await waitUntil(
          async () =>
            (await ConversationSubgroup.findAll(aliceP)).length ===
            (await ConversationSubgroup.findAll(bobP)).length,
          120_000,
          "both executors to converge on the same subgroups",
        );
        const seen = await ConversationSubgroup.findAll(aliceP);
        expect(seen.length, "the waves must have produced at least one subgroup").to.be.greaterThan(
          0,
        );
      });
    });
  };
}
