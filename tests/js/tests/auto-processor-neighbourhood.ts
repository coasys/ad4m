/**
 * Two-executor auto-processor integration: two real ad4m executors sharing a
 * neighbourhood must divide the work rather than duplicate it. This is the
 * full-integration mirror of the Rust
 * `auto_processor_two_users_one_executor_no_double_processing`, which runs both
 * peers in one process; here they are separate executors syncing over a link
 * language, so the claim and the processed-turn cursor have to coordinate
 * through the shared graph with real sync latency in between.
 *
 * Flow (mirrors ./neighbourhood.ts's publish/join pattern):
 *   1. Alice creates a perspective and publishes it as a neighbourhood.
 *   2. Bob joins → both share the graph, so claim + subgroup links sync.
 *   3. Both register the ConversationSubgroup class + the LLM; Alice registers
 *      one auto-processor (its config syncs to Bob).
 *   4. Messages are posted into the shared channel.
 *   5. Both executors' watch loops run and coordinate over the shared graph.
 *
 * The neighbourhood is published and joined here, the same way ./neighbourhood.ts
 * does it, so this needs nothing from the environment beyond what the rest of
 * the integration suite already needs — plus a reachable LLM, which the runner
 * provides for the Rust e2e suite too. Model/endpoint are overridable via
 * INTERPRETATION_E2E_MODEL / INTERPRETATION_E2E_BASE_URL.
 */
import {
  PerspectiveProxy,
  Perspective,
  Link,
  LinkQuery,
  InterpretationRun,
  InterpretationOverlay,
} from "@coasys/ad4m";
import type { AutoProcessorEvent } from "@coasys/ad4m";
import { TestContext } from "./integration.test";
import { sleep } from "../utils/utils";
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
      // + wave-1 cursor synced to Bob (up to 300s)
      // + wave-2 processed (up to 240s)
      // + subgroups converge (up to 120s)
      // = up to 1_080s. Suite timeout 1_500s gives every waitUntil budget
      // enough head-room to report its own diagnostic before Mocha times
      // out the whole test (CodeRabbit #881: never mask the real barrier).
      this.timeout(1_500_000);
      /**
       * Alice publishes a neighbourhood, Bob joins, both register the class and
       * the LLM, and Alice registers one processor whose config syncs to Bob.
       * Returns both proxies plus the merged event stream from both executors.
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
        await sleep(2000);

        const aliceP = (await alice.perspective.byUUID(aliceHandle.uuid)) as PerspectiveProxy;
        const bobP = (await bob.perspective.byUUID(bobHandle.uuid)) as PerspectiveProxy;

        // Explicit gossip warm-up: each `it` in this describe currently spins
        // up a fresh neighbourhood (new language template, new publish/join),
        // so p-diff-sync has to bootstrap its gossip peers from scratch on
        // every test. If gossip isn't actually flowing yet, any subsequent
        // cross-peer expectation (like the wave test's cursor barrier) will
        // fail with a confusing timeout deep inside the test.
        //
        // Prove the sync path works at setup time: Alice writes a marker
        // link, we wait until it reaches Bob. If gossip is broken we fail
        // here with a clear diagnostic, not 4 minutes later on wave 2's
        // barrier. If it's fine we proceed with confidence that further
        // link writes will sync in bounded time (2026-08-20 Marvin flake).
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

        // Pre-register the interpretation-overlay hard-wired classes on BOTH
        // peers up-front, so `ensure_interpretation_overlay_classes` inside
        // the pass is a no-op. Otherwise the very first pass fires ~35+
        // SHACL link writes for `InterpretationRun` + `InterpretationOverlay`
        // SDNA in tight succession — enough to hit the per-perspective
        // `IMMEDIATE_COMMITS_COUNT=20` throttle in
        // `perspective_instance::commit` and push the rest into the
        // pending-diff queue (which only drains on a 3s / 1s-idle timer).
        // Under that backlog the wave-1 `InterpretationRun` never reaches
        // Bob's copy within the barrier's budget, and Bob's watcher never
        // sees the wave-1 messages either, so wave 2 races.
        //
        // Registering up-front drains those writes during the warm-up window,
        // when the test is idle and gossip has time to catch up (2026-08-20
        // Marvin sync-latency investigation).
        await InterpretationRun.register(aliceP);
        await InterpretationRun.register(bobP);
        await InterpretationOverlay.register(aliceP);
        await InterpretationOverlay.register(bobP);

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
        await sleep(2000);

        return { aliceP, bobP, events };
      }

      async function say(p: PerspectiveProxy, uri: string, body: string) {
        await p.add(new Link({ source: uri, predicate: "ns://body", target: `literal:string:${body}` }));
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

        const retired = () =>
          events.filter((e) => e.step === "processed").flatMap((e) => e.itemIds);

        // Wave 1 is authored by Alice, wave 2 by Bob. Both land in the same
        // shared channel, so each peer re-gathers the other's turns on every
        // tick — only the claim and the cursor keep a turn from being
        // interpreted twice, once per executor.
        await say(aliceP, "msg://w1a", "Our webhook retries keep dropping during payment outages.");
        await say(aliceP, "msg://w1b", "Right, the payments queue cannot replay what got dropped.");
        await waitUntil(() => retired().length >= 2, 240_000, "the first wave to be processed");

        const firstWave = retired();

        // Wait for Bob to see any InterpretationRun (his own OR Alice's
        // synced) whose sources cover firstWave. The race we're closing:
        // Alice's `processed` fires on her local event stream the instant
        // she writes the run, but Bob only learns that w1a+w1b are retired
        // once Alice's `InterpretationRun.sources` reaches his copy of the
        // perspective. Without this barrier, wave 2 lands on Bob while his
        // watcher still thinks all 4 turns are new — Bob re-processes
        // w1a+w1b, and `retired()` (which aggregates BOTH executors' local
        // `processed` events) reports duplicates. The bare Alice-side
        // `retired().length >= 2` wait is Alice-local; the assertion below
        // is cross-peer.  `InterpretationRun` was already registered on Bob
        // during sharedChannel warm-up.
        // 300s budget: p-diff-sync gossip on Marvin under CI load can take
        // several minutes to deliver a fresh revision — the previous 120s
        // and 240s budgets both timed out. The gossip warm-up in
        // `sharedChannel` proves the SYNC PATH is up, but that doesn't
        // bound per-link latency. If this still expires the failure
        // diagnostic ("wave-1 InterpretationRun.sources to sync to Bob")
        // points at the underlying p-diff-sync reliability rather than at
        // our test.
        await waitUntil(
          async () => {
            const bobRuns = await InterpretationRun.findAll(bobP);
            return bobRuns.some(
              (r) =>
                Array.isArray(r.sources) &&
                firstWave.every((id) => r.sources!.includes(id)),
            );
          },
          300_000,
          "wave-1 InterpretationRun.sources to sync to Bob before wave 2",
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
