/**
 * Two-executor auto-processor integration: two real ad4m executors sharing a
 * neighbourhood must divide the work rather than duplicate it. This is the
 * full-integration mirror of the Rust
 * `auto_processor_two_users_one_executor_no_double_processing`, which runs both
 * peers in one process; here they are separate executors syncing over a link
 * language, so the claim and the processed-turn cursor have to coordinate
 * through the shared graph with real sync latency in between.
 *
 * Setup (once per describe, in `before`):
 *   1. Alice creates a perspective and publishes it as a neighbourhood.
 *   2. Bob joins → both share the graph, so claim + subgroup links sync.
 *   3. Explicit gossip warm-up: Alice writes a marker link, we wait until Bob
 *      sees it. Proves the sync path is live before any test runs.
 *   4. Register ConversationSubgroup + the interpretation-overlay hard-wired
 *      classes on BOTH peers up-front — drains the SHACL writes into the
 *      warm-up window when the test is idle.
 *   5. Register the LLM on both peers.
 *
 * Per-test isolation via graph portioning (Nico 2026-08-20):
 *   Each `it` block works over its OWN subtree of the shared perspective —
 *   messages live under a per-test scope root (`soa://ap-<test>/`), and the
 *   processor's `sourceScopeQuery` walks only that root. Two processors on
 *   the same perspective therefore see disjoint message sets, so they don't
 *   cross-contaminate each other's cursors or subgroup mints. `existingScope`
 *   + `mintScope` additionally isolate the interpreted-instance side (upsert
 *   scope + mint-scope linking) as belt-and-suspenders.
 *
 * Reusing the neighbourhood avoids paying the full Holochain gossip-bootstrap
 * on every test — the earlier "fresh neighbourhood per it" pattern reliably
 * timed out on Marvin under CI load (2026-08-20 investigation, jobs 21598,
 * 21612, 21624, 21680).
 *
 * Model/endpoint are overridable via INTERPRETATION_E2E_MODEL /
 * INTERPRETATION_E2E_BASE_URL.
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

// Predicate linking a scope-root node to the message URIs it contains.
// Used both by `say()` (to make each test's messages children of its scope
// root) and by the scope query below (to gather them by walking that edge).
const HAS_MSG = "soa://has-msg";

/**
 * Build a `sourceScopeQuery` that gathers only messages under `scopeRoot`.
 *
 * `?speaker` and `?timestamp` still come off the body-link reifier — required
 * bindings the processed-turn cursor keys on — same shape as
 * `BODY_AUTHOR_TIMESTAMP_SCOPE_QUERY`, plus a scope-root parent edge.
 */
function scopedSourceQuery(scopeRoot: string): string {
  return `SELECT ?speaker ?text ?timestamp WHERE {
    <${scopeRoot}> <${HAS_MSG}> ?m .
    ?m <ns://body> ?text .
    ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
    ?r <ad4m://ontology/author> ?speaker .
    ?r <ad4m://ontology/timestamp> ?timestamp .
  }
  ORDER BY ?timestamp`;
}

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
      // Per-test wait budgets: setup warm-up (≤180s) + wave-1 processed
      // (≤240s) + cross-peer cursor sync (≤300s) + wave-2 processed (≤240s)
      // + subgroup convergence (≤120s) ≈ 1_080s worst case. Cap the suite at
      // 1_500s so every `waitUntil` has room to report its own diagnostic
      // before Mocha times out the whole test (CodeRabbit #881: never mask
      // the real barrier).
      this.timeout(1_500_000);

      // Shared neighbourhood used by every `it` in this describe. Established
      // exactly once in `before`. Per-test isolation is by scope root
      // (different portion of this same graph per test), not by fresh
      // neighbourhoods — the old "publish per it" pattern paid the full
      // Holochain gossip-bootstrap cost each time and timed out under
      // Marvin CI load (2026-08-20).
      let sharedAliceP: PerspectiveProxy;
      let sharedBobP: PerspectiveProxy;
      const allEvents: AutoProcessorEvent[] = [];

      before(async function () {
        // Setup can take a while on cold Marvin — see the 180s warm-up wait.
        this.timeout(600_000);

        const alice = testContext.alice;
        const bob = testContext.bob;

        await registerLlm(alice);
        await registerLlm(bob);

        const aliceHandle = await alice.perspective.add(`ap-shared-${uuidv4()}`);
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

        sharedAliceP = (await alice.perspective.byUUID(aliceHandle.uuid)) as PerspectiveProxy;
        sharedBobP = (await bob.perspective.byUUID(bobHandle.uuid)) as PerspectiveProxy;

        // Explicit gossip warm-up: prove the sync path is up before any
        // test runs anything critical. If gossip is broken we fail here
        // with a clear diagnostic (3 minutes into setup) rather than
        // 5 minutes into a test's barrier.
        const marker = `warmup://${uuidv4()}`;
        await sharedAliceP.add(
          new Link({ source: marker, predicate: "ns://ping", target: "literal:string:ok" }),
        );
        await waitUntil(
          async () => {
            const links = await sharedBobP.get(new LinkQuery({ source: marker }));
            return links.length > 0;
          },
          180_000,
          "gossip warm-up: Alice's marker link to reach Bob",
        );

        // Register all subject classes up-front, before any pass runs. This
        // drains the SHACL writes for ConversationSubgroup +
        // InterpretationRun + InterpretationOverlay into the setup window,
        // so the first-pass `ensure_interpretation_overlay_classes` inside
        // the executor is a no-op and doesn't compete for immediate-commit
        // slots (`IMMEDIATE_COMMITS_COUNT=20`) with the pass's own writes.
        await ConversationSubgroup.register(sharedAliceP);
        await ConversationSubgroup.register(sharedBobP);
        await InterpretationRun.register(sharedAliceP);
        await InterpretationRun.register(sharedBobP);
        await InterpretationOverlay.register(sharedAliceP);
        await InterpretationOverlay.register(sharedBobP);

        // One merged event stream from both executors. Per-test event
        // filtering is by `processorId` on the consumer side, so each `it`
        // block only sees its own processor's events.
        await sharedAliceP.addAutoProcessorEventListener((e) => allEvents.push(e));
        await sharedBobP.addAutoProcessorEventListener((e) => allEvents.push(e));
      });

      /**
       * Add a message under a scope root. `say()` links the message URI to the
       * scope root via `HAS_MSG`, so `scopedSourceQuery(scopeRoot)` will
       * gather it. Only messages linked under the correct scope root are
       * visible to a scoped processor, so tests don't cross-contaminate.
       */
      async function say(
        p: PerspectiveProxy,
        scopeRoot: string,
        uri: string,
        body: string,
      ) {
        await p.add(new Link({ source: scopeRoot, predicate: HAS_MSG, target: uri }));
        await p.add(new Link({ source: uri, predicate: "ns://body", target: `literal:string:${body}` }));
      }

      /** Events from `allEvents` filtered to the given processor. */
      function eventsFor(processorId: string): AutoProcessorEvent[] {
        return allEvents.filter((e) => e.processorId === processorId);
      }

      it("processes a shared channel exactly once (claim coordinates)", async () => {
        const processorId = "flux-channel";
        const scopeRoot = "soa://ap-c/root";

        await sharedAliceP.addAutoProcessor({
          processorId,
          sourceScopeQuery: scopedSourceQuery(scopeRoot),
          interpretationClasses: ["ns://ConversationSubgroup"],
          debounceMs: 200,
          batchMin: 2,
          batchMax: 32,
          claimTtlMs: 60_000,
          // Isolate this processor's dedup lookup + mint side to its own
          // subtree so `ConversationSubgroup.findAll` scoped below sees only
          // subgroups this processor minted.
          existingScope: { id: scopeRoot, predicate: HAS_MSG },
          mintScope: { id: scopeRoot, predicate: HAS_MSG },
        } as any);
        await sleep(2000);

        await say(
          sharedAliceP,
          scopeRoot,
          "msg://c1",
          "Our webhook retries keep dropping during payment outages — we lose the failed events.",
        );
        await say(
          sharedAliceP,
          scopeRoot,
          "msg://c2",
          "Right, the payments queue has no way to replay what got dropped last time.",
        );

        // Wait until at least one subgroup is visible IN THIS SCOPE — same
        // 4-minute budget as the wave-division test uses.
        let subgroups: string[] = [];
        await waitUntil(
          async () => {
            // Subgroups this processor minted are linked under `scopeRoot`
            // via `mintScope`. Query for children of the scope root to isolate
            // to this test's outputs (avoids counting anything test 2 might
            // have produced first, in case order changes).
            const childLinks = await sharedAliceP.get(
              new LinkQuery({ source: scopeRoot, predicate: HAS_MSG }),
            );
            subgroups = childLinks
              .map((l) => l.data.target)
              .filter((t) => t.startsWith("ad4m://") || t.startsWith("soa://"));
            const processedCount = eventsFor(processorId).filter(
              (e) => e.step === "processed",
            ).length;
            return processedCount >= 1 && subgroups.length >= 1;
          },
          240_000,
          "a processed event and at least one subgroup",
        );

        // The load-bearing assertion: exactly ONE subgroup — the claim stopped
        // the two executors from both minting one for the same batch.
        // Note: `subgroups` includes the two msg:// URIs (children of the
        // scope root) plus the minted subgroup base. Filter for the minted
        // one.
        const minted = subgroups.filter(
          (uri) => !uri.startsWith("msg://") && !uri.startsWith("warmup://"),
        );
        expect(minted.length, `expected exactly 1 minted subgroup, got ${JSON.stringify(minted)}`).to.equal(1);

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
          eventsFor(processorId)
            .filter((e) => e.step === "processed" && e.agentDid)
            .map((e) => e.agentDid),
        );
        expect(processedDids.size, "exactly one executor should have processed").to.equal(1);
      });

      it("divides successive waves between the executors without re-processing a turn", async () => {
        const processorId = "flux-waves";
        const scopeRoot = "soa://ap-w/root";

        await sharedAliceP.addAutoProcessor({
          processorId,
          sourceScopeQuery: scopedSourceQuery(scopeRoot),
          interpretationClasses: ["ns://ConversationSubgroup"],
          debounceMs: 200,
          batchMin: 2,
          batchMax: 32,
          claimTtlMs: 60_000,
          existingScope: { id: scopeRoot, predicate: HAS_MSG },
          mintScope: { id: scopeRoot, predicate: HAS_MSG },
        } as any);
        await sleep(2000);

        const retired = () =>
          eventsFor(processorId)
            .filter((e) => e.step === "processed")
            .flatMap((e) => e.itemIds);

        // Wave 1 is authored by Alice, wave 2 by Bob. Both land in the same
        // scope subtree, so each peer re-gathers the other's turns on every
        // tick — only the claim and the cursor keep a turn from being
        // interpreted twice, once per executor.
        await say(
          sharedAliceP,
          scopeRoot,
          "msg://w1a",
          "Our webhook retries keep dropping during payment outages.",
        );
        await say(
          sharedAliceP,
          scopeRoot,
          "msg://w1b",
          "Right, the payments queue cannot replay what got dropped.",
        );
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
        // `processed` events) reports duplicates.
        //
        // 300s budget: p-diff-sync gossip on Marvin under CI load can take
        // several minutes to deliver a fresh revision, especially the
        // FIRST cross-peer roundtrip on a warmed neighbourhood. If this
        // still expires the failure diagnostic ("wave-1 InterpretationRun
        // .sources to sync to Bob") points at the underlying sync layer.
        await waitUntil(
          async () => {
            const bobRuns = await InterpretationRun.findAll(sharedBobP);
            return bobRuns.some(
              (r) =>
                Array.isArray(r.sources) &&
                firstWave.every((id) => r.sources!.includes(id)),
            );
          },
          300_000,
          "wave-1 InterpretationRun.sources to sync to Bob before wave 2",
        );

        await say(
          sharedBobP,
          scopeRoot,
          "msg://w2a",
          "Separately, the retro is moved to Thursday morning.",
        );
        await say(
          sharedBobP,
          scopeRoot,
          "msg://w2b",
          "I'll book the room and send the invite.",
        );
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

        // Both executors converge on the same subgroup set for THIS scope.
        // Filter to subgroups under this test's scope root so any subgroups
        // from the first test don't inflate the counts.
        const scopedSubgroupCount = async (p: PerspectiveProxy): Promise<number> => {
          const children = await p.get(new LinkQuery({ source: scopeRoot, predicate: HAS_MSG }));
          return children
            .map((l) => l.data.target)
            .filter((t) => !t.startsWith("msg://") && !t.startsWith("warmup://")).length;
        };
        await waitUntil(
          async () => (await scopedSubgroupCount(sharedAliceP)) === (await scopedSubgroupCount(sharedBobP)),
          120_000,
          "both executors to converge on the same subgroups",
        );
        expect(
          await scopedSubgroupCount(sharedAliceP),
          "the waves must have produced at least one subgroup in this scope",
        ).to.be.greaterThan(0);
      });
    });
  };
}
