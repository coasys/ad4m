/**
 * Multi-user auto-processor regression: on a hosted node in multi-user mode
 * the executor must spawn one auto_processor_watch_loop per online managed
 * user, not one loop as the host `main_agent`. If it does not, `elect_author`
 * walks the batch's authors, finds only managed-user DIDs (not the host),
 * returns `Other(managed-user)`, and stands down forever.
 *
 * Reproduction:
 *   1. Start one executor with `enable_multi_user`.
 *   2. Create + log in two managed users (Alice, Bob).
 *   3. Register an AutoProcessorConfig on a perspective owned by Alice's
 *      session.
 *   4. Alice + Bob each post a message via their own JWT'd Ad4mClient — the
 *      link's author is the managed user's DID.
 *   5. Wait for a ConversationSubgroup to appear.
 *
 * Pre-fix: no per-user loop runs, the main-agent loop stands down on every
 * tick, no subgroup appears, `waitUntil` times out. Post-fix: the supervisor
 * spawns a per-user loop for Alice (and Bob, once his session touches the
 * API), one of them wins election on their own messages, the pass runs.
 *
 * The interpretation engine's LLM is registered by the admin session; the
 * default model is looked up by class, not by user, so managed-user auto-
 * processor passes reach it the same way as single-user passes.
 */
import path from "path";
import { Ad4mClient, Link, PerspectiveProxy } from "@coasys/ad4m";
import type { AutoProcessorEvent } from "@coasys/ad4m";
import fs from "fs-extra";
import { fileURLToPath } from "url";
import { expect } from "chai";
import {
  baseUrl,
  pollUntil,
  startExecutor,
  runHcLocalServices,
  gracefulShutdown,
} from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { waitUntil } from "../helpers/index";
import { ChildProcess } from "node:child_process";
import { ConversationSubgroup } from "./model/auto-processor-models";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

const BASE_URL = process.env.INTERPRETATION_E2E_BASE_URL || "http://localhost:11434/v1";
const MODEL = process.env.INTERPRETATION_E2E_MODEL || "gemma3:12b";

// Same shape as the two-executor test uses: speaker + timestamp come off the
// body link's reifier, so `?speaker` is the DID that signed the body — which
// is what election walks.
const SCOPE_QUERY = `SELECT ?speaker ?text ?timestamp WHERE {
  ?m <ns://body> ?text .
  ?r <http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies> <<( ?m <ns://body> ?text )>> .
  ?r <ad4m://ontology/author> ?speaker .
  ?r <ad4m://ontology/timestamp> ?timestamp .
}
ORDER BY ?timestamp`;

describe("AutoProcessor runs for managed users on a hosted node", function () {
  this.timeout(600_000);

  const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
  const appDataPath = path.join(TEST_DIR, "agents", "auto-processor-multi-user");
  const bootstrapSeedPath = path.join(`${__dirname}/../bootstrapSeed.json`);

  let apiPort: number, hcAdminPort: number, hcAppPort: number;
  let executorProcess: ChildProcess | null = null;
  let localServicesProcess: ChildProcess | null = null;
  let admin: Ad4mClient | null = null;
  let alice: Ad4mClient | null = null;
  let bob: Ad4mClient | null = null;
  let proxyUrl: string | null = null;
  let bootstrapUrl: string | null = null;

  before(async () => {
    [apiPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
    registerPorts([apiPort, hcAdminPort, hcAppPort]);
    if (!fs.existsSync(appDataPath)) {
      fs.mkdirSync(appDataPath, { recursive: true });
    }

    const localServices = await runHcLocalServices();
    proxyUrl = localServices.proxyUrl;
    bootstrapUrl = localServices.bootstrapUrl;
    localServicesProcess = localServices.process;

    executorProcess = await startExecutor(
      appDataPath,
      bootstrapSeedPath,
      apiPort,
      hcAdminPort,
      hcAppPort,
      false,
      undefined,
      proxyUrl!,
      bootstrapUrl!,
    );

    admin = new Ad4mClient(baseUrl(apiPort), undefined, false);
    await admin.agent.generate("passphrase");
    await admin.runtime.setMultiUserEnabled(true);

    // LLM lives on the admin side — its default binding is global, so
    // interpretation passes running under a managed user's context still
    // resolve to it.
    const modelId = await admin.ai.addModel({
      name: "interpretation-llm",
      api: { baseUrl: BASE_URL, apiKey: "ollama", model: MODEL, apiType: "OPEN_AI" },
      modelType: "LLM",
    } as any);
    await admin.ai.setDefaultModel("LLM", modelId);

    // Two managed users, free-access so billing doesn't gate the LLM call.
    for (const email of ["alice@apmutest.local", "bob@apmutest.local"]) {
      await admin.agent.createUser(email, "password");
      await admin.runtime.setUserFreeAccess(email, true);
    }

    // Log in both — one JWT per user, one client per user.
    const aliceToken = await admin.agent.loginUser("alice@apmutest.local", "password");
    const bobToken = await admin.agent.loginUser("bob@apmutest.local", "password");
    alice = new Ad4mClient(baseUrl(apiPort), aliceToken, false);
    bob = new Ad4mClient(baseUrl(apiPort), bobToken, false);

    // Trigger last_seen updates for both — the supervisor's freshness filter
    // is what decides whether to spawn a loop per user, and last_seen is set
    // by any authenticated call.
    await alice.agent.me();
    await bob.agent.me();
  });

  after(async () => {
    try {
      if (admin) {
        for (const p of await admin.perspective.all()) {
          try {
            await admin.perspective.remove(p.uuid);
          } catch {}
        }
      }
    } catch {}
    await gracefulShutdown(executorProcess, "executor");
    await gracefulShutdown(localServicesProcess, "local services");
    deregisterPorts([apiPort, hcAdminPort, hcAppPort]);
  });

  it("processes utterances authored by managed users without a main-agent loop", async () => {
    // Alice creates the perspective, registers the extraction class, and
    // configures one auto-processor over the body-message scope.
    const handle = await alice!.perspective.add("auto-processor-managed-users");
    const aliceP = (await alice!.perspective.byUUID(handle.uuid)) as PerspectiveProxy;
    await ConversationSubgroup.register(aliceP);

    const events: AutoProcessorEvent[] = [];
    await aliceP.addAutoProcessorEventListener((e) => events.push(e));

    await aliceP.addAutoProcessor({
      processorId: "managed-users-channel",
      sourceScopeQuery: SCOPE_QUERY,
      interpretationClasses: ["ns://ConversationSubgroup"],
      debounceMs: 200,
      batchMin: 2,
      batchMax: 32,
      claimTtlMs: 60_000,
    } as any);

    // Wait for the supervisor to spawn per-user loops (first tick at ~5s)
    await pollUntil(async () => {
        const bobPRaw = await bob!.perspective.byUUID(handle.uuid);
        return bobPRaw !== null;
    }, { timeoutMs: 15000, intervalMs: 1000, label: "supervisor spawns per-user loops" });

    // `perspective.add` assigns the caller as the owner, so a strict
    // ownership regime would make Bob's `byUUID` return `null`
    // (CodeRabbit #881 review). On the current hosted-node/multi-user
    // path Bob is a managed user on Alice's executor, so he does see the
    // perspective, but assert it here so a future ownership tightening
    // surfaces a clear diagnostic instead of a silent NPE on `bobP.add`.
    const bobPRaw = await bob!.perspective.byUUID(handle.uuid);
    expect(
      bobPRaw,
      "Bob (managed user on the same executor) must resolve Alice's perspective — " +
        "if this fails, publish the perspective as a neighbourhood or add Bob " +
        "as an owner before this line.",
    ).to.exist;
    const bobP = bobPRaw as PerspectiveProxy;

    // One turn from each managed user's own JWT'd client — so each link is
    // signed by that user's DID and the body link's reifier carries the
    // managed user (not the host) as `?speaker` for election.
    await aliceP.add(
      new Link({
        source: "msg://alice-1",
        predicate: "ns://body",
        target:
          "literal:string:Our webhook retries keep dropping during payment outages — we lose the failed events.",
      }),
    );
    await bobP.add(
      new Link({
        source: "msg://bob-1",
        predicate: "ns://body",
        target:
          "literal:string:Right, the payments queue has no way to replay what got dropped last time.",
      }),
    );

    // Pre-fix: this waitUntil times out — no loop matches the authors so
    // every tick logs `notCandidate`, the batch never claims, no subgroup
    // gets minted. Post-fix: a per-user loop spawns for whichever managed
    // user's DID leads message order, election succeeds, the pass runs.
    let subgroups: ConversationSubgroup[] = [];
    await waitUntil(
      async () => {
        subgroups = await ConversationSubgroup.findAll(aliceP);
        const processedCount = events.filter((e) => e.step === "processed").length;
        return processedCount >= 1 && subgroups.length >= 1;
      },
      240_000,
      "a processed event and at least one subgroup from managed-user utterances",
    );

    expect(
      subgroups.length,
      `expected at least 1 subgroup, got ${subgroups.length}`,
    ).to.be.greaterThan(0);

    // The pass was run by a managed-user DID, not by the host `main_agent`.
    // This is the correctness half of the fix: instances minted on a shared
    // graph are attributed to the actual authoring user, not to the host
    // (which would corrupt provenance for Synergy-Fuel-style attribution).
    const processedDids = new Set(
      events
        .filter((e) => e.step === "processed" && (e as any).agentDid)
        .map((e) => (e as any).agentDid),
    );
    expect(processedDids.size, "at least one managed user should have processed").to.be.greaterThan(
      0,
    );
    for (const did of processedDids) {
      expect(did, `processor DID must be a managed user, not host main_agent (got ${did})`)
        .to.match(/^did:key:/);
    }
  });
});
