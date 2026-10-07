import path from "path";
import fs from "fs-extra";
import { fileURLToPath } from "url";
import * as chai from "chai";
import {
    Ad4mClient,
    LanguageMetaInput,
    Perspective,
    PerspectiveState,
    PerspectiveUnsignedInput,
} from "@coasys/ad4m";
import { ChildProcess } from "child_process";
import { startExecutor, baseUrl, runHcLocalServices, quitExecutor, pollUntil, sleep } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { TestContext } from "./test-context";

const expect = chai.expect;
const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const TEST_DIR = path.join(__dirname, "../tst-tmp");

// Regression test for #1099: two languages installed from the SAME embedded
// Holochain DNA/network-seed (only their content hash differs, e.g. because a
// language was republished without bumping its template uid) resolve to the
// same Holochain cell id. Before #1099, both languages shared one Holochain
// agent key, so they shared one cell, and the per-cell signal-handler
// registry is last-writer-wins: only the most recently loaded language's
// neighbourhood ever receives telepresence signals on that cell.
//
// This must run cross-node (Alice publishes, Bob joins from a real
// Holochain-backed neighbourhood on a separate executor): same-node tests
// route Alice-to-Bob-on-one-node signals locally without ever touching the
// shared cell, so they can't see the bug (see the discarded single-executor
// attempt in tests/multi-user-simple.test.ts history).
describe("Multi-node signal collision (#1099)", function () {
    //@ts-ignore
    this.timeout(200000);

    const bootstrapSeedPath = path.join(__dirname, "../bootstrapSeed.json");
    const aliceDataPath = path.join(TEST_DIR, "agents", "collision-alice");
    const bobDataPath = path.join(TEST_DIR, "agents", "collision-bob");

    let aliceApiPort: number, aliceHcAdminPort: number, aliceHcAppPort: number;
    let bobApiPort: number, bobHcAdminPort: number, bobHcAppPort: number;
    let aliceExecutor: ChildProcess | null = null;
    let bobExecutor: ChildProcess | null = null;
    let localServicesProcess: ChildProcess | null = null;
    let proxyUrl: string | null = null;
    let bootstrapUrl: string | null = null;
    let relayUrl: string | null = null;

    const testContext = new TestContext();

    before(async () => {
        [aliceApiPort, aliceHcAdminPort, aliceHcAppPort] = await getFreePorts(3);
        registerPorts([aliceApiPort, aliceHcAdminPort, aliceHcAppPort]);
        [bobApiPort, bobHcAdminPort, bobHcAppPort] = await getFreePorts(3);
        registerPorts([bobApiPort, bobHcAdminPort, bobHcAppPort]);

        fs.mkdirSync(aliceDataPath, { recursive: true });
        fs.mkdirSync(bobDataPath, { recursive: true });

        const localServices = await runHcLocalServices();
        proxyUrl = localServices.proxyUrl;
        bootstrapUrl = localServices.bootstrapUrl;
        relayUrl = localServices.relayUrl;
        localServicesProcess = localServices.process;

        aliceExecutor = await startExecutor(
            aliceDataPath, bootstrapSeedPath,
            aliceApiPort, aliceHcAdminPort, aliceHcAppPort,
            false, undefined, proxyUrl!, bootstrapUrl!, relayUrl!,
        );
        bobExecutor = await startExecutor(
            bobDataPath, bootstrapSeedPath,
            bobApiPort, bobHcAdminPort, bobHcAppPort,
            false, undefined, proxyUrl!, bootstrapUrl!, relayUrl!,
        );

        testContext.alice = new Ad4mClient(baseUrl(aliceApiPort));
        testContext.bob = new Ad4mClient(baseUrl(bobApiPort));
        testContext.aliceCore = aliceExecutor;
        testContext.bobCore = bobExecutor;

        await testContext.alice.agent.generate("passphrase");
        await testContext.bob.agent.generate("passphrase");
    });

    after(async () => {
        if (aliceExecutor) await quitExecutor(aliceExecutor, aliceApiPort);
        if (bobExecutor) await quitExecutor(bobExecutor, bobApiPort);
        if (localServicesProcess) localServicesProcess.kill("SIGKILL");
        deregisterPorts([aliceApiPort, aliceHcAdminPort, aliceHcAppPort]);
        deregisterPorts([bobApiPort, bobHcAdminPort, bobHcAppPort]);
    });

    it("delivers telepresence signals on two neighbourhoods republished from the same language without changing the network seed", async () => {
        const alice = testContext.alice;
        const bob = testContext.bob;

        // Two modified copies of the already-installed perspective-diff-sync
        // bundle: same embedded DNA, so the same default network seed;
        // different trailing comment, so different content hash, so
        // different language address. This stands in for "two languages
        // republished from the same source without changing the seed".
        const baseBundlePath = path.join(__dirname, "../tst-tmp/languages/perspective-diff-sync/build/bundle.js");
        const baseBundle = fs.readFileSync(baseBundlePath).toString();
        const bundleAPath = path.join(__dirname, "../tst-tmp/perspective-diff-sync-collision-node-a.js");
        const bundleBPath = path.join(__dirname, "../tst-tmp/perspective-diff-sync-collision-node-b.js");
        fs.writeFileSync(bundleAPath, baseBundle + "\n//CollisionNodeA");
        fs.writeFileSync(bundleBPath, baseBundle + "\n//CollisionNodeB");

        const languageA = await alice.languages.publish(bundleAPath, new LanguageMetaInput("Collision Node Test A", ""));
        const languageB = await alice.languages.publish(bundleBPath, new LanguageMetaInput("Collision Node Test B", ""));
        expect(languageA.address).to.not.equal(languageB.address, "the two republished copies must get different addresses");

        const perspectiveA = await alice.perspective.add("Collision Node Test Neighbourhood A");
        const perspectiveB = await alice.perspective.add("Collision Node Test Neighbourhood B");
        const neighbourhoodUrlA = await alice.neighbourhood.publishFromPerspective(perspectiveA.uuid, languageA.address, new Perspective([]));
        const neighbourhoodUrlB = await alice.neighbourhood.publishFromPerspective(perspectiveB.uuid, languageB.address, new Perspective([]));

        await pollUntil(async () => {
            const p = await alice.perspective.byUUID(perspectiveA.uuid);
            return p?.state === PerspectiveState.Synced || p?.state === PerspectiveState.LinkLanguageInstalledButNotSynced;
        }, { timeoutMs: 30000, label: "alice's neighbourhood A has its link language" });
        await pollUntil(async () => {
            const p = await alice.perspective.byUUID(perspectiveB.uuid);
            return p?.state === PerspectiveState.Synced || p?.state === PerspectiveState.LinkLanguageInstalledButNotSynced;
        }, { timeoutMs: 30000, label: "alice's neighbourhood B has its link language" });

        const bobP1AHandle = await bob.neighbourhood.joinFromUrl(neighbourhoodUrlA);
        const bobP1BHandle = await bob.neighbourhood.joinFromUrl(neighbourhoodUrlB);

        await testContext.makeAllNodesKnown();

        await pollUntil(async () => {
            const p = await bob.perspective.byUUID(bobP1AHandle.uuid);
            return p?.state === PerspectiveState.Synced || p?.state === PerspectiveState.LinkLanguageInstalledButNotSynced;
        }, { timeoutMs: 30000, label: "bob's neighbourhood A has its link language" });
        await pollUntil(async () => {
            const p = await bob.perspective.byUUID(bobP1BHandle.uuid);
            return p?.state === PerspectiveState.Synced || p?.state === PerspectiveState.LinkLanguageInstalledButNotSynced;
        }, { timeoutMs: 30000, label: "bob's neighbourhood B has its link language" });

        const aliceNHA = perspectiveA.getNeighbourhoodProxy();
        const aliceNHB = perspectiveB.getNeighbourhoodProxy();
        const bobP1A = await bob.perspective.byUUID(bobP1AHandle.uuid);
        const bobP1B = await bob.perspective.byUUID(bobP1BHandle.uuid);
        const bobNHProxyA = bobP1A!.getNeighbourhoodProxy();
        const bobNHProxyB = bobP1B!.getNeighbourhoodProxy();

        const bobDID = (await bob.agent.me()).did!;

        // Cross-node DID->AgentPubKey link propagation (perspective-diff-sync's
        // create_did_pub_key_link) takes real gossip time between two separate
        // executors (unlike same-process multi-user mode). sendSignal fails
        // with "no AgentPubKey found for DID" until each cell's otherAgents()
        // sees the peer, so wait for that on both neighbourhoods first.
        await pollUntil(async () => (await aliceNHA.otherAgents()).length >= 1,
            { timeoutMs: 60000, label: "neighbourhood A knows about bob" });
        await pollUntil(async () => (await aliceNHB.otherAgents()).length >= 1,
            { timeoutMs: 60000, label: "neighbourhood B knows about bob" });

        const bobReceivedA: any[] = [];
        const bobReceivedB: any[] = [];
        bobNHProxyA.addSignalHandler((signal: any) => { bobReceivedA.push(signal); });
        bobNHProxyB.addSignalHandler((signal: any) => { bobReceivedB.push(signal); });

        // Subscription-init delay: addSignalHandler() does not wait for the
        // server to register the subscription, and signals are not
        // redelivered.
        await sleep(1000);

        // The DID link can take a few retries to land even after otherAgents()
        // first reports the peer, so retry sendSignal instead of asserting on
        // the first call.
        await pollUntil(async () => {
            try {
                await aliceNHA.sendSignalU(bobDID, new PerspectiveUnsignedInput([
                    { source: "test://collision-node-a", predicate: "test://from", target: bobDID },
                ]));
                return true;
            } catch {
                return false;
            }
        }, { timeoutMs: 30000, label: "alice sends signal A" });
        await pollUntil(async () => {
            try {
                await aliceNHB.sendSignalU(bobDID, new PerspectiveUnsignedInput([
                    { source: "test://collision-node-b", predicate: "test://from", target: bobDID },
                ]));
                return true;
            } catch {
                return false;
            }
        }, { timeoutMs: 30000, label: "alice sends signal B" });

        const sourceOf = (s: any) => s?.data?.links?.[0]?.data?.source;

        // The verdict: with one shared Holochain cell behind both languages,
        // the per-cell signal-handler registry (last-writer-wins) routes
        // every signal on that cell to only one of the two languages —
        // whichever loaded last — so the other neighbourhood's own handler
        // never sees its own signal, and/or the winning one sees both. With
        // #1099's per-language agent keys the cells are distinct, so each
        // neighbourhood's handler sees only its own signal.
        try {
            await pollUntil(
                () => bobReceivedA.some(s => sourceOf(s) === "test://collision-node-a")
                    && bobReceivedB.some(s => sourceOf(s) === "test://collision-node-b"),
                { timeoutMs: 30000, label: "both neighbourhoods deliver their own telepresence signal" },
            );
        } finally {
            console.log("DIAGNOSTIC bobReceivedA sources:", bobReceivedA.map(sourceOf));
            console.log("DIAGNOSTIC bobReceivedB sources:", bobReceivedB.map(sourceOf));
        }

        expect(
            bobReceivedA.some(s => sourceOf(s) === "test://collision-node-a"),
            "neighbourhood A's handler must receive its own signal",
        ).to.be.true;
        expect(
            bobReceivedB.some(s => sourceOf(s) === "test://collision-node-b"),
            "neighbourhood B's handler must receive its own signal",
        ).to.be.true;
        expect(
            bobReceivedA.some(s => sourceOf(s) === "test://collision-node-b"),
            "neighbourhood A's handler must never receive neighbourhood B's signal",
        ).to.be.false;
        expect(
            bobReceivedB.some(s => sourceOf(s) === "test://collision-node-a"),
            "neighbourhood B's handler must never receive neighbourhood A's signal",
        ).to.be.false;
    });
});
