/**
 * Step 7 — holograph-link Language end-to-end integration test.
 *
 * Single-conductor by design. The HolographSpace's K2 builder currently
 * uses `kitsune2_core::default_test_builder()` which is mem-only and
 * therefore in-process. Cross-process JS-driven sync needs a real
 * transport (iroh / tx5) which is Step 8 / PR-B territory — see
 * `.spike-status/blocker-step-7.md`.
 *
 * What this file does prove (and what Step 6f intentionally could not):
 *   - The holograph-link bundle loads inside the v8 isolate.
 *   - The `__holographDelegate__` install path from Step 6c is reachable
 *     end-to-end from JS bundle code through `holograph_service` deno
 *     ops down to `HolographRuntime`.
 *   - The Step 6d default-switch (`HOLOGRAPH_DEFAULT_NEIGHBORHOOD=1`)
 *     fires through the WS publish handler: a `publishFromPerspective`
 *     call with an omitted `linkLanguage` resolves to the
 *     `holograph_link_default_address()` and instantiates the holograph
 *     substrate, not perspective-diff-sync.
 *   - The Step 6e typed-WireDiff path: Alice's `addLink` produces a
 *     `perspective.LinkAdded` event back through the subscriber loop
 *     (commit -> on_local_commit -> ChannelNotifier mpsc ->
 *     holographNextEmitted op -> bundle subscriber loop ->
 *     emitPerspectiveDiff -> runtime listener fires).
 *   - Restart-survives-state (SPIKE §2.5 exit check #5): kill the
 *     executor, restart against the same data dir, the same perspective
 *     still answers queries and a new commit still round-trips.
 */

import path from "path";
import fs from "fs-extra";
import { fileURLToPath } from "url";
import { ChildProcess, execSync } from "node:child_process";
import { expect } from "chai";
import { Ad4mClient, LinkQuery, Perspective } from "@coasys/ad4m";
import {
    baseUrl,
    sleep,
    startExecutor,
    gracefulShutdown,
} from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// Canonical AD4M address of the holograph-link Language. Produced by
// `cargo run --bin print_holograph_address` from
// `rust-executor/src/neighbourhoods.rs::holograph_link_default_address()`.
// If `HOLOGRAPH_LINK_PACKAGE_ID` in that file changes, re-derive this.
const HOLOGRAPH_LINK_ADDRESS = "QmzSYwdfDApp5UbcnS9o1xd4PkYP8F6UCRrQS4G1NFMB6hCU3ZR";

const HOLOGRAPH_BUNDLE_PATH = path.resolve(
    __dirname,
    "..",
    "..",
    "..",
    "bootstrap-languages",
    "holograph-link",
    "build",
    "bundle.js",
);

const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
const APP_DATA_PATH = path.join(TEST_DIR, "agents", "holograph-alice");
const BOOTSTRAP_SEED_PATH = path.join(`${__dirname}/../bootstrapSeed.json`);

/**
 * Drop the holograph-link bundle onto disk at the address the executor
 * resolves to under HOLOGRAPH_DEFAULT_NEIGHBORHOOD=1, so that the
 * disk-fast-path in `install_language_from_address()` finds it without
 * a language-language fetch.
 */
function preinstallHolographBundle(dataPath: string) {
    expect(fs.existsSync(HOLOGRAPH_BUNDLE_PATH)).to.equal(
        true,
        `holograph-link bundle missing — build it first: cd bootstrap-languages/holograph-link && deno run --allow-all esbuild.ts`,
    );
    // dataPath is symlinked to the hashed effective path inside startExecutor,
    // so writes through dataPath land in the executor's app-data-path.
    const targetDir = path.join(dataPath, "ad4m", "languages", HOLOGRAPH_LINK_ADDRESS);
    fs.ensureDirSync(targetDir);
    fs.copyFileSync(HOLOGRAPH_BUNDLE_PATH, path.join(targetDir, "bundle.js"));
}

describe("holograph-link Language end-to-end (single conductor)", function () {
    this.timeout(120_000);

    let apiPort: number;
    let hcAdminPort: number;
    let hcAppPort: number;
    let executorProcess: ChildProcess | null = null;
    let client: Ad4mClient | null = null;

    before(async () => {
        [apiPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
        registerPorts([apiPort, hcAdminPort, hcAppPort]);

        executorProcess = await startExecutor(
            APP_DATA_PATH,
            BOOTSTRAP_SEED_PATH,
            apiPort,
            hcAdminPort,
            hcAppPort,
            false,
            undefined,
            undefined,
            undefined,
            undefined,
            false,
            undefined,
            { env: { HOLOGRAPH_DEFAULT_NEIGHBORHOOD: "1" } },
        );

        // Pre-install the bundle now that startExecutor has run `init`
        // (which creates the data-path layout) but before any test calls
        // publishFromPerspective (which triggers install_language).
        preinstallHolographBundle(APP_DATA_PATH);

        client = new Ad4mClient(baseUrl(apiPort));
        await client.agent.generate("test-pass");
    });

    after(async () => {
        await gracefulShutdown(executorProcess, "executor");
        deregisterPorts([apiPort, hcAdminPort, hcAppPort]);
    });

    it("derives a stable Qm-prefixed address", () => {
        expect(HOLOGRAPH_LINK_ADDRESS).to.match(/^Qm[1-9A-HJ-NP-Za-km-z]+$/);
    });

    it("agent reaches initialized state with the flag on", async () => {
        const status = await client!.agent.status();
        expect(status).to.not.be.null;
        expect(status!.isInitialized).to.equal(true);
    });

    let aliceUuid: string;
    let neighbourhoodUrl: string;

    it("publishFromPerspective without linkLanguage resolves via the env-default switch", async () => {
        const perspective = await client!.perspective.add("holograph-alice-1");
        aliceUuid = perspective.uuid;

        // Omit linkLanguage. The Step 6d resolve_link_language reads
        // HOLOGRAPH_DEFAULT_NEIGHBORHOOD=1, substitutes the holograph
        // default address, and install_language_from_address loads the
        // bundle we pre-installed above.
        neighbourhoodUrl = await client!.neighbourhood.publishFromPerspective(
            aliceUuid,
            // @ts-expect-error — the v1 client type insists on a string;
            // the Rust API accepts Option<String>. PR-B will update the
            // client typings to match.
            undefined,
            new Perspective([]),
        );

        expect(neighbourhoodUrl).to.match(/^neighbourhood:\/\//);
    });

    it("perspective shows the holograph-link address as linkLanguage", async () => {
        const all = await client!.perspective.all();
        const alice = all.find((p) => p.uuid === aliceUuid);
        expect(alice, "alice perspective present").to.exist;
        expect(alice!.neighbourhood?.linkLanguage).to.equal(HOLOGRAPH_LINK_ADDRESS);
    });

    it("Alice's own addLink round-trips through the subscriber loop", async () => {
        // The subscriber loop in `holograph-link/index.ts` awaits
        // `holographNextEmitted` and pushes diffs through the registered
        // linkCallback + emitPerspectiveDiff. The runtime's
        // addPerspectiveLinkAddedListener fires off the same diff.
        const got: string[] = [];
        await client!.perspective.addPerspectiveLinkAddedListener(aliceUuid, [
            (link) => {
                got.push(`${link.data.source}->${link.data.target}`);
            },
        ]);

        await client!.perspective.addLink(aliceUuid, {
            source: "holograph://alice/root",
            target: "holograph://alice/topic/one",
            predicate: "holograph://has-topic",
        });

        // Allow the subscriber loop one tick to drain.
        const deadline = Date.now() + 10_000;
        while (got.length === 0 && Date.now() < deadline) {
            await sleep(100);
        }
        expect(got.length, "subscriber received the addition").to.be.greaterThan(0);
        expect(got[0]).to.equal("holograph://alice/root->holograph://alice/topic/one");
    });

    it("the link is present when re-queried from the perspective", async () => {
        const links = await client!.perspective.queryLinks(
            aliceUuid,
            new LinkQuery({ source: "holograph://alice/root" }),
        );
        expect(links.length, "link queryable from perspective state").to.be.greaterThan(0);
    });

    it("restart preserves perspective state and accepts new commits", async () => {
        // Stop the executor cleanly. The sled stores under
        // `<data>/holograph/<space-id>/` should survive.
        await gracefulShutdown(executorProcess, "executor pre-restart");

        // Restart against the same data path. initData: false skips the
        // rmSync + init that would otherwise wipe sled state.
        executorProcess = await startExecutor(
            APP_DATA_PATH,
            BOOTSTRAP_SEED_PATH,
            apiPort,
            hcAdminPort,
            hcAppPort,
            false,
            undefined,
            undefined,
            undefined,
            undefined,
            false,
            undefined,
            { env: { HOLOGRAPH_DEFAULT_NEIGHBORHOOD: "1" }, initData: false },
        );

        // Fresh Ad4mClient — the previous one's keepalive socket is dead.
        client = new Ad4mClient(baseUrl(apiPort));
        // Unlock the agent keystore that init created in the first run.
        await client.agent.unlock("test-pass", false);

        // Verify the perspective + link from before the restart is still
        // there. Sled-on-disk is the storage of record for both perspective
        // metadata (executor side) and op-DAG state (holograph side).
        const all = await client.perspective.all();
        const alice = all.find((p) => p.uuid === aliceUuid);
        expect(alice, "Alice's perspective survived restart").to.exist;

        const links = await client.perspective.queryLinks(
            aliceUuid,
            new LinkQuery({ source: "holograph://alice/root" }),
        );
        expect(links.length, "pre-restart link still present after reload").to.be.greaterThan(0);

        // New commit after restart must also round-trip through the
        // subscriber loop the restart started fresh.
        const got: string[] = [];
        await client.perspective.addPerspectiveLinkAddedListener(aliceUuid, [
            (link) => {
                got.push(`${link.data.source}->${link.data.target}`);
            },
        ]);

        await client.perspective.addLink(aliceUuid, {
            source: "holograph://alice/root",
            target: "holograph://alice/topic/post-restart",
            predicate: "holograph://has-topic",
        });

        const deadline = Date.now() + 10_000;
        while (got.length === 0 && Date.now() < deadline) {
            await sleep(100);
        }
        expect(got.length, "post-restart subscriber received the new commit").to.be.greaterThan(0);
    });
});
