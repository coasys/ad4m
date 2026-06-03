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

// Path to the holograph-link Language bundle. install_language()
// content-addresses the bundle (SHA-256 -> CIDv1 -> base58btc, "Qm"
// prefixed) and rejects a bundle whose hash doesn't match its install
// address, so the test must use the bundle's content hash, not the
// `holograph_link_default_address()` package-id hash. We derive the
// content hash at test-setup time by shelling out to the
// `print_holograph_address` binary (same algorithm Rust uses).
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

function computeHolographAddress(): string {
    const bin = path.resolve(
        __dirname,
        "..",
        "..",
        "..",
        "target",
        "debug",
        "print_holograph_address",
    );
    return execSync(`${bin} ${HOLOGRAPH_BUNDLE_PATH}`).toString().trim();
}

const TEST_DIR = path.join(`${__dirname}/../tst-tmp`);
const APP_DATA_PATH = path.join(TEST_DIR, "agents", "holograph-alice");
const BOOTSTRAP_SEED_PATH = path.join(`${__dirname}/../bootstrapSeed.json`);

/**
 * Drop the holograph-link bundle onto disk under its content-address
 * directory so `install_language`'s hash-verification accepts it
 * without a language-language fetch.
 */
function preinstallHolographBundle(dataPath: string, address: string) {
    expect(fs.existsSync(HOLOGRAPH_BUNDLE_PATH)).to.equal(
        true,
        `holograph-link bundle missing — build it first: cd bootstrap-languages/holograph-link && deno run --allow-all esbuild.ts`,
    );
    // dataPath is symlinked to the hashed effective path inside startExecutor,
    // so writes through dataPath land in the executor's app-data-path.
    const targetDir = path.join(dataPath, "ad4m", "languages", address);
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
    let holographAddress: string;

    before(async () => {
        // Derive the bundle's content address before booting anything;
        // the test reuses it everywhere (install path, publish arg,
        // restart pre-install).
        holographAddress = computeHolographAddress();

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
            {
                env: {
                    HOLOGRAPH_DEFAULT_NEIGHBORHOOD: "1",
                    // Step 8 Gap C hook: resolve_link_language reads
                    // this once at first call, computes the bundle's
                    // AD4M content hash, and substitutes that address
                    // for empty linkLanguage publishes. Without it the
                    // substitution falls back to a package-id-derived
                    // address that install_language can't install.
                    HOLOGRAPH_LINK_BUNDLE_PATH: HOLOGRAPH_BUNDLE_PATH,
                },
            },
        );

        // Pre-install the bundle now that startExecutor has run `init`
        // (which creates the data-path layout) but before any test calls
        // publishFromPerspective (which triggers install_language).
        preinstallHolographBundle(APP_DATA_PATH, holographAddress);

        client = new Ad4mClient(baseUrl(apiPort));
        await client.agent.generate("test-pass");
    });

    after(async () => {
        await gracefulShutdown(executorProcess, "executor");
        deregisterPorts([apiPort, hcAdminPort, hcAppPort]);
    });

    it("derives a stable Qm-prefixed content address", () => {
        expect(holographAddress).to.match(/^Qm[1-9A-HJ-NP-Za-km-z]+$/);
    });

    it("agent reaches initialized state with the flag on", async () => {
        const status = await client!.agent.status();
        expect(status).to.not.be.null;
        expect(status!.isInitialized).to.equal(true);
    });

    let aliceUuid: string;
    let neighbourhoodUrl: string;

    it("publishFromPerspective(undefined) resolves via the env-default switch", async () => {
        const perspective = await client!.perspective.add("holograph-alice-1");
        aliceUuid = perspective.uuid;

        // No explicit linkLanguage. The Step 8 Gap C hook reads
        // HOLOGRAPH_LINK_BUNDLE_PATH, derives the bundle's AD4M
        // content hash, and substitutes that as the link_language.
        // install_language then finds the pre-installed bundle at
        // <data>/ad4m/languages/<contentHash>/bundle.js and loads it
        // cleanly.
        neighbourhoodUrl = await client!.neighbourhood.publishFromPerspective(
            aliceUuid,
            // @ts-expect-error — client typings insist on a string;
            // Rust accepts Option<String> and routes undefined / empty
            // through resolve_link_language. PR-C updates the typings.
            undefined,
            new Perspective([]),
        );

        expect(neighbourhoodUrl).to.match(/^neighbourhood:\/\//);
    });

    it("perspective shows the holograph-link address as linkLanguage", async () => {
        const all = await client!.perspective.all();
        const alice = all.find((p) => p.uuid === aliceUuid);
        expect(alice, "alice perspective present").to.exist;
        // NeighbourhoodExpression wraps the Neighbourhood under `data`.
        expect(alice!.neighbourhood?.data?.linkLanguage).to.equal(holographAddress);
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
