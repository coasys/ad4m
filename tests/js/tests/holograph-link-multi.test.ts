/**
 * Step 9 — holograph-link Language two-conductor end-to-end test.
 *
 * Single-conductor proof lives in `holograph-link.test.ts`; this file
 * extends to two AD4M conductors (Alice + Bob) in separate processes,
 * synced via Tx5 transport against a local SBD signal server. Together
 * with the single-conductor test it closes SPIKE §2.5 exit checks #4
 * and #6 (cross-node propagation + JS-driven integration test).
 *
 * What this file proves on top of the single-conductor scaffold:
 *   - Two AD4M conductors with `HOLOGRAPH_DEFAULT_NEIGHBORHOOD=1` and
 *     `HOLOGRAPH_SBD_URL=<bootstrap-srv>` reach each other end-to-end
 *     via the Tx5/SBD path swapped in by `holograph_wires::
 *     build_dyn_space_inner`.
 *   - Alice publishes a neighbourhood; Bob joins via the returned URL;
 *     Alice's commits flow through Tx5 → Bob's perspective subscriber.
 *   - Bidirectional: Bob commits back; Alice's subscriber observes it.
 */

import path from "path";
import fs from "fs-extra";
import { fileURLToPath } from "url";
import { ChildProcess, execSync } from "node:child_process";
import { expect } from "chai";
import { Ad4mClient, Perspective } from "@coasys/ad4m";
import {
    baseUrl,
    sleep,
    startExecutor,
    runHcLocalServices,
    gracefulShutdown,
} from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

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
const BOOTSTRAP_SEED_PATH = path.join(`${__dirname}/../bootstrapSeed.json`);

function preinstallHolographBundle(dataPath: string, address: string) {
    const targetDir = path.join(dataPath, "ad4m", "languages", address);
    fs.ensureDirSync(targetDir);
    fs.copyFileSync(HOLOGRAPH_BUNDLE_PATH, path.join(targetDir, "bundle.js"));
}

interface Conductor {
    name: string;
    apiPort: number;
    hcAdminPort: number;
    hcAppPort: number;
    process: ChildProcess;
    client: Ad4mClient;
    dataPath: string;
}

describe("holograph-link Language end-to-end (two conductors via Tx5)", function () {
    this.timeout(300_000);

    let holographAddress: string;
    let sbdUrl: string;
    let bootstrapUrl: string;
    let localServicesProcess: ChildProcess | null = null;
    let alice: Conductor | null = null;
    let bob: Conductor | null = null;
    let charlie: Conductor | null = null;

    async function bootConductor(name: string): Promise<Conductor> {
        const [apiPort, hcAdminPort, hcAppPort] = await getFreePorts(3);
        registerPorts([apiPort, hcAdminPort, hcAppPort]);
        const dataPath = path.join(TEST_DIR, "agents", `holograph-multi-${name}`);
        const proc = await startExecutor(
            dataPath,
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
                    HOLOGRAPH_LINK_BUNDLE_PATH: HOLOGRAPH_BUNDLE_PATH,
                    HOLOGRAPH_SBD_URL: sbdUrl,
                    HOLOGRAPH_SBD_PLAINTEXT: "1",
                    HOLOGRAPH_BOOTSTRAP_URL: bootstrapUrl,
                    RUST_LOG:
                        process.env.HOLOGRAPH_DEBUG === "1"
                            ? "info,kitsune2_core::factories::core_bootstrap=debug,kitsune2_transport_tx5=debug,kitsune2_gossip=debug,holograph=debug"
                            : process.env.RUST_LOG ?? "info,holograph=info",
                },
            },
        );
        preinstallHolographBundle(dataPath, holographAddress);
        const client = new Ad4mClient(baseUrl(apiPort));
        await client.agent.generate(`pass-${name}`);
        return { name, apiPort, hcAdminPort, hcAppPort, process: proc, client, dataPath };
    }

    before(async () => {
        holographAddress = computeHolographAddress();

        // Boot the bootstrap-srv that doubles as the Tx5 SBD signal
        // server. The plain-text ws:// URL on loopback is acceptable
        // for the spike — `signal_allow_plain_text: true` in the
        // builder mirrors what the K2 own test harness uses.
        const services = await runHcLocalServices();
        localServicesProcess = services.process;
        const port = services.bootstrapUrl!.replace("https://", "");
        sbdUrl = `ws://${port}`;
        bootstrapUrl = `http://${port}`;

        alice = await bootConductor("alice");
        bob = await bootConductor("bob");
    });

    after(async () => {
        if (charlie) {
            await gracefulShutdown(charlie.process, "charlie");
            deregisterPorts([charlie.apiPort, charlie.hcAdminPort, charlie.hcAppPort]);
        }
        if (alice) {
            await gracefulShutdown(alice.process, "alice");
            deregisterPorts([alice.apiPort, alice.hcAdminPort, alice.hcAppPort]);
        }
        if (bob) {
            await gracefulShutdown(bob.process, "bob");
            deregisterPorts([bob.apiPort, bob.hcAdminPort, bob.hcAppPort]);
        }
        if (localServicesProcess) {
            await gracefulShutdown(localServicesProcess, "bootstrap-srv");
        }
    });

    let neighbourhoodUrl: string;
    let aliceUuid: string;
    let bobUuid: string;

    it("Alice publishes a holograph-backed neighbourhood", async () => {
        const p = await alice!.client.perspective.add("alice-multi");
        aliceUuid = p.uuid;
        neighbourhoodUrl = await alice!.client.neighbourhood.publishFromPerspective(
            aliceUuid,
            // @ts-expect-error see holograph-link.test.ts
            undefined,
            new Perspective([]),
        );
        expect(neighbourhoodUrl).to.match(/^neighbourhood:\/\//);
    });

    it("Bob joins via the neighbourhood URL", async () => {
        const joined = await bob!.client.neighbourhood.joinFromUrl(neighbourhoodUrl);
        bobUuid = joined.uuid;
        expect(joined.sharedUrl).to.equal(neighbourhoodUrl);
        const link =
            joined.neighbourhood?.data?.linkLanguage ??
            joined.neighbourhood?.linkLanguage;
        expect(link).to.equal(holographAddress);
    });

    it("Bob receives Alice's commit through Tx5 within 15s", async () => {
        const got: string[] = [];
        await bob!.client.perspective.addPerspectiveLinkAddedListener(bobUuid, [
            (l) => got.push(`${l.data.source}->${l.data.target}`),
        ]);
        await alice!.client.perspective.addLink(aliceUuid, {
            source: "holograph://alice/a",
            target: "holograph://alice/b",
            predicate: "holograph://multi/edge",
        });
        const deadline = Date.now() + 15_000;
        while (got.length === 0 && Date.now() < deadline) {
            await sleep(200);
        }
        expect(got.length, "Bob saw Alice's link").to.be.greaterThan(0);
        expect(got[0]).to.equal("holograph://alice/a->holograph://alice/b");
    });

    it("Alice receives Bob's return commit within 15s", async () => {
        const got: string[] = [];
        await alice!.client.perspective.addPerspectiveLinkAddedListener(aliceUuid, [
            (l) => got.push(`${l.data.source}->${l.data.target}`),
        ]);
        await bob!.client.perspective.addLink(bobUuid, {
            source: "holograph://bob/c",
            target: "holograph://bob/d",
            predicate: "holograph://multi/edge",
        });
        const deadline = Date.now() + 15_000;
        while (got.length === 0 && Date.now() < deadline) {
            await sleep(200);
        }
        expect(got.length, "Alice saw Bob's link").to.be.greaterThan(0);
    });

    it("late-join Charlie sees historical diffs via gossip catch-up", async () => {
        // Charlie boots AFTER Alice and Bob have exchanged the two
        // commits above. He should catch up via K2 gossip on first
        // join — no fresh commits required.
        charlie = await bootConductor("charlie");
        const joined = await charlie.client.neighbourhood.joinFromUrl(neighbourhoodUrl);
        const charlieUuid = joined.uuid;

        // Subscribe before any commits could be missed; gossip pushes
        // historical ops asynchronously after join.
        const got: string[] = [];
        await charlie.client.perspective.addPerspectiveLinkAddedListener(charlieUuid, [
            (l) => got.push(`${l.data.source}->${l.data.target}`),
        ]);

        // Wait up to 30s for gossip to catch up. The two prior commits
        // (alice/a->b, bob/c->d) should both surface. K2's gossip and
        // publish paths can both deliver the same op, so dedupe before
        // asserting set-equality.
        const deadline = Date.now() + 30_000;
        const unique = () => Array.from(new Set(got));
        while (unique().length < 2 && Date.now() < deadline) {
            await sleep(250);
        }
        const uniques = unique().slice().sort();
        expect(uniques).to.deep.equal([
            "holograph://alice/a->holograph://alice/b",
            "holograph://bob/c->holograph://bob/d",
        ]);
    });
});
