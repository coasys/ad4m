/**
 * Identity slice 1 (identity plan, Part 3; AR-01 8.4, AR-02 8.2, AR-04's D5 test): executors
 * with no Holochain, a real identity-store this suite starts on a temp file, and the
 * centralized agent language seeded as every executor's agent language.
 *
 * The holder is ad4m-identity-client's `holder` example: the client's create, link and revoke
 * flows, with master keys in a MemoryVault, against executor A over its RPC. Executor B joins
 * with `ad4m-executor init --operator` and `ad4m-executor claim <link>`.
 *
 * Needs `pnpm run prepare-test-local` first (it publishes the centralized agent language and
 * writes its address to scripts/centralized-agent-language-hash).
 */
import { expect } from "chai";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { ChildProcess, execSync } from "child_process";
import { Ad4mClient } from "@coasys/ad4m";
import {
    baseUrl, executorBinary, executorDataPath, pollUntil, quitExecutor, spawnExecutor, startExecutor,
} from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { buildIdentityTools, ClaimCommand, Holder, IdentityStore } from "../helpers/identity";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const TEST_DIR = path.join(__dirname, "..", "tst-tmp");
const SEED_PATH = path.join(TEST_DIR, "identityBootstrapSeed.json");
const STRANGER = "did:scid:coasys:1:QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG";

/** The local test seed with the centralized agent language as the agent language. */
function writeIdentitySeed(): string {
    const seed = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "bootstrapSeed.json"), "utf8"));
    const agentLanguage = fs
        .readFileSync(path.join(__dirname, "..", "scripts", "centralized-agent-language-hash"), "utf8")
        .trim();
    if (!agentLanguage) throw new Error("run prepare-test-local first: no centralized agent language");
    seed.agentLanguage = agentLanguage;
    fs.writeFileSync(SEED_PATH, JSON.stringify(seed));
    return agentLanguage;
}

interface Node {
    name: string;
    dataPath: string;
    ports: number[];
    process: ChildProcess | null;
    client: Ad4mClient;
}

describe("Identity slice 1: create, link, refuse a stranger, revoke, fork, restart", function () {
    this.timeout(600_000);

    let store: IdentityStore;
    let holder: Holder;
    const nodes: Node[] = [];
    let a: Node, b: Node, c: Node;
    let did = "";
    const storeCopy = path.join(TEST_DIR, "identity-store-before-revoke.sqlite");

    async function startNode(name: string, initArgs: string[] = []): Promise<Node> {
        const ports = await getFreePorts(3);
        registerPorts(ports);
        const dataPath = path.join(TEST_DIR, "agents", `identity-${name}`);
        fs.mkdirSync(path.dirname(dataPath), { recursive: true });
        const process = await startExecutor(
            dataPath, SEED_PATH, ports[0], ports[1], ports[2],
            false, undefined, undefined, undefined, undefined, false, undefined, undefined,
            false,          // runHolochain
            true,           // sharedStores
            store.executorEnv(),
            initArgs,
        );
        const client = new Ad4mClient(baseUrl(ports[0]));
        await client.agent.generate("passphrase");
        const node = { name, dataPath, ports, process, client };
        nodes.push(node);
        return node;
    }

    /** Restarts `node` on its data folder (no init, so nothing is wiped). */
    async function restartNode(node: Node) {
        await quitExecutor(node.process!, node.ports[0]);
        node.process = await spawnExecutor([
            "run",
            "--app-data-path", executorDataPath(node.dataPath),
            "--port", String(node.ports[0]),
            "--language-language-only", "false",
            "--run-dapp-server", "false",
            "--run-holochain", "false",
            "--insecure-no-admin-credential",
        ], node.ports[0], { env: store.executorEnv() });
        // No unlock: the identity calls read the device key from the agent's DID, which the
        // executor loads at start, and the cache from the database.
        node.client = new Ad4mClient(baseUrl(node.ports[0]));
    }

    before(async () => {
        if (!fs.existsSync(TEST_DIR)) throw Error("run prepare-test-local before the identity suite");
        writeIdentitySeed();
        buildIdentityTools();
        const [storePort] = await getFreePorts(1);
        registerPorts([storePort]);
        store = await IdentityStore.start(path.join(TEST_DIR, "identity-store"), storePort);
        a = await startNode("a");
        holder = new Holder();
        await holder.send({ cmd: "connect", url: baseUrl(a.ports[0]), token: "" });
    });

    after(async () => {
        await holder?.stop();
        for (const node of nodes) {
            if (node.process) await quitExecutor(node.process, node.ports[0]);
            deregisterPorts(node.ports);
        }
        await store?.stop();
        if (store) deregisterPorts([store.port]);
    });

    it("creates an identity on A: the incept lists A's key as d1", async () => {
        const before = await a.client.identity.status();
        expect(before.did).to.be.null;
        expect(before.deviceKey).to.match(/^z6Mk/);
        expect(before.deviceEnc).to.match(/^z6LS/);

        const created = await holder.send({ cmd: "create" });
        did = created.did;
        expect(did).to.match(/^did:scid:coasys:1:Qm/);
        expect(created.seq).to.equal(0);

        const status = await a.client.identity.status();
        expect(status.did).to.equal(did);
        expect(status.keyId).to.equal("d1");
        expect(status.revoked).to.be.null;
        expect(status.head!.seq).to.equal(0);
        expect(status.head!.store).to.equal(`did:key:${store.key}`);
        // agent.status keeps did as the device did:key and gains the identity.
        const agentStatus = await a.client.agent.status();
        expect(agentStatus.did).to.equal(`did:key:${status.deviceKey}`);
        expect(agentStatus.identity).to.equal(did);

        // The store holds the log the executor checked.
        const page = await (await fetch(`${store.url}/v1/logs/${did.split(":").pop()}`)).json();
        expect(page.events).to.have.length(1);
        const log = await a.client.identity.log(did);
        expect(log.events).to.deep.equal(page.events);
    });

    it("links B, started with --operator, through `ad4m-executor claim`", async () => {
        b = await startNode("b", ["--operator", did]);
        const opened = await holder.send({ cmd: "open", did, store: store.url });
        expect(opened.link).to.match(/^https:\/\/coasys\.id\/o#coasys:offer:1:/);

        const claim = new ClaimCommand(b.ports[0], opened.link, "server b");
        const bCode = await claim.matchCode();
        const claimed = await holder.send({ cmd: "waitClaim" });
        expect(claimed.matchCode).to.equal(bCode);
        expect(claimed.hint).to.equal("server b");
        const bStatus = await b.client.identity.status();
        expect(claimed.deviceKey).to.equal(bStatus.deviceKey);

        const confirmed = await holder.send({ cmd: "confirm" });
        expect(confirmed.seq).to.equal(1);
        expect(await claim.exited).to.equal(0);
        expect(claim.output.some((l) => l.startsWith(`Joined ${did} as d2`))).to.be.true;

        const joined = await b.client.identity.status();
        expect(joined.did).to.equal(did);
        expect(joined.keyId).to.equal("d2");
        const devices = await a.client.identity.devices();
        expect(devices.map((d) => [d.keyId, d.thisDevice])).to.deep.equal([["d1", true], ["d2", false]]);
    });

    it("refuses a claim on an executor started without --operator, and a stranger's first link", async () => {
        c = await startNode("c");
        const opened = await holder.send({ cmd: "open", did, store: store.url });
        let refused = await c.client.identity.claimOffer(opened.link).then(() => null, (e) => e);
        expect(String(refused?.message ?? refused)).to.contain("no-operator");

        // With an operator recorded for another identity, the first link must come from it.
        execSync(`${executorBinary()} init --data-path ${executorDataPath(c.dataPath)} --network-bootstrap-seed ${SEED_PATH} --operator ${STRANGER}`);
        refused = await c.client.identity.claimOffer(opened.link).then(() => null, (e) => e);
        expect(String(refused?.message ?? refused)).to.contain("first-link");
        // `init` never replaces a recorded operator.
        expect(() => execSync(
            `${executorBinary()} init --data-path ${executorDataPath(c.dataPath)} --network-bootstrap-seed ${SEED_PATH} --operator ${did}`,
            { stdio: "pipe" },
        )).to.throw(/This executor's operator is/);
        expect((await c.client.identity.status()).did).to.be.null;
    });

    it("revokes B's key: B reports it revoked", async () => {
        // Keep the store's log as it is now (incept, add) for the fork test.
        await store.stop();
        store.saveDb(storeCopy);
        await store.restart();

        const revoked = await holder.send({ cmd: "revoke", did, keyIds: ["d2"] });
        expect(revoked.seq).to.equal(2);
        const bStatus = await b.client.identity.status();
        expect(bStatus.did).to.equal(did);
        expect(bStatus.keyId).to.equal("d2");
        expect(bStatus.revoked).to.equal(2);
        const devices = await a.client.identity.devices();
        expect(devices.find((d) => d.keyId === "d2")!.revokedSeq).to.equal(2);
        expect((await a.client.identity.status()).revoked).to.be.null;
    });

    it("raises identity-fork on A when the store answers a shorter log, and keeps A's history", async () => {
        const forks: any[] = [];
        a.client.on("identity-fork", (event) => forks.push(event));
        // The store loses the revoke: it serves the log it held before (two events).
        await store.stop();
        store.restoreDb(storeCopy);
        await store.restart();

        const log = await a.client.identity.log(did, 0, true);
        expect(log.events).to.have.length(3);
        expect(log.fork).to.not.be.null;
        expect(log.fork!.seq).to.equal(2);
        await pollUntil(() => forks.length > 0, { timeoutMs: 10_000, intervalMs: 200, label: "identity-fork event" });
        expect(forks[0].did).to.equal(did);
        expect(forks[0].seq).to.equal(2);
        // A second refresh records no second fork.
        await a.client.identity.log(did, 0, true);
        await new Promise((r) => setTimeout(r, 1000));
        expect(forks).to.have.length(1);
        expect((await a.client.identity.status()).fork!.seq).to.equal(2);
    });

    it("serves the log from the cache after A restarts, with the store down", async () => {
        await store.stop();
        await restartNode(a);
        const log = await a.client.identity.log(did);
        expect(log.events).to.have.length(3);
        expect(log.head!.seq).to.equal(2);
        const status = await a.client.identity.status();
        expect(status.did).to.equal(did);
        expect(status.keyId).to.equal("d1");
        expect(status.fork!.seq).to.equal(2);
        expect((await a.client.agent.status()).identity).to.equal(did);
    });
});
