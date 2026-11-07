/**
 * Identity end to end (create, link, the person's profile, login with a device key, the first-link refusal,
 * revoke and the end of the revoked key's sessions, DID resolution on an executor that never saw the log, fork,
 * restart): executors
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
import { Ad4mClient, ExpressionProof, Link, LinkExpression, Perspective } from "@coasys/ad4m";
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

describe("Identity: create, link, refuse a stranger, revoke, fork, restart", function () {
    this.timeout(600_000);

    let store: IdentityStore;
    let holder: Holder;
    const nodes: Node[] = [];
    let a: Node, b: Node, c: Node;
    let did = "";
    let agentLanguage = "";
    const storeCopy = path.join(TEST_DIR, "identity-store-before-revoke.sqlite");

    /**
     * Starts an executor on `seedPath` (the suite's seed by default), pointed at the store by
     * `env` (`store.executorEnv()` by default).
     */
    async function startNode(
        name: string,
        initArgs: string[] = [],
        { seedPath = SEED_PATH, env = store.executorEnv() }: { seedPath?: string; env?: Record<string, string> } = {},
    ): Promise<Node> {
        const ports = await getFreePorts(3);
        registerPorts(ports);
        const dataPath = path.join(TEST_DIR, "agents", `identity-${name}`);
        fs.mkdirSync(path.dirname(dataPath), { recursive: true });
        const process = await startExecutor(
            dataPath, seedPath, ports[0], ports[1], ports[2],
            false, undefined, undefined, undefined, undefined, false, undefined, undefined,
            false,          // runHolochain
            true,           // sharedStores
            env,
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
        agentLanguage = writeIdentitySeed();
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
        // The configured store (AD4M_IDENTITY_STORE_URL/KEYS) wins over the seed's, trusted.
        expect(before.store).to.deep.equal({ url: store.url, trusted: true });

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
        // Binding A published the person's profile, anchored at the incept.
        const record = await (await fetch(`${store.url}/v1/agents/${did}`)).json();
        expect(record.signedBy).to.equal(agentStatus.did);
        expect(record.anchor).to.equal(0);

        // The store holds the log the executor checked.
        const page = await (await fetch(`${store.url}/v1/logs/${did.split(":").pop()}`)).json();
        expect(page.events).to.have.length(1);
        const log = await a.client.identity.log(did);
        expect(log.events).to.deep.equal(page.events);
    });

    it("agent.lock and agent.unlock answer, and announce, the identity A speaks for", async () => {
        // A hook that takes the event as the new status (`useMe`) would otherwise show
        // `identity: null` after every lock or unlock.
        const events: any[] = [];
        const stop = a.client.on("agent-status-changed", (event) => events.push(event));
        try {
            const locked = await a.client.agent.lock("passphrase");
            expect(locked.isUnlocked).to.be.false;
            expect(locked.identity).to.equal(did);
            await pollUntil(() => events.length >= 1, { timeoutMs: 10_000, intervalMs: 100, label: "agent-status-changed on lock" });
            expect(events[0].agent.isUnlocked).to.be.false;
            expect(events[0].agent.identity).to.equal(did);
        } finally {
            const unlocked = await a.client.agent.unlock("passphrase", false);
            expect(unlocked.isUnlocked).to.be.true;
            expect(unlocked.identity).to.equal(did);
        }
        await pollUntil(
            () => events.some((e) => e.agent.isUnlocked),
            { timeoutMs: 10_000, intervalMs: 100, label: "agent-status-changed on unlock" },
        );
        stop();
        const announced = events.find((e) => e.agent.isUnlocked);
        expect(announced.agent.identity).to.equal(did);
    });

    it("links B, started with --operator, through `ad4m-executor claim`", async () => {
        b = await startNode("b", ["--operator", did]);
        const opened = await holder.send({ cmd: "open", did, store: store.url });
        expect(opened.link).to.match(/^https:\/\/id\.coasys\.org\/o#coasys:offer:1:/);

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

    it("publishes the person's profile under the did:scid on joining and on every profile publish", async () => {
        const recordOf = async () => (await fetch(`${store.url}/v1/agents/${did}`)).json();
        const keyOf = async (node: Node) => `did:key:${(await node.client.identity.status()).deviceKey}`;

        // B's join wrote it, signed by B's key and anchored at the add that listed B.
        let record = await recordOf();
        expect(record.signedBy).to.equal(await keyOf(b));
        expect(record.anchor).to.equal(1);
        expect(record.profile.author).to.equal(did);
        expect(record.profile.data.did).to.equal(did);

        // A profile update on A publishes A's did:key profile and the person's.
        const link = new LinkExpression();
        link.author = await keyOf(a);
        link.timestamp = new Date().toISOString();
        link.data = new Link({ source: did, predicate: "sioc://has_name", target: "literal://string:Ada" });
        link.proof = new ExpressionProof("sig", "key");
        await a.client.agent.updatePublicPerspective(new Perspective([link]));
        record = await recordOf();
        expect(record.signedBy).to.equal(await keyOf(a));
        expect(record.anchor).to.equal(1);

        // Others see the person: the profile verifies at its anchor, and byDID answers it.
        const expression = await b.client.expression.get(`${agentLanguage}://${did}`);
        expect(expression.author).to.equal(did);
        expect(expression.proof.valid).to.be.true;
        const person = await b.client.agent.byDID(did);
        expect(person.did).to.equal(did);
        expect(person.perspective!.links.map((l) => l.data.target)).to.deep.equal(["literal://string:Ada"]);
        // The device's own profile stays under its did:key.
        expect((await b.client.agent.byDID(await keyOf(a))).did).to.equal(await keyOf(a));
    });

    /** `signer` signs a login to `target` with its own device key; `target` answers the session. */
    async function keyLogin(signer: Node, target: Node) {
        const challenge = await target.client.agent.sessionChallenge();
        const proof = await signer.client.agent.signSessionStatement({
            v: 1,
            purpose: "login",
            executor: challenge.executor,
            challenge: challenge.challenge,
            client: { kind: "native" },
            issuedAt: Math.floor(Date.now() / 1000),
        });
        const app = {
            appName: `${signer.name} shell`,
            appDesc: "key login test",
            appDomain: "coasys.org",
            capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
        };
        const session = await target.client.agent.loginWithKey(proof, app);
        return { proof, app, session, client: new Ad4mClient(baseUrl(target.ports[0]), session.token) };
    }

    let bOnA: Ad4mClient | null = null;

    it("logs B's device key in to A as A's agent, and A's key in to B as its operator", async () => {
        // A has no operator record, and its main agent speaks for the identity: B's device key
        // (d2 of the same identity) gets a session on A's main agent.
        const { proof, app, session, client } = await keyLogin(b, a);
        expect(session.role).to.equal("app");
        expect(session.scid).to.equal(did);
        expect(session.did).to.equal((await a.client.agent.status()).did);
        const info = await client.agent.currentSession();
        expect(info.role).to.equal("app");
        expect(info.keyId).to.equal("d2");
        expect(info.grantId).to.equal(session.grantId);
        // It acts as A's agent: A's identity, A's device key d1.
        const status = await client.identity.status();
        expect(status.did).to.equal(did);
        expect(status.keyId).to.equal("d1");
        bOnA = client;

        // The challenge was used: the same statement again is refused.
        const replay = await a.client.agent.loginWithKey(proof, app).then(() => null, (e) => e);
        expect(String(replay?.message ?? replay)).to.contain("challenge");

        // B was started with --operator <the identity>: A's device key gets the operator role.
        const onB = await keyLogin(a, b);
        expect(onB.session.role).to.equal("operator");
        const operator = await onB.client.agent.currentSession();
        expect(operator.operatorSource).to.equal("key");
        // Log out ends that session only.
        expect(await onB.client.agent.logout()).to.be.true;
        await pollUntil(
            () => onB.client.agent.currentSession().then(() => false, (e) => /Unauthorized/.test(String(e?.message ?? e))),
            { timeoutMs: 10_000, label: "the logged-out key session refused" },
        );
        expect((await client.agent.currentSession()).grantId).to.equal(session.grantId);
    });

    it("refuses a claim on an executor started without --operator, and a stranger's first link", async () => {
        c = await startNode("c");
        const opened = await holder.send({ cmd: "open", did, store: store.url });
        let refused = await c.client.identity.claimOffer(opened.link).then(() => null, (e) => e);
        expect(String(refused?.message ?? refused)).to.contain("no-operator");

        // With an operator recorded for another identity, the first link must come from it.
        execSync(`"${executorBinary()}" init --data-path "${executorDataPath(c.dataPath)}" --network-bootstrap-seed "${SEED_PATH}" --operator ${STRANGER}`);
        refused = await c.client.identity.claimOffer(opened.link).then(() => null, (e) => e);
        expect(String(refused?.message ?? refused)).to.contain("first-link");
        // `init` never replaces a recorded operator.
        expect(() => execSync(
            `"${executorBinary()}" init --data-path "${executorDataPath(c.dataPath)}" --network-bootstrap-seed "${SEED_PATH}" --operator ${did}`,
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
        // The binding stays, but B's key no longer speaks for the person: agent.status (and
        // agent.get, which reads the same check) names no identity.
        expect((await b.client.agent.status()).identity).to.be.null;
        expect((await a.client.agent.status()).identity).to.equal(did);
        const devices = await a.client.identity.devices();
        expect(devices.find((d) => d.keyId === "d2")!.revokedSeq).to.equal(2);
        expect((await a.client.identity.status()).revoked).to.be.null;
    });

    it("ends B's key session on A once A learns B's key is revoked", async () => {
        expect(bOnA, "the key login test ran").to.not.be.null;
        // A reads the log the revoke made (its accepted head moves): the key's verdict changes,
        // and every grant bound to it ends.
        await a.client.identity.log(did, 0, true);
        await pollUntil(
            () => bOnA!.agent.currentSession().then(() => false, (e) => /Unauthorized/.test(String(e?.message ?? e))),
            { timeoutMs: 15_000, label: "B's key session on A refused" },
        );
        // A new login by the revoked key is refused.
        const refused = await keyLogin(b, a).then(() => null, (e) => e);
        expect(String(refused?.message ?? refused)).to.contain("key");
    });

    it("resolves the DID on an executor that never saw its log", async () => {
        const d = await startNode("d");
        // The store's host as a did:scid src. It names d's configured store, so d reads it
        // through the store (the agent language fetches a foreign src over HTTPS only).
        const storeHost = store.url.replace(/^https?:\/\//, "").replace(/\/$/, "");
        const resolved = await d.client.identity.resolve(`${did}?src=${storeHost}`);
        const meta = resolved.didResolutionMetadata;
        expect(meta.error, meta["coasys:message"]).to.be.undefined;
        expect(meta.contentType).to.equal("application/did+ld+json");
        expect(meta["coasys:source"]).to.equal(storeHost);
        expect(meta["coasys:headTrusted"]).to.equal(true);
        const doc = resolved.didDocument!;
        expect(doc.id).to.equal(did);
        expect(doc.alsoKnownAs).to.deep.equal([`${did}?src=${storeHost}`]);
        const ids = doc.verificationMethod.map((m) => m.id);
        expect(ids).to.include("#d1");
        expect(ids).to.not.include("#d2");
        expect(resolved.didDocumentMetadata.versionId).to.equal("2");
        expect(resolved.didDocumentMetadata.deactivated).to.equal(false);

        // Version 1 (before the revoke) lists both device keys, from d's cache now.
        const v1 = await d.client.identity.resolve(`${did}?versionId=1`);
        expect(v1.didResolutionMetadata["coasys:source"]).to.equal("cache");
        expect(v1.didDocument!.verificationMethod.map((m) => m.id)).to.include.members(["#d1", "#d2"]);
        expect(v1.didDocumentMetadata.nextVersionId).to.equal("2");
        const method = await d.client.identity.resolve(`${did}?versionId=1#d2`);
        expect(method.didDocument).to.be.null;
        expect(method.verificationMethod!.id).to.equal(`${did}#d2`);

        const timed = await d.client.identity.resolve(`${did}?versionTime=2026-01-01T00:00:00Z`);
        expect(timed.didResolutionMetadata.error).to.equal("invalidDidUrl");
        expect(timed.didDocument).to.be.null;
        const past = await d.client.identity.resolve(`${did}?versionId=9`);
        expect(past.didResolutionMetadata.error).to.equal("notFound");

        // A src at another loopback port is refused before any fetch (identity.allowPrivateSrc
        // is off); the configured store still answers.
        const refused = await d.client.identity.resolve(`${did}?src=127.0.0.1:1`, { fresh: true });
        expect(refused.didResolutionMetadata.error).to.be.undefined;
        expect(refused.didResolutionMetadata["coasys:skipped"]![0].src).to.equal("127.0.0.1:1");
        expect(refused.didResolutionMetadata["coasys:skipped"]![0].reason).to.match(/^refused/);

        // B's revoked device key still names its person, through the store's key index.
        const bKey = (await b.client.identity.status()).deviceKey;
        const byKey = await d.client.identity.resolve(`did:key:${bKey}`);
        expect(byKey.didResolutionMetadata.error, byKey.didResolutionMetadata["coasys:message"]).to.be.undefined;
        expect(byKey.didDocument!.id).to.equal(did);
        expect(byKey.didResolutionMetadata["coasys:keyId"]).to.equal("d2");
        expect(byKey.didResolutionMetadata["coasys:keyValid"]).to.equal(false);
        const aKey = (await a.client.identity.status()).deviceKey;
        const byAKey = await d.client.identity.resolve(`did:key:${aKey}`);
        expect(byAKey.didResolutionMetadata["coasys:keyId"]).to.equal("d1");
        expect(byAKey.didResolutionMetadata["coasys:keyValid"]).to.equal(true);

        // agent.status keeps the device's did:key document.
        expect((await d.client.agent.status()).did).to.match(/^did:key:/);
    });

    it("takes the store from the network's seed when the config names none", async () => {
        // The suite's seed plus this store as its identityStore; no AD4M_IDENTITY_STORE_*.
        const seedPath = path.join(TEST_DIR, "identityStoreSeed.json");
        const seed = JSON.parse(fs.readFileSync(SEED_PATH, "utf8"));
        seed.identityStore = { url: store.url, keys: [store.key] };
        fs.writeFileSync(seedPath, JSON.stringify(seed));
        const e = await startNode("e", [], { seedPath, env: {} });

        expect((await e.client.identity.status()).store).to.deep.equal({ url: store.url, trusted: true });
        // The seed's store serves the log under the seed's key: e resolves the DID at its head.
        const resolved = await e.client.identity.resolve(did, { fresh: true });
        const meta = resolved.didResolutionMetadata;
        expect(meta.error, meta["coasys:message"]).to.be.undefined;
        expect(meta["coasys:headTrusted"]).to.equal(true);
        expect(resolved.didDocument!.id).to.equal(did);
        expect(resolved.didDocumentMetadata.versionId).to.equal("2");
    });

    it("keeps A's history and raises no identity-fork when the store is restored behind it", async () => {
        const forks: any[] = [];
        a.client.on("identity-fork", (event) => forks.push(event));
        // The store is restored from a backup taken before the revoke: it serves the log it
        // held then (two events), on the same branch as A's.
        await store.stop();
        store.restoreDb(storeCopy);
        await store.restart();

        for (let i = 0; i < 2; i++) {
            const log = await a.client.identity.log(did, 0, true);
            expect(log.events).to.have.length(3);
            expect(log.head!.seq).to.equal(2);
            expect(log.fork).to.be.null;
        }
        // The resolver keeps answering A's cached log at its accepted head.
        const resolved = await a.client.identity.resolve(did, { fresh: true });
        const meta = resolved.didResolutionMetadata;
        expect(meta.error, meta["coasys:message"]).to.be.undefined;
        expect(meta["coasys:headTrusted"]).to.equal(true);
        expect(meta["coasys:fork"]).to.be.undefined;
        expect(resolved.didDocumentMetadata.versionId).to.equal("2");
        expect(meta["coasys:skipped"]!.some((s) => /the store is behind/.test(s.reason))).to.equal(true);
        await new Promise((r) => setTimeout(r, 1000));
        expect(forks).to.have.length(0);
        const status = await a.client.identity.status();
        expect(status.fork).to.be.null;
        // The status shows the store behind, at the head it answers (seq 1 of A's 2).
        expect(status.storeBehind?.seq).to.equal(1);
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
        expect(status.fork).to.be.null;
        expect((await a.client.agent.status()).identity).to.equal(did);
    });
});

/**
 * The device inbox end to end, on a store of its own: a desktop executor (started with an admin
 * credential and the custody channel) shows its device string, the holder scans it and pushes
 * an offer ticket into its pre-join inbox, and the executor claims it; then the holder seals the
 * master key to the desktop's key, and only the custody service takes it. The private record
 * lists the desktop and whether it holds the master key; the desktop asks for the master key
 * through the request box and the holder answers.
 */
describe("Identity: the device inbox, a ticket in the pre-join inbox and a master key for the custody service", function () {
    this.timeout(600_000);
    const CREDENTIAL = "custody-service-credential";

    let store: IdentityStore;
    let holder: Holder;
    // The desktop's custody service, connected to the desktop's executor.
    let custody: Holder;
    const nodes: Node[] = [];
    let home: Node, desk: Node;
    let did = "";

    async function start(
        name: string,
        opts: { credential?: string; env?: Record<string, string>; initArgs?: string[] } = {},
    ): Promise<Node> {
        const ports = await getFreePorts(3);
        registerPorts(ports);
        const dataPath = path.join(TEST_DIR, "agents", `identity-${name}`);
        fs.mkdirSync(path.dirname(dataPath), { recursive: true });
        const process = await startExecutor(
            dataPath, SEED_PATH, ports[0], ports[1], ports[2],
            false, opts.credential, undefined, undefined, undefined, false, undefined, undefined,
            false,          // runHolochain
            true,           // sharedStores
            { ...store.executorEnv(), ...(opts.env ?? {}) },
            opts.initArgs ?? [],
        );
        const client = new Ad4mClient(baseUrl(ports[0]), opts.credential);
        await client.agent.generate("passphrase");
        const node = { name, dataPath, ports, process, client };
        nodes.push(node);
        return node;
    }

    async function refused(call: Promise<unknown>): Promise<string> {
        try {
            await call;
        } catch (e) {
            return e instanceof Error ? e.message : String(e);
        }
        throw new Error("the call was not refused");
    }

    before(async () => {
        if (!fs.existsSync(TEST_DIR)) throw Error("run prepare-test-local before the identity suite");
        writeIdentitySeed();
        buildIdentityTools();
        const [storePort] = await getFreePorts(1);
        registerPorts([storePort]);
        store = await IdentityStore.start(path.join(TEST_DIR, "identity-store-inbox"), storePort);
        home = await start("inbox-home");
        holder = new Holder();
        await holder.send({ cmd: "connect", url: baseUrl(home.ports[0]), token: "" });
        did = (await holder.send({ cmd: "create" })).did;
    });

    after(async () => {
        await holder?.stop();
        await custody?.stop();
        for (const node of nodes) {
            if (node.process) await quitExecutor(node.process, node.ports[0]);
            deregisterPorts(node.ports);
        }
        await store?.stop();
        if (store) deregisterPorts([store.port]);
    });

    it("links a desktop through its device string: its claim, then link-ticket with the match code", async () => {
        desk = await start("inbox-desktop", {
            credential: CREDENTIAL,
            env: { AD4M_CUSTODY_CHANNEL: "true" },
            initArgs: ["--operator", did],
        });
        const tickets: any[] = [];
        desk.client.on("link-ticket", (event) => tickets.push(event));
        const status = await desk.client.identity.status();

        const prepared = await desk.client.identity.prepareDevice();
        expect(prepared.device).to.match(/^coasys:device:1:/);
        expect(prepared.sign).to.equal(status.deviceKey);
        expect(prepared.enc).to.equal(status.deviceEnc);
        expect(prepared.expires - Math.floor(Date.now() / 1000)).to.be.within(200, 300);

        // The holder scans the device string: a bound offer, its ticket in the pre-join inbox.
        const pushed = await holder.send({ cmd: "scan", did, store: store.url, device: prepared.device });
        await pollUntil(() => tickets.length > 0, { timeoutMs: 30_000, intervalMs: 200, label: "link-ticket event" });
        expect(tickets[0].did).to.equal(did);
        expect(tickets[0].offerId).to.equal(pushed.offerId);

        const claimed = await holder.send({ cmd: "waitClaim" });
        expect(claimed.matchCode).to.equal(tickets[0].matchCode);
        expect(claimed.deviceKey).to.equal(status.deviceKey);
        // The custody channel lets the desktop's executor say it may keep the master key.
        expect(claimed.canKeepMaster).to.be.true;
        const confirmed = await holder.send({ cmd: "confirm", name: "Desk", kind: "desktop-app" });
        expect(confirmed).to.deep.equal({ seq: 1, keyId: "d2", master: null, record: 1 });

        await pollUntil(
            async () => (await desk.client.identity.joinStatus(pushed.offerId)).state === "spent",
            { timeoutMs: 30_000, intervalMs: 500, label: "the desktop's key in the log" },
        );
        const joined = await desk.client.identity.status();
        expect(joined.did).to.equal(did);
        expect(joined.keyId).to.equal("d2");
        // The record lists the desktop, read through the holder's executor.
        const opened = await holder.send({ cmd: "record", did });
        expect(opened.version).to.equal(1);
        expect(opened.record.devices.d2).to.include({ name: "Desk", kind: "desktop-app", holdsMaster: false });
    });

    it("hands the master key sealed to the desktop to its custody service only, once", async () => {
        const announced: any[] = [];
        desk.client.on("inbox-message", (event) => announced.push(event));
        const sealed = await holder.send({ cmd: "sendMaster", did, keyId: "d2" });
        expect(sealed.record).to.equal(2);
        expect((await holder.send({ cmd: "record", did })).record.devices.d2.holdsMaster).to.equal("offered");

        const listed = await desk.client.identity.inbox();
        expect(listed.map((m) => [m.id, m.kind])).to.deep.equal([[sealed.id, "master"]]);
        expect(JSON.stringify(listed)).to.not.contain(sealed.secret);
        await pollUntil(() => announced.length > 0, { timeoutMs: 10_000, intervalMs: 200, label: "inbox-message event" });
        expect(announced[0]).to.include({ id: sealed.id, kind: "master" });
        expect(JSON.stringify(announced)).to.not.contain(sealed.secret);

        // A caller without the admin credential is not the custody service.
        const stranger = new Ad4mClient(baseUrl(desk.ports[0]));
        expect(await refused(stranger.identity.takeMaster())).to.match(/Admin credential required|403/);

        const taken = await desk.client.identity.takeMaster();
        expect(taken).to.deep.equal({ messageId: sealed.id, did, index: 0, secret: sealed.secret });
        // It waits until the custody service acknowledges it.
        expect((await desk.client.identity.inbox()).map((m) => m.id)).to.deep.equal([sealed.id]);
        expect(await desk.client.identity.inboxAck(sealed.id)).to.be.true;
        expect(await desk.client.identity.inbox()).to.deep.equal([]);
        expect(await desk.client.identity.takeMaster()).to.be.null;
    });

    it("the desktop's custody service takes the master key and the record shows it holds it", async () => {
        // The custody service: the client library on the desktop's executor, with the admin
        // credential over the loopback listener.
        custody = new Holder();
        expect(await custody.send({ cmd: "connect", url: baseUrl(desk.ports[0]), token: CREDENTIAL }))
            .to.deep.equal({ ok: true });
        expect(await custody.send({ cmd: "takeMaster" })).to.be.null;

        const sent = await holder.send({ cmd: "sendMaster", did, keyId: "d2" });
        const taken = await custody.send({ cmd: "takeMaster" });
        expect(taken).to.deep.equal({ did, index: 0, record: sent.record + 1 });
        expect(await desk.client.identity.inbox()).to.deep.equal([]);
        // Both devices read the same record: the desktop holds the master key.
        for (const reader of [holder, custody]) {
            const opened = await reader.send({ cmd: "record", did });
            expect(opened.version).to.equal(sent.record + 1);
            expect(opened.record.devices.d2).to.include({ name: "Desk", holdsMaster: true });
        }
    });

    it("answers the desktop's request for the master key from the holder", async () => {
        const posted = await custody.send({ cmd: "postRequest", body: { kind: "send-master" } });
        const listed = await holder.send({ cmd: "requests" });
        expect(listed.map((r: any) => [r.id, r.by, r.body.kind])).to.deep.equal([[posted.id, "d2", "send-master"]]);

        const answered = await holder.send({ cmd: "answer", did, id: posted.id });
        expect(answered.answered).to.equal("send-master");
        expect(await holder.send({ cmd: "requests" })).to.deep.equal([]);
        expect((await holder.send({ cmd: "record", did })).record.devices.d2.holdsMaster).to.equal("offered");
        expect((await desk.client.identity.inbox()).map((m) => [m.id, m.kind])).to.deep.equal([[answered.id, "master"]]);
        const taken = await custody.send({ cmd: "takeMaster" });
        expect(taken).to.include({ did, index: 0 });
        expect((await holder.send({ cmd: "record", did })).record.devices.d2.holdsMaster).to.equal(true);
    });

    it("deletes a master key on an executor without the custody channel and never hands it out", async () => {
        // The master key goes only to a device the record lists as an app: a server is refused.
        expect(await refused(holder.send({ cmd: "sendMaster", did, keyId: "d1" }))).to.contain("may not keep");
        await holder.send({ cmd: "recordDevice", did, keyId: "d1", name: "Home", kind: "desktop-app" });
        await holder.send({ cmd: "sendMaster", did, keyId: "d1" });
        expect(await home.client.identity.inbox()).to.deep.equal([]);
        expect(await refused(home.client.identity.takeMaster())).to.contain("no-custody-channel");
        // A claim saying it keeps the master key is refused there too.
        expect(await refused(home.client.identity.claimOffer("coasys:offer:1:x", undefined, true)))
            .to.contain("no-custody-channel");
    });
});
