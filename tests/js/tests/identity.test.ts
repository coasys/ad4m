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
import WebSocket from "ws";
import { v4 as uuidv4 } from "uuid";
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
            // The origin the client dials: the target's API.
            audience: baseUrl(target.ports[0]),
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

    // A relay: a server the person added takes a challenge from A and has the person's executor
    // sign it. The app names the origin it dialed, the relay's, so A refuses the statement
    // whatever Host the relay's own connection to A sends. A runs without public_origins, so
    // only its own loopback origins are accepted.
    it("refuses a key statement signed for another origin, whatever Host the connection sends", async () => {
        const port = a.ports[0];
        const call = (host: string, type: string, params: unknown) =>
            new Promise<any>((resolve, reject) => {
                const ws = new WebSocket(`ws://127.0.0.1:${port}/api/v1/ws`, { headers: { Host: host } });
                ws.on("open", () => ws.send(JSON.stringify({ id: "relay", type, params })));
                ws.on("message", (data) => {
                    const reply = JSON.parse(String(data));
                    if (reply.id === "relay") {
                        ws.close();
                        resolve(reply);
                    }
                });
                ws.on("error", reject);
            });
        const signed = async (audience: string) => {
            const challenge = await a.client.agent.sessionChallenge();
            return b.client.agent.signSessionStatement({
                v: 1,
                purpose: "login",
                executor: challenge.executor,
                audience,
                challenge: challenge.challenge,
                client: { kind: "native" },
                issuedAt: Math.floor(Date.now() / 1000),
            });
        };
        const app = {
            appName: "relay",
            appDesc: "key login relay test",
            appDomain: "coasys.org",
            capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
        };
        const login = async (host: string, audience: string) =>
            call(host, "user.loginWithKey", { proof: await signed(audience), app });

        // The relay's origin, with the Host the relay forges to match it: refused, naming the setting.
        for (const [host, audience] of [
            ["e.example", "https://e.example"],
            ["e.example", "http://e.example"],
            [`127.0.0.1:${port}`, "https://e.example"],
        ]) {
            const refused = await login(host, audience);
            expect(refused.result, `${host} ${audience}`).to.be.undefined;
            expect(refused.error.code).to.equal(403);
            expect(refused.error.message).to.contain("no-public-origin");
            expect(refused.error.message).to.contain("public_origins");
        }
        // A loopback origin on another port is another executor: refused.
        const other = await login(`127.0.0.1:${port + 1}`, `http://127.0.0.1:${port + 1}`);
        expect(other.error.message).to.equal("audience");
        // A's own origin logs in, whatever the Host: the Host plays no part.
        for (const audience of [baseUrl(port), `http://localhost:${port}`, `ws://127.0.0.1:${port}`]) {
            const own = await login("e.example", audience);
            expect(own.error, audience).to.be.undefined;
            expect(own.result.role).to.equal("app");
        }
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
 * credential, the custody channel and its custody credential) shows its device string, the
 * holder scans it and pushes an offer ticket into its pre-join inbox, and the executor claims it;
 * then the holder seals the master key to the desktop's key, and only the custody service (the
 * custody credential, never the admin credential) takes it. The private record lists the desktop
 * and whether it holds the master key; the desktop asks for the master key through the request
 * box and the holder answers.
 */
describe("Identity: the device inbox, a ticket in the pre-join inbox and a master key for the custody service", function () {
    this.timeout(600_000);
    const CREDENTIAL = "desktop-admin-credential";
    const CUSTODY = "custody-service-credential";

    let store: IdentityStore;
    let holder: Holder;
    // The desktop's custody service, connected to the desktop's executor.
    let custody: Holder;
    const nodes: Node[] = [];
    let home: Node, desk: Node;
    let did = "";
    // The 12 words the holder made the identity with.
    let words = "";

    async function start(
        name: string,
        opts: {
            credential?: string;
            env?: Record<string, string>;
            initArgs?: string[];
            /** `--custody-channel`'s credential, written as the first line of stdin. */
            custodyCredential?: string;
        } = {},
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
            opts.custodyCredential,
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
        ({ did, words } = await holder.send({ cmd: "create" }));
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
            custodyCredential: CUSTODY,
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
        // Creating the identity wrote record version 1; the link writes version 2.
        expect(confirmed).to.deep.equal({ seq: 1, keyId: "d2", master: null, record: 2 });

        await pollUntil(
            async () => (await desk.client.identity.joinStatus(pushed.offerId)).state === "spent",
            { timeoutMs: 30_000, intervalMs: 500, label: "the desktop's key in the log" },
        );
        const joined = await desk.client.identity.status();
        expect(joined.did).to.equal(did);
        expect(joined.keyId).to.equal("d2");
        // The record lists the desktop, read through the holder's executor.
        const opened = await holder.send({ cmd: "record", did });
        expect(opened.version).to.equal(2);
        expect(opened.record.devices.d2).to.include({ name: "Desk", kind: "desktop-app", holdsMaster: false });
    });

    it("hands the master key sealed to the desktop to its custody service only, once", async () => {
        const announced: any[] = [];
        desk.client.on("inbox-message", (event) => announced.push(event));
        const sealed = await holder.send({ cmd: "sendMaster", did, keyId: "d2" });
        expect(sealed.record).to.equal(3);
        expect((await holder.send({ cmd: "record", did })).record.devices.d2.holdsMaster).to.equal("offered");

        const listed = await desk.client.identity.inbox();
        expect(listed.map((m) => [m.id, m.kind])).to.deep.equal([[sealed.id, "master"]]);
        expect(JSON.stringify(listed)).to.not.contain(sealed.secret);
        await pollUntil(() => announced.length > 0, { timeoutMs: 10_000, intervalMs: 200, label: "inbox-message event" });
        expect(announced[0]).to.include({ id: sealed.id, kind: "master" });
        expect(JSON.stringify(announced)).to.not.contain(sealed.secret);

        // A caller without a credential is not the custody service, and neither is the admin
        // credential (an app's window may hold it).
        const stranger = new Ad4mClient(baseUrl(desk.ports[0]));
        expect(await refused(stranger.identity.takeMaster())).to.match(/Admin credential required|403/);
        expect(await refused(desk.client.identity.takeMaster())).to.contain("no-custody-channel");

        const service = new Ad4mClient(baseUrl(desk.ports[0]), CUSTODY);
        const taken = await service.identity.takeMaster();
        expect(taken).to.deep.equal({ messageId: sealed.id, did, index: 0, secret: sealed.secret });
        // It waits until the custody service acknowledges it.
        expect((await desk.client.identity.inbox()).map((m) => m.id)).to.deep.equal([sealed.id]);
        expect(await desk.client.identity.inboxAck(sealed.id)).to.be.true;
        expect(await desk.client.identity.inbox()).to.deep.equal([]);
        expect(await service.identity.takeMaster()).to.be.null;
    });

    it("the desktop's custody service takes the master key and the record shows it holds it", async () => {
        // The custody service: the client library on the desktop's executor, with the custody
        // credential over the loopback listener (the admin credential never opens the channel).
        custody = new Holder();
        expect(await custody.send({ cmd: "connect", url: baseUrl(desk.ports[0]), token: CUSTODY }))
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
        // The log decides who may keep the master key, never the record's kind: d1 made the
        // identity (a standing key of the incept), so the holder sends it even though the record
        // calls it a server; a key the log does not list is refused.
        await holder.send({ cmd: "recordDevice", did, keyId: "d1", name: "Home", kind: "server" });
        expect(await refused(holder.send({ cmd: "sendMaster", did, keyId: "d9" }))).to.contain("may not keep");
        await holder.send({ cmd: "sendMaster", did, keyId: "d1" });
        expect(await home.client.identity.inbox()).to.deep.equal([]);
        expect(await refused(home.client.identity.takeMaster())).to.contain("no-custody-channel");
        // A claim saying it keeps the master key is refused there too.
        expect(await refused(home.client.identity.claimOffer("coasys:offer:1:x", undefined, true)))
            .to.contain("no-custody-channel");
    });

    it("rotates the master key with the words and the desktop's custody service takes the new one", async () => {
        const rotated = await holder.send({ cmd: "rotate", did });
        expect(rotated).to.include({ index: 1, tries: 1 });
        expect(rotated.revoked).to.deep.equal([]);
        expect(rotated.pushes.d2).to.be.a("string");
        expect((await holder.send({ cmd: "record", did })).record.devices.d1.holdsMaster).to.equal(true);

        // The custody service still keeps master #0: it takes #1 from its inbox.
        const refreshed = await custody.send({ cmd: "refreshMaster", did });
        expect(refreshed).to.include({ refreshed: "taken", index: 1 });
        expect(await custody.send({ cmd: "refreshMaster", did })).to.deep.equal({ refreshed: "current" });
        expect((await holder.send({ cmd: "record", did })).record.devices.d2.holdsMaster).to.equal(true);
    });

    it("removes the master key from one device, and refuses on the device left as its only holder", async () => {
        const removed = await holder.send({ cmd: "removeMaster", did });
        expect(removed.record).to.be.a("number");
        expect((await holder.send({ cmd: "record", did })).record.devices.d1.holdsMaster).to.equal(false);
        expect(await refused(custody.send({ cmd: "removeMaster", did }))).to.contain("only copy");
    });

    it("restores the identity from the 12 words on a new device, locking out the lost one", async () => {
        const laptop = await start("inbox-restored", { initArgs: ["--operator", did] });
        const restorer = new Holder();
        try {
            await restorer.send({ cmd: "connect", url: baseUrl(laptop.ports[0]), token: "" });
            // A wrong word is named by its position.
            const typo = words.split(" ").map((w: string, i: number) => (i === 4 ? "zebrra" : w)).join(" ");
            expect(await refused(restorer.send({ cmd: "restore", words: typo }))).to.contain("word 5 (\"zebrra\")");

            const restored = await restorer.send({ cmd: "restore", words, name: "Laptop", kind: "desktop-app", lockOut: ["d1"] });
            expect(restored.did).to.equal(did);
            expect(restored.devices.map((d: any) => d.keyId)).to.deep.equal(["d1", "d2"]);
            expect(restored).to.include({ index: 2, keyId: "d3" });
            expect(restored.revoked).to.deep.equal(["d1"]);
            expect(restored.pushes.d2).to.be.a("string");

            const status = await laptop.client.identity.status();
            expect(status.did).to.equal(did);
            expect(status.keyId).to.equal("d3");
            const devices = await laptop.client.identity.devices();
            expect(devices.find((d) => d.keyId === "d1")!.revokedSeq).to.equal(restored.seq);
            const opened = await restorer.send({ cmd: "record", did });
            expect(opened.record.devices.d3).to.include({ name: "Laptop", kind: "desktop-app", holdsMaster: true });
            expect(opened.record.devices.d1).to.be.undefined;

            // The kept desktop takes master #2.
            expect(await custody.send({ cmd: "refreshMaster", did })).to.include({ refreshed: "taken", index: 2 });
        } finally {
            await restorer.stop();
        }
    });
});

describe("Identity: a bot acts for its owner in the spaces of its grant, gives its reasons, and stops when revoked", function () {
    this.timeout(600_000);
    const CREDENTIAL = "bots-admin-credential";
    const CUSTODY = "bots-custody-credential";
    const DIFF_SYNC = () => fs.readFileSync(path.join(__dirname, "..", "scripts", "perspective-diff-sync-hash"), "utf8").trim();

    let store: IdentityStore;
    let holder: Holder;
    const nodes: Node[] = [];
    let home: Node, desk: Node;
    let did = "";
    let botDid = "";
    let bot: Ad4mClient;
    // Three neighbourhoods the owner made on the desktop; the grant names the first two.
    const spaces: { uuid: string; url: string }[] = [];
    // The perspective the bot acts in: the first neighbourhood, as the bot joined it.
    let botSpace = "";

    async function start(
        name: string,
        opts: { credential?: string; env?: Record<string, string>; custodyCredential?: string } = {},
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
            [],
            opts.custodyCredential,
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

    const note = (n: number) => ({ source: "ad4m://self", predicate: "test://note", target: `literal://string:note-${n}` });

    before(async () => {
        if (!fs.existsSync(TEST_DIR)) throw Error("run prepare-test-local before the identity suite");
        writeIdentitySeed();
        buildIdentityTools();
        const [storePort] = await getFreePorts(1);
        registerPorts([storePort]);
        store = await IdentityStore.start(path.join(TEST_DIR, "identity-store-bots"), storePort);
        home = await start("bots-home");
        holder = new Holder();
        await holder.send({ cmd: "connect", url: baseUrl(home.ports[0]), token: "" });
        ({ did } = await holder.send({ cmd: "create" }));
        // The bots' executor is a desktop with the custody channel: one person, no multi-user mode.
        desk = await start("bots-desktop", {
            credential: CREDENTIAL,
            env: { AD4M_CUSTODY_CHANNEL: "true" },
            custodyCredential: CUSTODY,
        });
    });

    after(async () => {
        bot?.close();
        await holder?.stop();
        for (const node of nodes) {
            if (node.process) await quitExecutor(node.process, node.ports[0]);
            deregisterPorts(node.ports);
        }
        await store?.stop();
        if (store) deregisterPorts([store.port]);
    });

    it("makes a bot agent on a desktop with the custody channel, and the owner's log lists its key with the bot mark", async () => {
        // A person stays the desktop's only person.
        const person = await desk.client.agent.createUser("person@example.org", "password");
        expect(person.success).to.be.false;

        const expires = Math.floor(Date.now() / 1000) + 3600;
        const opened = await holder.send({ cmd: "openBot", did, store: store.url, can: ["read", "write", "join"], expires });
        const created = await desk.client.bot.createAgent(opened.offerString, "Gardener");
        botDid = created.did;
        expect(created.did).to.equal(`did:key:${created.key}`);
        expect(created.owner).to.equal(did);
        expect(created.offerId).to.equal(opened.offerId);
        expect(created.token).to.be.a("string").and.not.empty;

        const claimed = await holder.send({ cmd: "waitClaim" });
        expect(claimed.matchCode).to.equal(created.matchCode);
        expect(claimed.deviceKey).to.equal(created.key);
        expect(claimed.hint).to.equal("Gardener");
        expect(claimed.canKeepMaster).to.be.false;
        const confirmed = await holder.send({ cmd: "confirm" });
        expect(confirmed.keyId).to.equal("d2");

        const view = await desk.client.bot.grant(botDid);
        expect(view).to.include({ did: botDid, name: "Gardener", owner: did });
        expect(view.grant).to.include({ state: "active", version: 1 });
        expect(view.grant.spaces).to.deep.equal([]);
        expect(view.key).to.deep.equal({ keyId: "d2", verdict: "valid", can: ["read", "write", "join"], expires });
        expect((await desk.client.bot.list()).map((b) => b.did)).to.deep.equal([botDid]);

        bot = new Ad4mClient(baseUrl(desk.ports[0]), created.token);
        // Its own session answers before any grant.
        expect((await bot.agent.status()).did).to.equal(botDid);
    });

    it("grants two of three spaces: the bot joins and writes there with its reasons, and the third answers bot-grant", async () => {
        for (const n of [1, 2, 3]) {
            const perspective = await desk.client.perspective.add(`bots space ${n}`);
            const language = await desk.client.languages.applyTemplateAndPublish(
                DIFF_SYNC(), JSON.stringify({ uid: uuidv4(), name: `bots space ${n}` }),
            );
            const url = await desk.client.neighbourhood.publishFromPerspective(perspective.uuid, language.address, new Perspective());
            spaces.push({ uuid: perspective.uuid, url });
        }
        const granted = await desk.client.bot.setGrant(botDid, {
            spaces: [spaces[0].url, spaces[1].url], can: ["read", "write", "join"], state: "active", version: 1,
        });
        expect(granted.grant).to.include({ state: "active", version: 2 });

        // A write and a join give a reason.
        expect(await refused(bot.neighbourhood.joinFromUrl(spaces[0].url))).to.contain("reason-required");
        bot.setReason("tend the garden board");
        const joined = await bot.neighbourhood.joinFromUrl(spaces[0].url);
        botSpace = joined.uuid;
        await bot.neighbourhood.joinFromUrl(spaces[1].url);
        await bot.perspective.addLink(botSpace, note(1));
        bot.setReason("log the first sowing");
        await bot.perspective.addLink(botSpace, note(2));
        const links = await bot.perspective.queryLinks(botSpace, { predicate: "test://note" } as any);
        expect(links.map((l: any) => l.data.target).sort()).to.deep.equal(["literal://string:note-1", "literal://string:note-2"]);

        const third = await refused(bot.neighbourhood.joinFromUrl(spaces[2].url));
        expect(third).to.contain("bot-grant");
        // The owner's work outside any space is no bot's either.
        expect(await refused(bot.perspective.add("bot's own"))).to.contain("bot-grant");
    });

    it("pauses the bot: its calls in its spaces answer bot-paused, then work again on resume", async () => {
        await desk.client.bot.pause(botDid);
        expect(await refused(bot.perspective.addLink(botSpace, note(3)))).to.contain("bot-paused");
        expect(await refused(bot.perspective.queryLinks(botSpace, {} as any))).to.contain("bot-paused");
        const resumed = await desk.client.bot.resume(botDid);
        expect(resumed.grant).to.include({ state: "active", version: 4 });
        await bot.perspective.addLink(botSpace, note(3));
    });

    it("narrows the grant to reading: a write answers not-in-can and a read still answers", async () => {
        const view = await desk.client.bot.grant(botDid);
        await desk.client.bot.setGrant(botDid, { ...view.grant, can: ["read"] });
        expect(await refused(bot.perspective.addLink(botSpace, note(4)))).to.contain("not-in-can");
        const links = await bot.perspective.queryLinks(botSpace, { predicate: "test://note" } as any);
        expect(links).to.have.length(3);
    });

    it("revokes the bot: its calls are refused, its grant stays revoked, and its log keeps every entry", async () => {
        const revoked = await desk.client.bot.revoke(botDid);
        expect(revoked.grant.state).to.equal("revoked");
        const after = await refused(bot.perspective.queryLinks(botSpace, {} as any));
        expect(after).to.match(/bot-revoked|revoked|Unauthorized/);
        // A revoked grant is for good.
        const again = await refused(desk.client.bot.resume(botDid));
        expect(again).to.contain("bot-revoked");

        const log = await desk.client.bot.log(botDid);
        const allowed = log.filter((e) => e.outcome === "allowed");
        expect(allowed.map((e) => [e.method, e.reason])).to.deep.include.members([
            ["neighbourhood.join", "tend the garden board"],
            ["perspective.addLink", "log the first sowing"],
            ["perspective.addLink", "tend the garden board"],
        ]);
        expect(allowed.every((e) => e.action === "write" || e.action === "join")).to.be.true;
        const outcomes = log.map((e) => e.outcome);
        for (const code of ["reason-required", "bot-grant", "bot-paused", "not-in-can"]) {
            expect(outcomes, code).to.include(code);
        }
        // Newest first.
        expect(log.map((e) => e.at)).to.deep.equal([...log.map((e) => e.at)].sort((x, y) => y - x));
        expect((await desk.client.bot.log(botDid, { limit: 2 }))).to.have.length(2);
    });
});
