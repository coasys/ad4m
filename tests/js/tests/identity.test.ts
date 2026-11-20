/**
 * Identity end to end (create, link, the person's profile, the first-link refusal, revoke and
 * the links of the revoked key on a reader, DID resolution on an executor that never saw the
 * log, fork, restart): executors
 * with no Holochain, a real identity-store this suite starts on a temp file, and the
 * centralized agent language seeded as every executor's agent language.
 *
 * The holder is ad4m-identity-client's `holder` example: the client's create, link and revoke
 * flows, with master keys in a MemoryVault, against executor A over its RPC. Executor B joins
 * with `ad4m-executor init --operator` and `ad4m-executor claim <link>`.
 *
 * The revocation test shares a neighbourhood between B, A and a reader on the server link
 * language, through a link server this suite starts (`link-server/dist`).
 *
 * Needs `pnpm run prepare-test-local` first (it publishes the centralized agent language and
 * writes its address to scripts/centralized-agent-language-hash).
 */
import { expect } from "chai";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { ChildProcess, execSync } from "child_process";
import {
    Ad4mClient, Ad4mModel, ExpressionProof, Flag, Link, LinkExpression, LinkQuery, Model, Perspective,
    PerspectiveProxy, PerspectiveState,
} from "@coasys/ad4m";
import {
    baseUrl, executorBinary, executorDataPath, pollUntil, quitExecutor, spawnExecutor, startExecutor,
} from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { buildIdentityTools, ClaimCommand, Holder, IdentityStore } from "../helpers/identity";
import { LinkServerHandle, startLinkServer } from "../utils/linkServer";
import { publishLinkLanguage, serverLinkLang } from "../utils/linkLangConfig";

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

/**
 * A note: a subject with the flag `rev://type` → `rev://note`. A model read is a logic read, so
 * it counts only the links whose verdict is `valid`.
 */
@Model({ name: "RevocationNote" })
class RevocationNote extends Ad4mModel {
    @Flag({ through: "rev://type", value: "rev://note" })
    type = "rev://note";
}

/** The server link language that prepare-test-local published. */
function serverLinkLanguageHash(): string {
    const file = path.join(__dirname, "..", "scripts", "server-link-language-hash");
    const hash = fs.existsSync(file) ? fs.readFileSync(file, "utf8").trim() : "";
    if (!hash) throw new Error("run prepare-test-local first: no server link language");
    return hash;
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
    let linkServer: LinkServerHandle | null = null;
    let revoking: Promise<any> | null = null;
    // The revocation neighbourhood, for the late-syncer test after it.
    let nhUrl = "";
    let nhOnB: PerspectiveProxy | null = null;

    /**
     * Revokes B's key (d2) one time, through the holder. Before the revoke, it keeps a copy of
     * the store's log (incept, add) for the fork test. Each later call gets the same answer.
     */
    function revokeD2(): Promise<any> {
        revoking ??= (async () => {
            await store.stop();
            store.saveDb(storeCopy);
            await store.restart();
            return holder.send({ cmd: "revoke", did, keyIds: ["d2"] });
        })();
        return revoking;
    }

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
        await linkServer?.kill();
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

    it("keeps a link that a device key signed before its revocation, and does not count a link that the key signs after it", async () => {
        // B (d2) makes a neighbourhood on the server link language. A (d1) and a reader with
        // no identity join it. The links go through a link server.
        linkServer = await startLinkServer();
        const linkLanguage = serverLinkLang(serverLinkLanguageHash(), linkServer.url);
        const reader = await startNode("reader");
        const onB = await b.client.perspective.add("revocation");
        const language = await publishLinkLanguage(b.client, linkLanguage, "Identity revocation");
        const url = await b.client.neighbourhood.publishFromPerspective(onB.uuid, language.address, new Perspective());
        nhUrl = url;
        nhOnB = onB;
        const onA = await a.client.neighbourhood.joinFromUrl(url);
        const onReader = await reader.client.neighbourhood.joinFromUrl(url);
        for (const [client, uuid] of [[b.client, onB.uuid], [a.client, onA.uuid], [reader.client, onReader.uuid]] as const) {
            await pollUntil(async () => {
                const state = (await client.perspective.byUUID(uuid))?.state;
                return state === PerspectiveState.LinkLanguageInstalledButNotSynced || state === PerspectiveState.Synced;
            }, { timeoutMs: 60_000, intervalMs: 500, label: `link language attached for ${uuid}` });
        }
        const readerProxy = (await reader.client.perspective.byUUID(onReader.uuid))!;
        await RevocationNote.register(readerProxy);

        const note = (id: string) => ({ source: `rev://${id}`, predicate: "rev://type", target: "rev://note" });
        /** The reader's copy of the note link `id`, whatever its verdict. */
        const onReaderLink = async (id: string): Promise<LinkExpression | undefined> => {
            const links = await reader.client.perspective.queryLinks(
                onReader.uuid,
                new LinkQuery({ source: `rev://${id}`, predicate: "rev://type", includeUnverified: true }),
            );
            return links[0];
        };
        /** Waits until the reader has the note link `id` with a verdict other than cantCheck. */
        const judgedOnReader = async (id: string): Promise<LinkExpression> => {
            let link: LinkExpression | undefined;
            await pollUntil(async () => {
                link = await onReaderLink(id);
                return !!link?.verdict && link.verdict.kind !== "cantCheck";
            }, { timeoutMs: 90_000, intervalMs: 500, label: `the reader judges rev://${id}` });
            return link!;
        };
        /** The notes that a logic read on the reader counts. */
        const counted = async () => (await RevocationNote.findAll(readerProxy)).map((n) => n.id).sort();
        const shown = async () =>
            (await RevocationNote.findAll(readerProxy, { includeUnverified: true })).map((n) => n.id).sort();

        // 1. d2 signs L1 before the revocation. The reader counts it, for the person.
        await b.client.perspective.addLink(onB.uuid, note("l1"));
        let l1 = await judgedOnReader("l1");
        expect(l1.verdict).to.deep.equal({ kind: "valid" });
        expect(l1.proof.anchor).to.equal(1);
        expect(l1.person).to.equal(did);
        expect(await counted()).to.deep.equal(["rev://l1"]);
        // d2 also signs a link that stays on B for now (local).
        const lateLink = await b.client.perspective.addLink(onB.uuid, note("late"), "local");
        expect(lateLink.proof.anchor).to.equal(1);

        // 2. A revokes d2.
        const revoked = await revokeD2();
        expect(revoked.seq).to.equal(2);

        // 3. The revocation gets to the reader's key map. L1 entered before it: it stays valid
        // and counted.
        await pollUntil(
            async () => (await reader.client.identity.log(did, 0, true)).head?.seq === 2,
            { timeoutMs: 30_000, intervalMs: 500, label: "the reader has the revoke" },
        );
        l1 = (await onReaderLink("l1"))!;
        expect(l1.verdict).to.deep.equal({ kind: "valid" });
        expect(await counted()).to.deep.equal(["rev://l1"]);

        // 4. A link that d2 signed before the revocation, but that gets to the reader only after
        // it (a copy of the key can do this too): the reader does not know that it entered
        // before the revoke, so it is flagged. A display read shows it; a logic read does not
        // count it.
        const late = await reader.client.perspective.addLinkExpression(onReader.uuid, lateLink, "local");
        expect(late.proof.anchor).to.equal(1);
        expect(late.verdict).to.deep.equal({ kind: "flagged" });
        const lateShown = await reader.client.perspective.queryLinks(onReader.uuid, new LinkQuery({ source: "rev://late" }));
        expect(lateShown.map((l) => l.verdict)).to.deep.equal([{ kind: "flagged" }]);
        expect(await counted()).to.deep.equal(["rev://l1"]);

        // 5. B reads its log again (identity.status does), so the revocation gets to B's key
        // map and B anchors its next link at the revoke. agent.status reads no store.
        await pollUntil(
            async () => (await b.client.identity.status()).revoked === 2
                && (await b.client.agent.status()).identity === null,
            { timeoutMs: 30_000, intervalMs: 500, label: "B's key no longer speaks for the person" },
        );
        // B still holds the revoked key and signs L2 with it. The anchor is at the revoke, so L2
        // is invalid (revoked) on B.
        const l2OnB = await b.client.perspective.addLink(onB.uuid, note("l2"), "local");
        expect(l2OnB.proof.anchor).to.equal(2);
        expect(l2OnB.verdict).to.deep.equal({ kind: "invalid", reason: "revoked" });
        // The test gives L2, as B signed it, to the reader directly. (Through the link language,
        // the reader does not store a remote link that is invalid, so no read shows it.) The
        // reader also finds L2 invalid (revoked): a display read does not show it, and a logic
        // read does not count it.
        const l2 = await reader.client.perspective.addLinkExpression(onReader.uuid, l2OnB, "local");
        expect(l2.proof.anchor).to.equal(2);
        expect(l2.verdict).to.deep.equal({ kind: "invalid", reason: "revoked" });
        expect(l2.proof.valid).to.equal(false);
        expect(l2.person ?? null).to.be.null;
        const l2Shown = await reader.client.perspective.queryLinks(onReader.uuid, new LinkQuery({ source: "rev://l2" }));
        expect(l2Shown).to.have.length(0);
        expect(await counted()).to.deep.equal(["rev://l1"]);

        // 6. A's device d1 is not revoked: its link L3, signed after the revocation, counts.
        await a.client.perspective.addLink(onA.uuid, note("l3"));
        const l3 = await judgedOnReader("l3");
        expect(l3.verdict).to.deep.equal({ kind: "valid" });
        expect(l3.proof.anchor).to.equal(2);
        expect(l3.person).to.equal(did);

        // 7. At the end, the logic read counts L1 and L3, not the late link or L2. L1 is still
        // valid. With includeUnverified, the late link and L2 are there too.
        expect((await onReaderLink("l1"))!.verdict).to.deep.equal({ kind: "valid" });
        expect(await counted()).to.deep.equal(["rev://l1", "rev://l3"]);
        expect(await shown()).to.deep.equal(["rev://l1", "rev://l2", "rev://l3", "rev://late"]);
    });

    it("counts a pre-revocation link for a reader that syncs after the revocation (the notice orders it)", async () => {
        // A reader that joins the neighbourhood only now — after the store's head carries
        // the revoke — syncs the whole history late. Arrival order alone would leave L1
        // flagged (its key is revoked past its anchor, and this store never counted it
        // before the revoke). A's executor wrote a revocation notice for L1 when the
        // revoke reached its key map — a still-valid device of the same identity can only
        // sign it after the revoke — and the shared notice orders L1 before the revoke for
        // every reader. Force A's refresh first so the notice is written and committed.
        await a.client.identity.log(did, 0, true);

        const lateReader = await startNode("late-reader");
        const onLate = await lateReader.client.neighbourhood.joinFromUrl(nhUrl);
        await pollUntil(async () => {
            const state = (await lateReader.client.perspective.byUUID(onLate.uuid))?.state;
            return state === PerspectiveState.LinkLanguageInstalledButNotSynced || state === PerspectiveState.Synced;
        }, { timeoutMs: 60_000, intervalMs: 500, label: "the late reader's link language attached" });

        const onLateLink = async (id: string): Promise<LinkExpression | undefined> => {
            const links = await lateReader.client.perspective.queryLinks(
                onLate.uuid,
                new LinkQuery({ source: `rev://${id}`, predicate: "rev://type", includeUnverified: true }),
            );
            return links[0];
        };

        // L1 counts on the late reader, as on the early one. If it arrived before the
        // notice, the flagged re-check lifts it on the next tick (60 s), so wait for valid.
        let l1: LinkExpression | undefined;
        await pollUntil(async () => {
            l1 = await onLateLink("l1");
            return l1?.verdict?.kind === "valid";
        }, { timeoutMs: 150_000, intervalMs: 1_000, label: "l1 valid on the late reader" });
        expect(l1!.proof.anchor).to.equal(1);
        expect(l1!.person).to.equal(did);

        // The notice itself is there: A's still-valid device signed (reifier(l1),
        // ad4m://entered-before-revoke, did) anchored at or past the revoke.
        const notices = await lateReader.client.perspective.queryLinks(
            onLate.uuid,
            new LinkQuery({ predicate: "ad4m://entered-before-revoke" }),
        );
        expect(notices).to.have.length(1);
        expect(notices[0].verdict).to.deep.equal({ kind: "valid" });
        expect(notices[0].data.target).to.equal(did);
        expect(notices[0].person).to.equal(did);
        expect(notices[0].proof.anchor).to.be.gte(2);

        // And nothing changed for post-revocation links: a new link of the revoked key is
        // invalid on the late reader, as it was on the early one.
        const l4OnB = await b.client.perspective.addLink(
            nhOnB!.uuid,
            { source: "rev://l4", predicate: "rev://type", target: "rev://note" },
            "local",
        );
        expect(l4OnB.verdict).to.deep.equal({ kind: "invalid", reason: "revoked" });
        const l4 = await lateReader.client.perspective.addLinkExpression(onLate.uuid, l4OnB, "local");
        expect(l4.verdict).to.deep.equal({ kind: "invalid", reason: "revoked" });
        const l4Shown = await lateReader.client.perspective.queryLinks(
            onLate.uuid,
            new LinkQuery({ source: "rev://l4" }),
        );
        expect(l4Shown).to.have.length(0);
    });

    it("revokes B's key: B reports it revoked", async () => {
        // The test before revoked it (revokeD2 revokes one time).
        const revoked = await revokeD2();
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
