/**
 * An executor as a host (AR-05): the sign-up setting, invites, join, use, leave keeping the
 * data, rejoin to the kept data, and leave deleting it. Executors with no Holochain, a real
 * identity-store this suite starts on a temp file, and the centralized agent language.
 *
 * Node A is the person's own executor: the identity-client holder creates the identity on it
 * and makes the offer the host claims. The host H is a multi-user executor with an admin
 * credential. A tenant of H is reached by a key login that A's device key signs, as an app on
 * the person's device would.
 *
 * Needs `pnpm run prepare-test-local` first (the centralized agent language).
 */
import { expect } from "chai";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { ChildProcess } from "child_process";
import { Ad4mClient } from "@coasys/ad4m";
import { baseUrl, pollUntil, quitExecutor, startExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { buildIdentityTools, Holder, IdentityStore, VouchService } from "../helpers/identity";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const TEST_DIR = path.join(__dirname, "..", "tst-tmp");
const SEED_PATH = path.join(TEST_DIR, "hostTenantsBootstrapSeed.json");
const ADMIN_CREDENTIAL = "host-tenants-admin";

interface Node {
    dataPath: string;
    ports: number[];
    process: ChildProcess | null;
    client: Ad4mClient;
}

/** The local test seed with the centralized agent language as the agent language. */
function writeSeed() {
    const seed = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "bootstrapSeed.json"), "utf8"));
    const agentLanguage = fs
        .readFileSync(path.join(__dirname, "..", "scripts", "centralized-agent-language-hash"), "utf8")
        .trim();
    if (!agentLanguage) throw new Error("run prepare-test-local first: no centralized agent language");
    seed.agentLanguage = agentLanguage;
    fs.writeFileSync(SEED_PATH, JSON.stringify(seed));
}

/** The message of the refusal `call` ends in, or null when it succeeds. */
async function refusal(call: () => Promise<unknown>): Promise<string | null> {
    return call().then(
        () => null,
        (e: Error) => e.message,
    );
}

describe("Host: sign-up, invites, join, leave keeping or deleting the data, rejoin", function () {
    this.timeout(600_000);

    let store: IdentityStore;
    let vouch: VouchService;
    let holder: Holder;
    let a: Node;
    let h: Node;
    let did = "";
    const nodes: Node[] = [];

    async function startNode(name: string, adminCredential?: string): Promise<Node> {
        const ports = await getFreePorts(3);
        registerPorts(ports);
        const dataPath = path.join(TEST_DIR, "agents", `host-tenants-${name}`);
        fs.mkdirSync(path.dirname(dataPath), { recursive: true });
        const process = await startExecutor(
            dataPath, SEED_PATH, ports[0], ports[1], ports[2],
            false, adminCredential, undefined, undefined, undefined, false, undefined, undefined,
            false,          // runHolochain
            true,           // sharedStores
            store.executorEnv(),
        );
        const client = new Ad4mClient(baseUrl(ports[0]), adminCredential);
        await client.agent.generate("passphrase");
        const node = { dataPath, ports, process, client };
        nodes.push(node);
        return node;
    }

    /** `signer`'s device key signs a login to H: a session of the tenant that speaks for the identity. */
    async function tenantSession(signer: Node): Promise<Ad4mClient> {
        const challenge = await h.client.agent.sessionChallenge();
        const proof = await signer.client.agent.signSessionStatement({
            v: 1,
            purpose: "login",
            executor: challenge.executor,
            audience: baseUrl(h.ports[0]),
            challenge: challenge.challenge,
            client: { kind: "native" },
            issuedAt: Math.floor(Date.now() / 1000),
        });
        const app = {
            appName: "host tenants test",
            appDesc: "a tenant's session",
            appDomain: "coasys.org",
            capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
        };
        const session = await h.client.agent.loginWithKey(proof, app);
        return new Ad4mClient(baseUrl(h.ports[0]), session.token);
    }

    /** One join: the holder makes an offer, H claims it for the tenant's key, the holder confirms. */
    async function join(inviteSecret?: string) {
        const opened = await holder.send({ cmd: "open", did, store: store.url });
        const joined = await h.client.host.join(did, opened.offerString, inviteSecret);
        const claim = await holder.send({ cmd: "waitClaim" });
        expect(claim.deviceKey).to.match(/^z6Mk/);
        await holder.send({ cmd: "confirm", name: "Coasys cloud", kind: "server", keepMaster: false });
        await pollUntil(async () => (await h.client.host.joinStatus(joined.joinHandle)).state === "active", {
            timeoutMs: 60_000, intervalMs: 500, label: "the join turns active",
        });
        return joined;
    }

    before(async () => {
        if (!fs.existsSync(TEST_DIR)) throw Error("run prepare-test-local before the host suite");
        writeSeed();
        buildIdentityTools();
        const [storePort] = await getFreePorts(1);
        registerPorts([storePort]);
        vouch = await VouchService.start();
        store = await IdentityStore.start(path.join(TEST_DIR, "identity-store-host"), storePort, { vouch });
        a = await startNode("a");
        holder = new Holder();
        await holder.send({ cmd: "connect", url: baseUrl(a.ports[0]), token: "" });
        did = (await holder.send({ cmd: "create" })).did;
        h = await startNode("h", ADMIN_CREDENTIAL);
        await h.client.runtime.setMultiUserEnabled(true);
    });

    after(async () => {
        await holder?.stop();
        for (const node of nodes) {
            if (node.process) await quitExecutor(node.process, node.ports[0]);
            deregisterPorts(node.ports);
        }
        await store?.stop();
        if (store) deregisterPorts([store.port]);
        await vouch?.stop();
    });

    it("answers `host.info` to anyone and refuses every join while sign-up is `No one`", async () => {
        const anyone = new Ad4mClient(baseUrl(h.ports[0]));
        const info = await anyone.host.info();
        expect(info.multiUser).to.equal(true);
        expect(info.signup).to.equal("no_one");
        const opened = await holder.send({ cmd: "open", did, store: store.url });
        expect(await refusal(() => anyone.host.join(did, opened.offerString))).to.match(/signups-closed/);
        // The refusal made no tenant.
        expect(await h.client.host.tenants()).to.have.length(0);
    });

    it("asks for an invite once the operator sets `by invite`, and an invite works once", async () => {
        const anyone = new Ad4mClient(baseUrl(h.ports[0]));
        expect(await refusal(() => anyone.host.tenants())).to.not.be.null; // an operator call
        const policy = await h.client.host.setPolicy({ signup: "by_invite" });
        expect(policy.signup).to.equal("by_invite");

        const opened = await holder.send({ cmd: "open", did, store: store.url });
        expect(await refusal(() => anyone.host.join(did, opened.offerString))).to.match(/invite-invalid/);
        expect(await refusal(() => anyone.host.join(did, opened.offerString, "not-an-invite"))).to.match(/invite-invalid/);
        expect(await h.client.host.tenants()).to.have.length(0);
    });

    let tenantId = "";
    let tenantDid = "";
    let perspectiveUuid = "";

    it("joins with an invite: the tenant becomes active, the invite is spent, usage reads", async () => {
        const invite = await h.client.host.createInvite(1, "test");
        expect(invite.secret).to.be.a("string");
        const joined = await join(invite.secret);
        expect(joined.rejoin).to.equal(false);
        tenantId = joined.tenantId;
        tenantDid = joined.deviceDid;

        const tenants = await h.client.host.tenants();
        expect(tenants).to.have.length(1);
        expect(tenants[0].state).to.equal("active");
        expect(tenants[0].didScid).to.equal(did);
        const invites = await h.client.host.listInvites();
        expect(invites[0].state).to.not.equal("open");

        // The invite is spent: a second join with it is refused.
        const again = await holder.send({ cmd: "open", did, store: store.url });
        const anyone = new Ad4mClient(baseUrl(h.ports[0]));
        expect(await refusal(() => anyone.host.join(did, again.offerString, invite.secret))).to.match(
            /invite-invalid|already-a-tenant/,
        );

        const session = await tenantSession(a);
        const usage = await session.host.usage();
        expect(usage.activatedAt).to.not.be.null;
    });

    it("lets the tenant use the host: a perspective only it owns", async () => {
        const session = await tenantSession(a);
        const created = await session.perspective.add("tenant space");
        perspectiveUuid = created.uuid;
        expect((await session.perspective.all()).map((p) => p.uuid)).to.include(perspectiveUuid);
        // The operator's main agent does not see the tenant's perspective as its own.
        expect((await a.client.perspective.all()).map((p) => p.uuid)).to.not.include(perspectiveUuid);
    });

    it("leave(keep): the tenant is detached, its sessions end, its device key is gone, the data stays", async () => {
        const session = await tenantSession(a);
        const left = await session.host.leave(true);
        expect(left.kept).to.equal(true);
        expect(left.keyDestroyedAt).to.be.null;

        const tenants = await h.client.host.tenants();
        expect(tenants.find((t) => t.tenantId === tenantId)?.state).to.equal("detached");
        // The session of the left tenant answers nothing more.
        expect(await refusal(() => session.perspective.all())).to.not.be.null;
        // The person's key log no longer lists the tenant's device key as a live one the host holds:
        // a login with A's key on H finds no tenant.
        expect(await refusal(() => tenantSession(a))).to.not.be.null;
        expect(tenantDid).to.match(/^did:key:/);
    });

    it("rejoin reattaches the kept data: the same perspective, a new device key", async () => {
        const invite = await h.client.host.createInvite(1, "rejoin");
        const joined = await join(invite.secret);
        expect(joined.rejoin).to.equal(true);
        expect(joined.tenantId).to.equal(tenantId);
        expect(joined.deviceDid).to.not.equal(tenantDid);

        const session = await tenantSession(a);
        expect((await session.perspective.all()).map((p) => p.uuid)).to.include(perspectiveUuid);
    });

    it("leave(delete): the data key is destroyed, the record is deleted, a new join starts empty", async () => {
        const session = await tenantSession(a);
        const left = await session.host.leave(false);
        expect(left.kept).to.equal(false);
        expect(left.keyDestroyedAt).to.not.be.null;
        expect(left.shreddedEverywhereAt).to.not.be.null;
        expect(await h.client.host.tenants()).to.have.length(0);

        const invite = await h.client.host.createInvite(1, "after delete");
        const joined = await join(invite.secret);
        expect(joined.rejoin).to.equal(false);
        const fresh = await tenantSession(a);
        expect((await fresh.perspective.all()).map((p) => p.uuid)).to.not.include(perspectiveUuid);
    });
});
