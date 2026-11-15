/**
 * Spaces end to end on a protocol 2 link server, in multi-user mode, with no AUTO_ADMIT.
 *
 * The world:
 * - a link server (utils/linkServer.ts) with its own identity store, which refreshes the keys it
 *   admitted every 2 s, so a revoked key loses its socket (4005) within seconds;
 * - executor A (multi-user, `--enable-multi-user true`): the managed user Alice, with a person;
 * - executor B (multi-user): the managed users Bob and Carol, each with a person, and Dave, who
 *   has none (a device-only key);
 * - executor A2: Alice's second device, linked to her person with `ad4m-executor claim`;
 * - a desktop with the custody channel, where Alice's bot runs (bots need one; it is not a
 *   multi-user executor).
 * Every executor runs against the link server's identity store with the centralized agent
 * language (helpers/identity.ts `writePersonSeed`). Alice makes every space with
 * `neighbourhood.create`; the others join by its link, as an app's users do.
 *
 * CI job: integration-tests-spaces (pnpm run test-main-spaces).
 */
import { expect } from "chai";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";
import { ChildProcess } from "child_process";
import { Ad4mClient, LinkExpression } from "@coasys/ad4m";
import { startExecutor, baseUrl, quitExecutor, pollUntil, sleep } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { startLinkServer, LinkServerHandle } from "../utils/linkServer";
import { givePerson, linkDevice, Holder, Person, writePersonSeed } from "../helpers/identity";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const TEST_DIR = path.join(__dirname, "..", "tst-tmp");

const ADMIN = "spaces-admin-credential";
const DESK_ADMIN = "spaces-desk-credential";
const CUSTODY = "spaces-custody-credential";
const MULTI_USER = ["--enable-multi-user", "true"];

const RULES = "coasys://membership/rules";
const NOTE = "test://note";
const VOTE = "test://vote";
const POLL = "test://poll";

interface Node {
    name: string;
    ports: number[];
    process: ChildProcess;
    /** The node's admin (its operator on a single-user node). */
    admin: Ad4mClient;
}

/** A managed user of a multi-user node. */
interface User {
    email: string;
    token: string;
    client: Ad4mClient;
}

describe("Spaces on a protocol 2 link server (multi-user, no AUTO_ADMIT)", function () {
    this.timeout(900_000);

    let linkServer: LinkServerHandle;
    let seedPath = "";
    const nodes: Node[] = [];
    const clients: Ad4mClient[] = [];
    const holders: Holder[] = [];
    let a: Node, b: Node;
    let alice: User, bob: User, carol: User, dave: User;
    let alicePerson: Person, bobPerson: Person, carolPerson: Person;

    // Test 1's space: open, then invite-only (test 4).
    let open = { uuid: "", url: "" };
    // The space of tests 2, 3, 6 and 7: Alice, her second device and her bot.
    let home = { uuid: "", url: "" };
    let a2: Node | null = null;
    let a2Key = "";
    let a2KeyId = "";
    let a2Space = "";

    async function start(
        name: string,
        opts: {
            credential?: string;
            env?: Record<string, string>;
            initArgs?: string[];
            runArgs?: string[];
            custodyCredential?: string;
        } = {},
    ): Promise<Node> {
        const ports = await getFreePorts(3);
        registerPorts(ports);
        const dataPath = path.join(TEST_DIR, "agents", `spaces-${name}`);
        fs.mkdirSync(path.dirname(dataPath), { recursive: true });
        const process = await startExecutor(
            dataPath, seedPath, ports[0], ports[1], ports[2],
            false, opts.credential, undefined, undefined, undefined, false, undefined, undefined,
            false,          // runHolochain
            true,           // sharedStores
            { ...linkServer.store.executorEnv(), ...(opts.env ?? {}) },
            opts.initArgs ?? [],
            opts.custodyCredential,
            opts.runArgs ?? [],
        );
        const admin = new Ad4mClient(baseUrl(ports[0]), opts.credential);
        clients.push(admin);
        await admin.agent.generate("passphrase");
        const node = { name, ports, process, admin };
        nodes.push(node);
        return node;
    }

    async function user(node: Node, name: string): Promise<User> {
        const email = `${name}@spaces.test`;
        const created = await node.admin.agent.createUser(email, "password");
        if (!created.success) throw new Error(`createUser ${email}: ${JSON.stringify(created)}`);
        await node.admin.runtime.setUserFreeAccess(email, true);
        const token = await node.admin.agent.loginUser(email, "password");
        const client = new Ad4mClient(baseUrl(node.ports[0]), token);
        clients.push(client);
        await client.agent.me();
        return { email, token, client };
    }

    async function person(node: Node, u: User): Promise<Person> {
        const p = await givePerson(node.ports[0], u.token);
        holders.push(p.holder);
        const me = await u.client.agent.me();
        expect(me.identity, `${u.email} speaks for the person the holder made`).to.equal(p.did);
        return p;
    }

    /** Makes a space as `u`'s person (`add: admins`, `join: open`) with one note link. */
    async function space(u: User, name: string, note: string) {
        const perspective = await u.client.perspective.add(name);
        const made = await u.client.neighbourhood.create({
            perspectiveUuid: perspective.uuid,
            sync: { kind: "ownLinkServer", url: linkServer.url },
            rules: { add: "admins", join: "open" },
        });
        await u.client.perspective.addLink(perspective.uuid, { source: "test://space", predicate: NOTE, target: literal(note) });
        return { uuid: perspective.uuid, url: made.url };
    }

    async function refused(call: Promise<unknown>): Promise<string> {
        try {
            await call;
        } catch (e) {
            return e instanceof Error ? e.message : String(e);
        }
        throw new Error("the call was not refused");
    }

    /** The `neighbourhood-join-refused` events `client`'s agent gets, from now on. */
    async function joinRefusals(client: Ad4mClient) {
        const seen: { url: string; reason: string }[] = [];
        const off = client.on("neighbourhood-join-refused", (event) => seen.push(event));
        // The socket gets no event until its `events.watch` naming it is answered.
        await sleep(1000);
        return { seen, off };
    }

    async function notes(client: Ad4mClient, uuid: string): Promise<string[]> {
        const links = await client.perspective.queryLinks(uuid, { predicate: NOTE } as any);
        return links.map((l: LinkExpression) => l.data.target);
    }

    before(async () => {
        if (!fs.existsSync(TEST_DIR)) throw Error("run prepare-test-local before the spaces suite");
        linkServer = await startLinkServer({ refreshIntervalMs: 2000 });
        seedPath = writePersonSeed(path.join(TEST_DIR, "spacesSeed.json"));

        a = await start("a", { credential: ADMIN, runArgs: MULTI_USER });
        b = await start("b", { credential: ADMIN, runArgs: MULTI_USER });

        alice = await user(a, "alice");
        bob = await user(b, "bob");
        carol = await user(b, "carol");
        dave = await user(b, "dave");
        alicePerson = await person(a, alice);
        bobPerson = await person(b, bob);
        carolPerson = await person(b, carol);
        expect((await dave.client.agent.me()).identity, "Dave speaks for no person").to.be.undefined;

        open = await space(alice, "spaces open", "alice's open note");
        home = await space(alice, "spaces home", "alice's home note");
    });

    after(async () => {
        for (const client of clients) client.close?.();
        for (const holder of holders) await holder.stop();
        for (const node of [...nodes].reverse()) {
            await quitExecutor(node.process, node.ports[0]);
            deregisterPorts(node.ports);
        }
        await linkServer?.kill();
    });

    it("1. Alice makes a space; Bob, with a person, joins it by its open link and reads Alice's link", async () => {
        const creator = (await alice.client.neighbourhood.members(open.uuid)).find((m) => m.person === alicePerson.did);
        expect(creator, "the creator is the space's first admin").to.include({ role: "admin" });
        const joined = await bob.client.neighbourhood.joinFromUrl(open.url);
        await pollUntil(async () => (await notes(bob.client, joined.uuid)).includes(literal("alice's open note")), {
            timeoutMs: 60_000,
            label: "Bob reads Alice's link in the space he joined by its open link",
        });
        await pollUntil(
            async () => (await alice.client.neighbourhood.members(open.uuid)).some((m) => m.person === bobPerson.did && m.role === "member"),
            { timeoutMs: 30_000, label: "Alice lists Bob as a member" },
        );
    });

    it("2. Alice's second executor: `members` shows her person once and `countByPerson` counts her once", async () => {
        a2 = await start("a2", { initArgs: ["--operator", alicePerson.did] });
        a2KeyId = await linkDevice(alicePerson, linkServer.store, a2.ports[0]);
        const me = await a2.admin.agent.me();
        expect(me.identity).to.equal(alicePerson.did);
        a2Key = me.did;
        expect(a2Key).to.match(/^did:key:/);

        a2Space = (await a2.admin.neighbourhood.joinFromUrl(home.url)).uuid;
        await pollUntil(async () => (await notes(a2.admin, a2Space)).includes(literal("alice's home note")), {
            timeoutMs: 60_000,
            label: "Alice's second device reads the space's link",
        });

        // One vote from each of Alice's devices: one person, whose latest link counts.
        await alice.client.perspective.addLink(home.uuid, { source: POLL, predicate: VOTE, target: literal("yes") });
        await sleep(1100);
        await a2.admin.perspective.addLink(a2Space, { source: POLL, predicate: VOTE, target: literal("no") });
        await pollUntil(
            async () => (await alice.client.perspective.queryLinks(home.uuid, { predicate: VOTE } as any)).length === 2,
            { timeoutMs: 60_000, label: "Alice's executor has both devices' votes" },
        );

        for (const [client, uuid] of [[alice.client, home.uuid], [a2.admin, a2Space]] as const) {
            const members = await client.neighbourhood.members(uuid);
            expect(members.map((m) => m.person), "members").to.deep.equal([alicePerson.did]);
        }
        const count = await alice.client.neighbourhood.countByPerson({ perspectiveUuid: home.uuid, source: POLL, predicate: VOTE });
        expect(count.counted).to.equal(1);
        expect(count.eligible).to.equal(1);
        expect(count.byTarget).to.deep.equal({ [literal("no")]: 1 });
    });

    it("3. `addBot`: Alice's bot links a vote, and `countByPerson` leaves it out as a bot's", async () => {
        const desk = await start("desk", {
            credential: DESK_ADMIN,
            env: { AD4M_CUSTODY_CHANNEL: "true" },
            custodyCredential: CUSTODY,
        });
        const expires = Math.floor(Date.now() / 1000) + 3600;
        const opened = await alicePerson.holder.send({
            cmd: "openBot", did: alicePerson.did, store: linkServer.store.url, can: ["read", "write", "join"], expires,
        });
        const created = await desk.admin.bot.createAgent(opened.offerString, "Teller");
        await alicePerson.holder.send({ cmd: "waitClaim" });
        await alicePerson.holder.send({ cmd: "confirm" });
        const botDid = created.did;

        await alice.client.neighbourhood.addBot(home.uuid, botDid);
        await desk.admin.bot.setGrant(botDid, {
            spaces: [home.url], can: ["read", "write", "join"], state: "active", version: 1,
        });
        const bot = new Ad4mClient(baseUrl(desk.ports[0]), created.token);
        clients.push(bot);
        bot.setReason("count the poll");
        const botSpace = (await bot.neighbourhood.joinFromUrl(home.url)).uuid;
        await bot.perspective.addLink(botSpace, { source: POLL, predicate: VOTE, target: literal("yes") });

        await pollUntil(
            async () => (await alice.client.perspective.queryLinks(home.uuid, { predicate: VOTE } as any)).some((l: LinkExpression) => l.author === botDid),
            { timeoutMs: 60_000, label: "Alice's executor has the bot's vote" },
        );
        const members = await alice.client.neighbourhood.members(home.uuid);
        expect(members.find((m) => m.person === botDid)).to.include({ role: "bot", botOf: alicePerson.did });
        const count = await alice.client.neighbourhood.countByPerson({ perspectiveUuid: home.uuid, source: POLL, predicate: VOTE });
        expect(count.skipped.bots).to.equal(1);
        expect(count.counted).to.equal(1);
    });

    it("4. invite-only: Carol's plain join is refused `invite-only`; with an invite she becomes a member", async () => {
        await alice.client.neighbourhood.setJoinMode(open.uuid, "invite");
        const events = await joinRefusals(carol.client);
        try {
            const message = await refused(carol.client.neighbourhood.joinFromUrl(open.url));
            expect(message).to.contain("invite-only");
            await pollUntil(() => events.seen.some((e) => e.url === open.url && e.reason === "invite-only"), {
                timeoutMs: 10_000,
                label: "neighbourhood-join-refused{invite-only} for Carol",
            });
        } finally {
            events.off();
        }

        const invite = await alice.client.neighbourhood.createInvite(open.uuid);
        const joined = await carol.client.neighbourhood.joinFromUrl(open.url, { invite: invite.invite });
        await pollUntil(
            async () => (await alice.client.neighbourhood.members(open.uuid)).some((m) => m.person === carolPerson.did && m.role === "member"),
            { timeoutMs: 60_000, label: "Alice lists Carol as a member" },
        );
        await pollUntil(async () => (await notes(carol.client, joined.uuid)).includes(literal("alice's open note")), {
            timeoutMs: 60_000,
            label: "Carol reads Alice's link",
        });
    });

    it("5. a device-only executor's join is refused `needs-identity`", async () => {
        const events = await joinRefusals(dave.client);
        try {
            const message = await refused(dave.client.neighbourhood.joinFromUrl(home.url));
            expect(message).to.contain("needs-identity");
            await pollUntil(() => events.seen.some((e) => e.url === home.url && e.reason === "needs-identity"), {
                timeoutMs: 10_000,
                label: "neighbourhood-join-refused{needs-identity} for Dave",
            });
        } finally {
            events.off();
        }
        const left = await dave.client.perspective.all();
        expect(left.map((p) => p.sharedUrl), "the refused join leaves no perspective").to.not.include(home.url);
    });

    it("6. revoking Alice's second device: the server closes its socket (4005), and `revocationReach` lists the space", async () => {
        if (!a2) throw new Error("test 2 made no second device");
        await alicePerson.holder.send({ cmd: "revoke", did: alicePerson.did, keyIds: [a2KeyId] });

        // The revoked device: the room closed its socket (4005), so the space has droppedAt. Only
        // the revoked key's own language hears the close, so only its executor notes the drop.
        await pollUntil(
            async () => (await a2!.admin.agent.revocationReach(a2Key)).some((r) => r.perspectiveUuid === a2Space && !!r.droppedAt),
            { timeoutMs: 60_000, label: "the second device's revocationReach lists the space with droppedAt" },
        );
        // Alice's executor: the space she holds, with the time the room key rotated past the key.
        await pollUntil(
            async () => (await alice.client.agent.revocationReach(a2Key)).some((r) => r.perspectiveUuid === home.uuid && !!r.rotatedAt),
            { timeoutMs: 60_000, label: "Alice's revocationReach lists the space with rotatedAt" },
        );
        const rows = await alice.client.agent.revocationReach(a2Key);
        const row = rows.find((r) => r.perspectiveUuid === home.uuid)!;
        expect(row).to.include({ didKey: a2Key, name: "spaces home" });
    });

    it("7. a membership link cannot be removed", async () => {
        const rules = await alice.client.perspective.queryLinks(home.uuid, { predicate: RULES } as any);
        expect(rules, "the space's rules link").to.have.length(1);
        expect(await refused(alice.client.perspective.removeLink(home.uuid, rules[0]))).to.contain("is monotonic");
        expect(await refused(alice.client.perspective.removeLinks(home.uuid, rules))).to.contain("is monotonic");
        expect(await alice.client.perspective.queryLinks(home.uuid, { predicate: RULES } as any)).to.have.length(1);
        expect((await alice.client.neighbourhood.members(home.uuid)).find((m) => m.person === alicePerson.did)).to.include({ role: "admin" });
    });
});

function literal(text: string): string {
    return `literal://string:${encodeURIComponent(text)}`;
}
