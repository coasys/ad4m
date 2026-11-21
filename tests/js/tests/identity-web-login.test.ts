/**
 * A web login end to end (Coasys Connect): a page signs in with the person's browser key, the
 * executor answers a one-time code, and the page redeems it with its PKCE verifier. One executor
 * with no Holochain, a real identity-store this suite starts, the centralized agent language, and
 * a fake app registry (`GET /v1/connect/apps/{appId}`) the executor is pointed at.
 *
 * The person's identity is made by the identity-client holder on the executor. The browser key
 * lives here, in JS, as in the Connect frame: it claims the holder's offer at the store with its
 * own co-signature, the holder confirms it as a `browser` device, and every statement is signed
 * here with that key. Nothing in the test asks the executor to sign.
 *
 * Needs `pnpm run prepare-test-local` first (the centralized agent language).
 */
import { expect } from "chai";
import crypto from "crypto";
import fs from "fs";
import http from "http";
import path from "path";
import { fileURLToPath } from "url";
import { ChildProcess } from "child_process";
import { Ad4mClient } from "@coasys/ad4m";
import { baseUrl, quitExecutor, startExecutor } from "../utils/utils";
import { getFreePorts, registerPorts, deregisterPorts } from "../helpers/ports.js";
import { buildIdentityTools, Holder, IdentityStore } from "../helpers/identity";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const TEST_DIR = path.join(__dirname, "..", "tst-tmp");
const SEED_PATH = path.join(TEST_DIR, "identityWebLoginBootstrapSeed.json");

const APP_ID = "web-login-test-app";
const APP_ORIGIN = "https://app.example";
const OTHER_ORIGIN = "https://other.example";
const APP = {
    appName: "web login test",
    appDesc: "a page signed in with its browser key",
    appDomain: "app.example",
    capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
};

const B58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

function b58encode(bytes: Uint8Array): string {
    let n = BigInt("0x" + Buffer.from(bytes).toString("hex") || "0");
    let out = "";
    while (n > 0n) {
        out = B58[Number(n % 58n)] + out;
        n /= 58n;
    }
    for (const byte of bytes) {
        if (byte !== 0) break;
        out = "1" + out;
    }
    return out;
}

function b58decode(text: string): Buffer {
    let n = 0n;
    for (const c of text) n = n * 58n + BigInt(B58.indexOf(c));
    let hex = n.toString(16);
    if (hex.length % 2) hex = "0" + hex;
    const zeros = text.length - text.replace(/^1+/, "").length;
    return Buffer.concat([Buffer.alloc(zeros), n === 0n ? Buffer.alloc(0) : Buffer.from(hex, "hex")]);
}

/** JSON Canonicalization Scheme for what this suite signs: objects, arrays, strings, integers, booleans. */
function jcs(value: unknown): string {
    if (Array.isArray(value)) return `[${value.map(jcs).join(",")}]`;
    if (value && typeof value === "object") {
        const entries = Object.entries(value as Record<string, unknown>)
            .filter(([, v]) => v !== undefined)
            .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
        return `{${entries.map(([k, v]) => `${JSON.stringify(k)}:${jcs(v)}`).join(",")}}`;
    }
    return JSON.stringify(value);
}

function rawPublic(key: crypto.KeyObject): Buffer {
    return Buffer.from(key.export({ format: "jwk" }).x!, "base64url");
}

function multikey(prefix: number[], key: crypto.KeyObject): string {
    return "z" + b58encode(Buffer.concat([Buffer.from(prefix), rawPublic(key)]));
}

/** A browser's device key: Ed25519 to sign, X25519 for the identity's sealed messages. */
class BrowserKey {
    private readonly signer = crypto.generateKeyPairSync("ed25519");
    private readonly enc = crypto.generateKeyPairSync("x25519");
    readonly key = multikey([0xed, 0x01], this.signer.publicKey);
    readonly encKey = multikey([0xec, 0x01], this.enc.publicKey);

    /** `signing_bytes(domain, value)`: JCS of the value with the domain member, signed as a proof value. */
    sign(domain: string, value: Record<string, unknown>): string {
        const bytes = Buffer.from(jcs({ ...value, domain }));
        return "z" + b58encode(crypto.sign(null, bytes, this.signer.privateKey));
    }
}

/** An app registry on loopback: the one app, its origins, and every path asked for. */
class FakeRegistry {
    readonly asked: string[] = [];
    private readonly server = http.createServer((req, res) => {
        this.asked.push(req.url!);
        res.setHeader("content-type", "application/json");
        if (req.url === `/v1/connect/apps/${APP_ID}`) {
            res.end(JSON.stringify({ origins: [APP_ORIGIN], firstParty: false }));
        } else {
            res.statusCode = 404;
            res.end(JSON.stringify({ error: "unknown app" }));
        }
    });

    constructor(readonly port: number) {}

    get url(): string {
        return `http://127.0.0.1:${this.port}`;
    }

    start(): Promise<void> {
        return new Promise((resolve) => this.server.listen(this.port, "127.0.0.1", resolve));
    }

    stop(): Promise<void> {
        return new Promise((resolve) => this.server.close(() => resolve()));
    }
}

/** The message of the refusal `call` ends in, or null when it succeeds. */
async function refusal(call: () => Promise<unknown>): Promise<string | null> {
    return call().then(
        () => null,
        (e: Error) => e.message,
    );
}

function pkce() {
    const verifier = crypto.randomBytes(32).toString("base64url");
    return { verifier, challenge: crypto.createHash("sha256").update(verifier).digest("base64url") };
}

describe("Identity: web login with the browser key (code, PKCE verifier, origin)", function () {
    this.timeout(600_000);

    let store: IdentityStore;
    let registry: FakeRegistry;
    let holder: Holder;
    let node: { dataPath: string; ports: number[]; process: ChildProcess | null; client: Ad4mClient };
    let did = "";
    const browser = new BrowserKey();

    /** The browser key's statement of `purpose` for a page at `origin`, signed here. */
    async function statement(
        purpose: "login" | "renew",
        origin: string,
        extra: { codeChallenge?: string; grant?: string },
    ) {
        const challenge = await node.client.agent.sessionChallenge();
        const body = {
            v: 1,
            purpose,
            scid: did,
            key: browser.key,
            executor: challenge.executor,
            audience: baseUrl(node.ports[0]),
            challenge: challenge.challenge,
            client: { kind: "web", origin },
            ...extra,
            issuedAt: Math.floor(Date.now() / 1000),
        };
        return { statement: body, sig: browser.sign("coasys:session", body) } as any;
    }

    /** A web login for a page at `origin` of the app `appId`: the code and the verifier. */
    async function login(origin = APP_ORIGIN, appId = APP_ID) {
        const { verifier, challenge } = pkce();
        const proof = await statement("login", origin, { codeChallenge: challenge });
        const answer = await node.client.agent.loginWithKeyForWeb(proof, APP, appId);
        return { ...answer, verifier };
    }

    before(async () => {
        if (!fs.existsSync(TEST_DIR)) throw Error("run prepare-test-local before the web login suite");
        const seed = JSON.parse(fs.readFileSync(path.join(__dirname, "..", "bootstrapSeed.json"), "utf8"));
        seed.agentLanguage = fs
            .readFileSync(path.join(__dirname, "..", "scripts", "centralized-agent-language-hash"), "utf8")
            .trim();
        if (!seed.agentLanguage) throw new Error("run prepare-test-local first: no centralized agent language");
        fs.writeFileSync(SEED_PATH, JSON.stringify(seed));
        buildIdentityTools();

        const [storePort, registryPort] = await getFreePorts(2);
        registerPorts([storePort, registryPort]);
        store = await IdentityStore.start(path.join(TEST_DIR, "identity-store-web-login"), storePort);
        registry = new FakeRegistry(registryPort);
        await registry.start();

        const ports = await getFreePorts(3);
        registerPorts(ports);
        const dataPath = path.join(TEST_DIR, "agents", "identity-web-login");
        fs.mkdirSync(path.dirname(dataPath), { recursive: true });
        const process = await startExecutor(
            dataPath, SEED_PATH, ports[0], ports[1], ports[2],
            false, undefined, undefined, undefined, undefined, false, undefined, undefined,
            false,          // runHolochain
            true,           // sharedStores
            { ...store.executorEnv(), AD4M_IDENTITY_REGISTRY_URL: registry.url },
        );
        const client = new Ad4mClient(baseUrl(ports[0]));
        await client.agent.generate("passphrase");
        node = { dataPath, ports, process, client };

        holder = new Holder();
        await holder.send({ cmd: "connect", url: baseUrl(ports[0]), token: "" });
        did = (await holder.send({ cmd: "create" })).did;
    });

    after(async () => {
        await holder?.stop();
        if (node?.process) await quitExecutor(node.process, node.ports[0]);
        if (node) deregisterPorts(node.ports);
        await registry?.stop();
        await store?.stop();
        if (store) deregisterPorts([store.port, registry.port]);
    });

    it("links a browser key held in JS: it claims the holder's offer, the holder lists it as a browser", async () => {
        const opened = await holder.send({ cmd: "open", did, store: store.url });
        const offer = JSON.parse(
            b58decode(opened.offerString.slice(opened.offerString.indexOf("coasys:offer:1:") + "coasys:offer:1:".length)).toString(),
        );
        const claim = {
            offerId: offer.offerId,
            s: offer.s,
            deviceKey: browser.key,
            encKey: browser.encKey,
            coSig: "",
        };
        claim.coSig = browser.sign("coasys:claim", {
            scid: offer.scid,
            offerId: offer.offerId,
            deviceKey: browser.key,
            encKey: browser.encKey,
            canKeepMaster: false,
        });
        const claimed = await fetch(`${offer.store}/v1/offers/${offer.scid}/${offer.offerId}/claim`, {
            method: "POST",
            headers: { "content-type": "application/json" },
            body: JSON.stringify(claim),
        });
        expect(claimed.status, await claimed.clone().text()).to.be.below(300);

        const seen = await holder.send({ cmd: "waitClaim" });
        expect(seen.deviceKey).to.equal(browser.key);
        const confirmed = await holder.send({ cmd: "confirm", name: "Browser", kind: "browser" });
        expect(confirmed.seq).to.equal(1);
        const page = await (await fetch(`${store.url}/v1/logs/${did.split(":").pop()}`)).json();
        expect(page.events).to.have.length(2);
    });

    it("answers a code, not a token, to a web statement; the registry was asked for the app", async () => {
        const answer = await login();
        expect(answer.code).to.be.a("string").that.is.not.empty;
        expect(answer).to.not.have.property("token");
        const now = Math.floor(Date.now() / 1000);
        expect(answer.expiresAt).to.be.within(now, now + 61);
        expect(registry.asked).to.include(`/v1/connect/apps/${APP_ID}`);
    });

    it("redeems the code with the verifier: a working session of the browser key, once", async () => {
        const { code, verifier } = await login();
        const session = await node.client.agent.redeemCode(code, verifier);
        expect(session.token).to.be.a("string").that.is.not.empty;
        expect(session.scid).to.equal(did);
        expect(session.role).to.equal("app");

        const client = new Ad4mClient(baseUrl(node.ports[0]), session.token);
        const info = await client.agent.currentSession();
        expect(info.keyId).to.equal("d2");
        expect(info.scid).to.equal(did);
        expect(info.grantId).to.equal(session.grantId);
        expect((await client.identity.status()).did).to.equal(did);

        // The code is spent: a second redeem with the right verifier is refused.
        expect(await refusal(() => node.client.agent.redeemCode(code, verifier))).to.contain("code");
    });

    it("refuses a wrong verifier, and the refusal spends the code", async () => {
        const { code, verifier } = await login();
        expect(await refusal(() => node.client.agent.redeemCode(code, pkce().verifier))).to.contain("code");
        expect(await refusal(() => node.client.agent.redeemCode(code, verifier))).to.contain("code");
    });

    it("refuses an origin the registry does not list for the app, and an app it does not know", async () => {
        expect(await refusal(() => login(OTHER_ORIGIN))).to.contain("origin");
        expect(await refusal(() => login(APP_ORIGIN, "no-such-app"))).to.contain("origin");
    });

    it("renews the session for its own origin and refuses a renewal from another origin", async () => {
        const { code, verifier } = await login();
        const session = await node.client.agent.redeemCode(code, verifier);
        const client = new Ad4mClient(baseUrl(node.ports[0]), session.token);

        const foreign = await statement("renew", OTHER_ORIGIN, { grant: session.grantId });
        expect(await refusal(() => client.agent.renewSession(foreign))).to.contain("grant");

        const own = await statement("renew", APP_ORIGIN, { grant: session.grantId });
        const renewed = await client.agent.renewSession(own);
        expect(renewed.token).to.be.a("string").that.is.not.empty;
    });
});
