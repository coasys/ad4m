import { test } from "node:test";
import assert from "node:assert/strict";
import { defineLanguage } from "../src/defineLanguage.js";
import {
    IdentityError,
    LanguageError,
    TokenError,
    parseIdentityError,
    parseTokenError,
} from "../src/errors.js";
import {
    TOKEN_TRANSFER_SIGNAL,
    tokenTransferSignal,
    type AgentRecord,
    type AppendRequest,
    type Claim,
    type IdentitySpec,
    type JsonObject,
    type LogPage,
    type SignedHead,
    type SignedOffer,
    type SignedRequest,
    type TokenSpec,
    type TokenTransfer,
} from "../src/types.js";

// A test agent language: every call records what it got and answers from
// fixed data, so the test sees exactly what crossed `defineLanguage`.

const head: SignedHead = {
    scid: "zScid",
    seq: 1,
    hash: "zHash1",
    time: 1_700_000_000,
    store: "did:key:zStore",
    sig: "zSig",
};

// Events are opaque: odd keys, nesting and numbers must cross untouched.
const events: JsonObject[] = [
    { v: 1, op: "incept", seq: 0, keys: [{ id: "d1", sign: "z6Mk1", enc: "z6LS1" }], "x-extra": { deep: [1, null, true] } },
    { v: 1, op: "add", seq: 1, prev: "zHash0", add: { id: "d2", kind: "bot", can: ["read"], expires: 9 } },
];

const request: SignedRequest = {
    req: {
        did: "did:scid:zScid",
        keyId: "m1",
        kelSeq: 1,
        kelHash: "zHash1",
        method: "GET",
        path: "/v1/offers/zScid",
        bodyHash: "zBody",
        nonce: "zNonce",
        expires: 1_700_000_300,
    },
    sig: "zReqSig",
};

const signedOffer: SignedOffer = {
    offer: { scid: "zScid", offerId: "zOffer", kind: "device", commit: "zCommit", expires: 1_700_000_300 },
    by: { keyId: "m1", kelSeq: 1, kelHash: "zHash1" },
    sig: "zOfferSig",
};

const transfer: TokenTransfer = {
    id: "t1",
    from: "did:scid:zA",
    to: "did:key:zB",
    asset: "unyt:x",
    amount: { value: "12345678901234567890", decimals: 18 },
    time: 5,
    state: "settled",
};

function testLanguage() {
    const calls: Array<[string, unknown[]]> = [];
    let transferSink: ((t: TokenTransfer) => void) | null = null;
    const identity: IdentitySpec & { calls: typeof calls } = {
        calls,
        async log(did, from) {
            this.calls.push(["log", [did, from]]);
            const page: LogPage = {
                events,
                appendedAt: [10, 11],
                head,
                note: null,
                vouched: false,
                pauses: [],
            };
            return page;
        },
        async head(did) { this.calls.push(["head", [did]]); return head; },
        async append(did, req) { this.calls.push(["append", [did, req]]); return head; },
        async keyOwner(multikey, waitSec) {
            this.calls.push(["keyOwner", [multikey, waitSec]]);
            return null;
        },
        async powParams() { this.calls.push(["powParams", []]); return { bits: 22, previousBits: 20, changedAt: 3 }; },
        async attachNote(did, token, req) {
            this.calls.push(["attachNote", [did, token, req]]);
            return { did, method: "email", time: 4, voucher: "did:key:zVouch", sig: "zNoteSig" };
        },
        async putAgent(record, req) { this.calls.push(["putAgent", [record, req]]); },
        async openOffer(offer) { this.calls.push(["openOffer", [offer]]); return { offerId: offer.offer.offerId }; },
        async listOffers(did, req, waitSec) {
            this.calls.push(["listOffers", [did, req, waitSec]]);
            return [{ offer: signedOffer, state: { state: "open" } }];
        },
        async cancelOffer(did, offerId, req) { this.calls.push(["cancelOffer", [did, offerId, req]]); },
        async claimOffer(did, offerId, claim) {
            this.calls.push(["claimOffer", [did, offerId, claim]]);
            return { state: "claimed", kind: "device", expires: 1_700_000_300 };
        },
        async offerState(did, offerId, key, waitSec) {
            this.calls.push(["offerState", [did, offerId, key, waitSec]]);
            return { state: "spent", seq: 2 };
        },
    };
    const token: TokenSpec & { calls: typeof calls } = {
        calls,
        async assets() { this.calls.push(["assets", []]); return ["unyt:x"]; },
        async balance(account, asset) {
            this.calls.push(["balance", [account, asset]]);
            return { account, asset, amount: { value: "0", decimals: 0 }, asOf: 1 };
        },
        async send(to, asset, amount, memo) { this.calls.push(["send", [to, asset, amount, memo]]); return transfer; },
        async received(cursor, limit) {
            this.calls.push(["received", [cursor, limit]]);
            return { items: [transfer], cursor: "c2" };
        },
        onTransfer(callback) { this.calls.push(["onTransfer", []]); transferSink = callback; },
    };
    const lang = defineLanguage({ name: "agent", async init() {}, identity, token });
    return { lang, calls, fire: (t: TokenTransfer) => transferSink?.(t) };
}

test("identity and token: flat exports carry the interface's names", () => {
    const { lang } = testLanguage();
    const identityNames = [
        "identityLog", "identityHead", "identityAppend", "identityKeyOwner",
        "identityPowParams", "identityAttachNote", "identityPutAgent",
        "identityOpenOffer", "identityListOffers", "identityCancelOffer",
        "identityClaimOffer", "identityOfferState",
    ];
    const tokenNames = ["tokenAssets", "tokenBalance", "tokenSend", "tokenReceived", "tokenOnTransfer"];
    for (const name of [...identityNames, ...tokenNames]) {
        assert.equal(typeof (lang as any)[name], "function", name);
    }
    const flat = Object.keys(lang).filter((k) => k.startsWith("identity") || k.startsWith("token"));
    assert.deepEqual(flat.sort(), [...identityNames, ...tokenNames].sort());
});

test("identity and token: no exports without the capability", () => {
    const lang = defineLanguage({ name: "plain", async init() {} });
    assert.deepEqual(Object.keys(lang).filter((k) => k.startsWith("identity") || k.startsWith("token")), []);
});

test("identity: events and requests pass through unchanged, as free functions", async () => {
    const { lang, calls } = testLanguage();
    // The runtime calls flat exports with no receiver.
    const { identityLog, identityAppend, identityHead, identityKeyOwner, identityPowParams,
        identityAttachNote, identityPutAgent } = lang;

    const page = await identityLog!("did:scid:zScid", 0);
    assert.deepEqual(page!.events, events);
    assert.equal(page!.events, events, "the page carries the language's own event objects");
    assert.deepEqual(JSON.parse(JSON.stringify(page)).events, events);

    const req: AppendRequest = { events, vouch: null, pow: { nonce: "zNonce16", bits: 22 } };
    assert.deepEqual(await identityAppend!("did:scid:zScid", req), head);
    assert.equal((calls.at(-1)![1][1] as AppendRequest).events, events);
    assert.equal(calls.at(-1)![1][1], req);

    assert.deepEqual(await identityHead!("did:scid:zScid"), head);
    assert.equal(await identityKeyOwner!("z6MkKey"), null);
    assert.deepEqual(calls.at(-1), ["keyOwner", ["z6MkKey", undefined]]);
    assert.deepEqual(await identityPowParams!(), { bits: 22, previousBits: 20, changedAt: 3 });
    assert.equal((await identityAttachNote!("did:scid:zScid", "tok", request)).method, "email");
    assert.equal(calls.at(-1)![1][2], request);

    const record: AgentRecord = {
        did: "did:key:zBot",
        profile: { perspective: { links: [] }, directMessageLanguage: "lang://x" },
        version: 3,
        signedBy: "did:key:zBot",
        anchor: null,
        sig: "zRecSig",
    };
    await identityPutAgent!(record);
    assert.deepEqual(calls.at(-1), ["putAgent", [record, undefined]]);
    assert.equal(calls.at(-1)![1][0], record);
});

test("identity: offer and claim calls", async () => {
    const { lang, calls } = testLanguage();
    const { identityOpenOffer, identityListOffers, identityCancelOffer, identityClaimOffer, identityOfferState } = lang;
    assert.deepEqual(await identityOpenOffer!(signedOffer), { offerId: "zOffer" });
    assert.equal(calls.at(-1)![1][0], signedOffer);
    assert.deepEqual(await identityListOffers!("did:scid:zScid", request, 8), [{ offer: signedOffer, state: { state: "open" } }]);
    assert.deepEqual(calls.at(-1), ["listOffers", ["did:scid:zScid", request, 8]]);
    await identityCancelOffer!("did:scid:zScid", "zOffer", request);
    assert.deepEqual(calls.at(-1), ["cancelOffer", ["did:scid:zScid", "zOffer", request]]);
    const claim: Claim = { offerId: "zOffer", s: "zS", deviceKey: "z6MkNew", encKey: "z6LSNew", canKeepMaster: true, coSig: "zCo" };
    assert.deepEqual(await identityClaimOffer!("did:scid:zScid", "zOffer", claim), { state: "claimed", kind: "device", expires: 1_700_000_300 });
    assert.equal(calls.at(-1)![1][2], claim);
    assert.deepEqual(await identityOfferState!("did:scid:zScid", "zOffer", "z6MkNew", 2), { state: "spent", seq: 2 });
});

test("token: calls and the transfer callback", async () => {
    const { lang, calls, fire } = testLanguage();
    const { tokenAssets, tokenBalance, tokenSend, tokenReceived, tokenOnTransfer } = lang;
    assert.deepEqual(await tokenAssets!(), ["unyt:x"]);
    assert.equal((await tokenBalance!("did:key:zA", "unyt:x")).amount.value, "0");
    const amount = { value: "1.5", decimals: 0 };
    assert.deepEqual(await tokenSend!("did:key:zB", "unyt:x", amount, "hi"), transfer);
    assert.equal(calls.at(-1)![1][2], amount);
    assert.deepEqual(await tokenReceived!(null, 10), { items: [transfer], cursor: "c2" });
    const seen: unknown[] = [];
    tokenOnTransfer!((t) => seen.push(tokenTransferSignal(t)));
    fire(transfer);
    assert.deepEqual(seen, [{ tag: "token-transfer", transfer }]);
    assert.equal(TOKEN_TRANSFER_SIGNAL, "token-transfer");
});

test("identity and token errors: refusals survive as message JSON", () => {
    const conflict = new IdentityError({ code: "conflict", head });
    assert.ok(conflict instanceof LanguageError);
    assert.equal(conflict.code, "invalid-input");
    assert.deepEqual(parseIdentityError(conflict.message), { code: "conflict", head });
    const limited = new IdentityError({ code: "rate-limited", scope: "identity", retryAfterSec: 30 });
    assert.equal(limited.code, "transient");
    assert.deepEqual(JSON.parse(limited.message), { code: "rate-limited", scope: "identity", retryAfterSec: 30 });
    assert.equal(new IdentityError({ code: "forbidden", reason: "secret" }).code, "permission-denied");
    assert.equal(new IdentityError({ code: "not-found" }).code, "not-found");
    assert.equal(parseIdentityError("plain message"), null);
    assert.equal(parseIdentityError(JSON.stringify({ code: "no-such-code" })), null);

    const disabled = new TokenError({ code: "disabled" });
    assert.equal(disabled.code, "permission-denied");
    assert.deepEqual(parseTokenError(disabled.message), { code: "disabled" });
    assert.equal(parseTokenError(conflict.message), null);
});
