import { test } from "node:test";
import assert from "node:assert/strict";
import { defineLanguage } from "../src/defineLanguage.js";

test("defineLanguage: minimal lifecycle", () => {
    const lang = defineLanguage({
        name: "minimal",
        version: "1.0.0",
        async init() {},
    });
    assert.equal(lang.name, "minimal");
    assert.equal(lang.version, "1.0.0");
    assert.equal(typeof lang.init, "function");
    assert.equal(lang.isPublic, undefined);
});

test("defineLanguage: isPublic boolean → function getter", () => {
    const pub = defineLanguage({ name: "p", isPublic: true, async init() {} });
    const priv = defineLanguage({ name: "p", isPublic: false, async init() {} });
    assert.equal(pub.isPublic?.(), true);
    assert.equal(priv.isPublic?.(), false);
});

test("defineLanguage: commit + sync + peers → flat exports", () => {
    let committed: any = null;
    const lang = defineLanguage({
        name: "ps",
        async init() {},
        commit: { async commit(d) { committed = d; } },
        sync: {
            async sync() { return { additions: [], removals: [] }; },
            async render() { return { links: [] }; },
            currentRevision() { return "abc"; },
        },
        peers: {
            setLocal(_a) {},
            async remote() { return ["did:key:zBob"]; },
        },
    });
    assert.equal(typeof lang.perspectiveCommit, "function");
    assert.equal(typeof lang.perspectiveSyncSync, "function");
    assert.equal(typeof lang.perspectiveSyncRender, "function");
    assert.equal(typeof lang.perspectiveSyncCurrentRevision, "function");
    assert.equal(typeof lang.peersSetLocal, "function");
    assert.equal(typeof lang.peersRemote, "function");

    return Promise.resolve(lang.perspectiveCommit!({ additions: [], removals: [] }))
        .then(() => assert.deepEqual(committed, { additions: [], removals: [] }));
});

test("defineLanguage: expression capability fields", () => {
    const lang = defineLanguage({
        name: "e",
        async init() {},
        expression: {
            async get(_a) { return null; },
            async create(_c) { return "addr"; },
            isImmutable(_a) { return true; },
            icon() { return "<icon/>"; },
            constructorIcon() { return "<ctor/>"; },
        },
    });
    assert.equal(typeof lang.expressionGet, "function");
    assert.equal(typeof lang.expressionCreate, "function");
    assert.equal(typeof lang.isImmutableExpression, "function");
    assert.equal(lang.expressionIcon?.(), "<icon/>");
    assert.equal(lang.expressionConstructorIcon?.(), "<ctor/>");
});

test("defineLanguage: query capability", async () => {
    const lang = defineLanguage({
        name: "q",
        async init() {},
        query: {
            supportedKinds() { return ["link", "prolog"]; },
            async run(req) { return { kind: req.kind, payload: "ok" }; },
        },
    });
    assert.deepEqual(lang.perspectiveQuerySupportedKinds?.(), ["link", "prolog"]);
    const r = await lang.perspectiveQueryRun?.({ kind: "link", payload: null });
    assert.deepEqual(r, { kind: "link", payload: "ok" });
});

test("defineLanguage: telepresence + holochain signal handler", () => {
    const lang = defineLanguage({
        name: "t",
        async init() {},
        telepresence: {
            async setOnlineStatus(_s) {},
            async getOnlineAgents() { return []; },
        },
        handleHolochainSignal(_s) {},
    });
    assert.equal(typeof lang.telepresenceSetOnlineStatus, "function");
    assert.equal(typeof lang.telepresenceGetOnlineAgents, "function");
    assert.equal(typeof lang.handleHolochainSignal, "function");
});
