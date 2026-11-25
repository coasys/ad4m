import { test } from "node:test";
import assert from "node:assert/strict";
import {
    defineLanguage,
    resolveMountCallSources,
} from "../src/defineLanguage.js";
import { Model } from "../../../core/src/model/decorators";
// The decorated class is a single source of truth shared with core's
// `mount-classes.test.ts` — it lives under ad4m-ldk/tests/ so that
// `ad4m-ldk/tsconfig.test.json` (TSX_TSCONFIG_PATH) covers it for tsx;
// jest compiles it with core's own `experimentalDecorators`.
import { MountThing } from "../../tests/mount-fixtures.js";

/** The executor's canonical JSON: every object's keys sorted. */
function canonicalJson(value: unknown): string {
    if (Array.isArray(value)) {
        return `[${value.map(canonicalJson).join(",")}]`;
    }
    if (value !== null && typeof value === "object") {
        return `{${Object.keys(value as object).sort()
            .filter((k) => (value as any)[k] !== undefined)
            .map((k) => `${JSON.stringify(k)}:${canonicalJson((value as any)[k])}`)
            .join(",")}}`;
    }
    return JSON.stringify(value);
}

const updateCall = [{ action: "call", source: "this", predicate: "update", target: "{}" }];

/**
 * The shape `rust-executor/src/languages/fixtures/mount_test_language.js`
 * builds by hand inside its `mountShapes()`, at
 * `languageAddress() === "langtest"`.
 */
const HAND_WRITTEN_SHAPE = {
    target_class: "mounttest://Thing",
    constructor_actions: [
        { action: "call", source: "langtest://class/MountThing", predicate: "create", target: "{}" },
    ],
    destructor_actions: [
        { action: "call", source: "this", predicate: "delete", target: "{}" },
    ],
    properties: [
        { path: "rdf://type", name: "type", has_value: "mounttest://Thing", min_count: 1, max_count: 1 },
        { path: "mounttest://title", name: "title", max_count: 1, writable: true, setter: updateCall },
        { path: "mounttest://done", name: "done", max_count: 1, writable: true, setter: updateCall },
        { path: "mounttest://tag", name: "tags", collection: true, writable: true, setter: updateCall },
    ],
};

const emptySnapshot = () => ({ entries: [] });

test("mount.classes: mountShapes() equals the hand-written fixture, byte for byte", async () => {
    const lang = defineLanguage({
        name: "mount-test",
        async init() {},
        mount: {
            classes: [MountThing],
            version: "1",
            snapshot: emptySnapshot,
            languageAddress: "langtest",
        },
    });

    const shapes = await lang.mountShapes!();
    assert.equal(shapes.version, "1");
    assert.equal(shapes.shapes.length, 1);
    assert.equal(shapes.shapes[0].name, "MountThing");
    assert.equal(canonicalJson(shapes.shapes[0].shape), canonicalJson(HAND_WRITTEN_SHAPE));

    assert.deepEqual(await lang.mountSnapshot!(), { entries: [] });
});

test("mount.classes: {language} resolves through a function, version through one too", async () => {
    const lang = defineLanguage({
        name: "mount-test",
        async init() {},
        mount: {
            classes: [MountThing],
            version: () => "v2",
            snapshot: emptySnapshot,
            languageAddress: async () => "langfn",
        },
    });
    const shapes = await lang.mountShapes!();
    assert.equal(shapes.version, "v2");
    const ctor = (shapes.shapes[0].shape as any).constructor_actions[0];
    assert.equal(ctor.source, "langfn://class/MountThing");
});

test("mount.classes: class with no {language} placeholder needs no address", async () => {
    class PlainRecord {
        static className = "PlainRecord";
        static generateSHACL() {
            return {
                name: "PlainRecord",
                shape: {
                    toJSON: () => ({
                        node_shape_uri: "plain://PlainRecordShape",
                        target_class: "plain://PlainRecord",
                        properties: [],
                    }),
                },
            };
        }
    }
    // No languageAddress at all — a missing host import must not matter.
    const lang = defineLanguage({
        name: "m",
        async init() {},
        mount: { classes: [PlainRecord as any], version: "1", snapshot: emptySnapshot },
    });
    const shapes = await lang.mountShapes!();
    // node_shape_uri is dropped for the mount form; the parser derives it.
    assert.deepEqual(shapes.shapes[0], {
        name: "PlainRecord",
        shape: { target_class: "plain://PlainRecord", properties: [] },
    });
});

test("mount.classes: a class without generateSHACL fails loudly", async () => {
    class Bare {}
    const lang = defineLanguage({
        name: "m",
        async init() {},
        mount: { classes: [Bare as any], version: "1", snapshot: emptySnapshot },
    });
    await assert.rejects(() => lang.mountShapes!(), /generateSHACL/);
});

test("resolveMountCallSources substitutes {language} in every action list", () => {
    const shape = {
        constructor_actions: [{ action: "call", source: "{language}://class/X", predicate: "create", target: "{}" }],
        properties: [
            { path: "p", setter: [{ action: "call", source: "{language}://rec/1", predicate: "s", target: "{}" }] },
            { path: "q", adder: [{ action: "call", source: "this", predicate: "a", target: "{}" }] },
            { path: "r", remover: [{ action: "call", source: "this", predicate: "r", target: "{\"to\":\"{language}://x\"}" }] },
        ],
    };
    const out = resolveMountCallSources(shape, "langabc") as any;
    assert.equal(out.constructor_actions[0].source, "langabc://class/X");
    assert.equal(out.properties[0].setter[0].source, "langabc://rec/1");
    assert.equal(out.properties[1].adder[0].source, "this");
    assert.equal(out.properties[2].remover[0].target, '{"to":"langabc://x"}');
    // Input not mutated (generateSHACL results are memoized upstream).
    assert.equal(shape.properties[0].setter[0].source, "{language}://rec/1");
});

test("mount.classes: {language} on a non-call action stays byte-identical and does not import the host", async () => {
    const link = {
        action: "addLink",
        source: "this",
        predicate: "lit://note",
        target: "literal {language} text",
    };
    class LiteralLink {
        static className = "LiteralLink";
        static generateSHACL() {
            return {
                name: "LiteralLink",
                shape: {
                    toJSON: () => ({
                        node_shape_uri: "lit://LiteralLinkShape",
                        target_class: "lit://LiteralLink",
                        constructor_actions: [link],
                        properties: [],
                    }),
                },
            };
        }
    }
    // No languageAddress: a placeholder on a non-call action must not
    // reach `import("./imports.js")` (that module loads `ad4m:host`).
    const lang = defineLanguage({
        name: "m",
        async init() {},
        mount: { classes: [LiteralLink as any], version: "1", snapshot: emptySnapshot },
    });
    const shapes = await lang.mountShapes!();
    const got = (shapes.shapes[0].shape as any).constructor_actions[0];
    assert.equal(got, link);
    assert.equal(canonicalJson(got), canonicalJson(link));
});

test("mount.classes: a call action substitutes source and target; a non-call action stays byte-identical", async () => {
    const call = {
        action: "call",
        source: "{language}://class/X",
        predicate: "create",
        target: "see {language}://other",
    };
    const link = {
        action: "setSingleTarget",
        source: "{language}://stay",
        predicate: "lit://note",
        target: "literal {language} text",
    };
    let calls = 0;
    class Mixed {
        static className = "Mixed";
        static generateSHACL() {
            return {
                name: "Mixed",
                shape: {
                    toJSON: () => ({
                        node_shape_uri: "mix://MixedShape",
                        target_class: "mix://Mixed",
                        constructor_actions: [call],
                        properties: [{ path: "lit://note", setter: [link] }],
                    }),
                },
            };
        }
    }
    const lang = defineLanguage({
        name: "m",
        async init() {},
        mount: {
            classes: [Mixed as any],
            version: "1",
            snapshot: emptySnapshot,
            languageAddress() {
                calls += 1;
                return "langabc";
            },
        },
    });
    const shapes = await lang.mountShapes!();
    const shape = shapes.shapes[0].shape as any;
    assert.equal(calls, 1);
    assert.equal(shape.constructor_actions[0].source, "langabc://class/X");
    assert.equal(shape.constructor_actions[0].target, "see langabc://other");
    assert.equal(shape.properties[0].setter[0], link);
    assert.equal(canonicalJson(shape.properties[0].setter[0]), canonicalJson(link));
});

test("mount.classes: {language} only in interpretation_hint does not resolve an address", async () => {
    let called = false;
    class HintOnly {
        static className = "HintOnly";
        static generateSHACL() {
            return {
                name: "HintOnly",
                shape: {
                    toJSON: () => ({
                        node_shape_uri: "h://HintOnlyShape",
                        target_class: "h://HintOnly",
                        interpretation_hint: "Keep {language} literal",
                        properties: [],
                    }),
                },
            };
        }
    }
    const lang = defineLanguage({
        name: "m",
        async init() {},
        mount: {
            classes: [HintOnly as any],
            version: "1",
            snapshot: emptySnapshot,
            languageAddress() {
                called = true;
                return "should-not-run";
            },
        },
    });
    const shapes = await lang.mountShapes!();
    assert.equal(called, false);
    assert.equal((shapes.shapes[0].shape as any).interpretation_hint, "Keep {language} literal");
});

test("mount.classes: languageAddress keeps the mount receiver", async () => {
    class NeedsAddr {
        static className = "NeedsAddr";
        static generateSHACL() {
            return {
                name: "NeedsAddr",
                shape: {
                    toJSON: () => ({
                        node_shape_uri: "n://NeedsAddrShape",
                        target_class: "n://NeedsAddr",
                        constructor_actions: [
                            { action: "call", source: "{language}://class/NeedsAddr", predicate: "create", target: "{}" },
                        ],
                        properties: [],
                    }),
                },
            };
        }
    }
    const mount = {
        x: "from-this",
        classes: [NeedsAddr as any],
        version: "1",
        snapshot: emptySnapshot,
        languageAddress() { return this.x; },
    };
    const lang = defineLanguage({
        name: "m",
        async init() {},
        mount,
    });
    const shapes = await lang.mountShapes!();
    assert.equal(
        (shapes.shapes[0].shape as any).constructor_actions[0].source,
        "from-this://class/NeedsAddr",
    );
});

test("mount.classes: parent_shapes use the URI the executor derives from target_class", async () => {
    @Model({
        name: "NsParent",
        namespace: "declared://",
        targetClass: "other://ParentRec",
        mount: true,
    })
    class NsParent {}

    @Model({
        name: "NsChild",
        namespace: "declared://",
        targetClass: "elsewhere://ChildRec",
        mount: true,
    })
    class NsChild extends NsParent {}

    const lang = defineLanguage({
        name: "m",
        async init() {},
        mount: {
            classes: [NsParent, NsChild],
            version: "1",
            snapshot: emptySnapshot,
        },
    });
    const shapes = await lang.mountShapes!();
    const byName = Object.fromEntries(shapes.shapes.map((s) => [s.name, s.shape as any]));
    assert.equal(byName.NsParent.node_shape_uri, undefined);
    assert.equal(byName.NsParent.target_class, "other://ParentRec");
    assert.equal(byName.NsChild.node_shape_uri, undefined);
    assert.equal(byName.NsChild.target_class, "elsewhere://ChildRec");
    // Declared shape is `declared://NsParentShape`. The executor stores the
    // parent at `other://ParentRecShape` (`{namespace of target_class}{local}Shape`).
    assert.deepEqual(byName.NsChild.parent_shapes, ["other://ParentRecShape"]);
});
