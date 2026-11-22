import { test } from "node:test";
import assert from "node:assert/strict";
import {
    defineLanguage,
    resolveMountCallSources,
} from "../src/defineLanguage.js";
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
