/**
 * Mount-mode decorator tests: `@Model({ mount, constructorActions,
 * destructorActions })`, the call-action options (`setter`, `adder`,
 * `remover`), and the parity gate against the hand-written mount shape in
 * `rust-executor/src/languages/fixtures/mount_test_language.js`.
 *
 * The same `GoldenRecord` class backs the cross-language golden fixture
 * `ad4m-ldk/tests/golden_record_shape.json` — this test is the fixture's
 * writer and reader (the Rust `#[derive(Ad4mModel)]` parity test is the
 * other reader).
 */
import { readFileSync } from "fs";
import { join } from "path";
import {
    Model,
    Property,
    Optional,
    ReadOnly,
    Flag,
    HasMany,
    HasOne,
} from "./decorators";
import { SHACLShape } from "../shacl/SHACLShape";
import { MountThing, GoldenRecord } from "../../../ad4m-ldk/tests/mount-fixtures";

/** The executor's canonical JSON: every object's keys sorted (UTF-16
 *  order), one spelling — `canonical_json` in shacl_parser.rs. */
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
 * The shape `mount_test_language.js#mountShapes` builds by hand, at
 * `languageAddress() === "langtest"`. The decorator form differs only by
 * carrying `node_shape_uri` (`toJSON()` always emits it; the LDK's
 * `mount.classes` drops it again because the parser derives it) and by
 * keeping the `{language}` placeholder unresolved until mountShapes() time.
 */
const HAND_WRITTEN_MOUNT_THING = {
    node_shape_uri: "mounttest://MountThingShape",
    target_class: "mounttest://Thing",
    constructor_actions: [
        { action: "call", source: "{language}://class/MountThing", predicate: "create", target: "{}" },
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

describe("mounted MountThing — decorator parity with the hand-written fixture", () => {
    it("is byte-equal after key sort", () => {
        const { shape } = (MountThing as any).generateSHACL();
        expect(canonicalJson(shape.toJSON())).toBe(canonicalJson(HAND_WRITTEN_MOUNT_THING));
    });
});

describe("call action options", () => {
    it("emits call setter/adder/remover/constructor/destructor in the executor's wire form", () => {
        const json = (GoldenRecord as any).generateSHACL().shape.toJSON();

        expect(json.constructor_actions).toEqual([
            { action: "call", source: "{language}://class/GoldenRecord", predicate: "create", target: "{}" },
            { action: "call", source: "{language}://class/GoldenRecord", predicate: "index", target: '{"kind":"record"}' },
        ]);
        expect(json.destructor_actions).toEqual([
            { action: "call", source: "this", predicate: "delete", target: "{}" },
        ]);

        const props = Object.fromEntries(json.properties.map((p: any) => [p.name, p]));
        expect(props.title.setter).toEqual(updateCall);
        expect(props.tags.setter).toEqual(updateCall);
        expect(props.tags.collection).toBe(true);
        expect(props.tags.max_count).toBeUndefined();
        expect(props.members.adder).toEqual([
            { action: "call", source: "this", predicate: "add_member", target: "{}" },
        ]);
        expect(props.members.remover).toEqual([
            { action: "call", source: "this", predicate: "remove_member", target: '{"soft":true}' },
        ]);
    });

    it("keeps the fixture's read shapes: flag, read-only property, IRI refs", () => {
        const json = (GoldenRecord as any).generateSHACL().shape.toJSON();
        const props = Object.fromEntries(json.properties.map((p: any) => [p.name, p]));

        // Flag — mount emits no writable for it.
        expect(props.type).toEqual({
            path: "rdf://type", name: "type", has_value: "mountgold://Record", min_count: 1, max_count: 1,
        });
        // Read-only property: path + cardinality, nothing else.
        expect(props.created).toEqual({
            path: "mountgold://created", name: "created", max_count: 1,
        });
        // IRI reference, read-only — relation kind + node kind, no writer.
        expect(props.owner).toEqual({
            path: "mountgold://owner", name: "owner", node_kind: "IRI", relation_kind: "hasOne", max_count: 1,
        });
        // IRI collection with call adder/remover.
        expect(props.members.node_kind).toBe("IRI");
        expect(props.members.relation_kind).toBe("hasMany");
        expect(props.members.collection).toBe(true);
    });

    it("round-trips every property kind through SHACLShape.fromJSON", () => {
        const json = (GoldenRecord as any).generateSHACL().shape.toJSON();
        const roundTripped = SHACLShape.fromJSON(json).toJSON();
        expect(canonicalJson(roundTripped)).toBe(canonicalJson(json));
    });

    it("matches the shared golden fixture", () => {
        const json = (GoldenRecord as any).generateSHACL().shape.toJSON();
        const golden = readFileSync(
            join(__dirname, "../../../ad4m-ldk/tests/golden_record_shape.json"),
            "utf8",
        ).trim();
        // The fixture file stores the canonical (key-sorted) spelling.
        expect(canonicalJson(json)).toBe(golden);
    });

    it("requires a source on a constructor call — 'this' does not exist yet", () => {
        @Model({
            name: "Bad",
            mount: true,
            constructorActions: { interaction: "create" },
        })
        class Bad {}
        expect(() => (Bad as any).generateSHACL()).toThrow(/constructor.*source|source.*constructor/i);
    });
});

describe("non-mount classes are unchanged", () => {
    @Model({ name: "Plain" })
    class Plain {
        @Property({ through: "plain://title", required: true, initial: "x" })
        title!: string;

        @HasMany({ through: "plain://item" })
        items!: string[];
    }

    it("keeps link actions: setSingleTarget setter, addLink/remover, constructor addLink", () => {
        const json = (Plain as any).generateSHACL().shape.toJSON();
        const props = Object.fromEntries(json.properties.map((p: any) => [p.name, p]));

        expect(props.title.setter).toEqual([
            { action: "setSingleTarget", source: "this", predicate: "plain://title", target: "value" },
        ]);
        expect(props.items.adder).toEqual([
            { action: "addLink", source: "this", predicate: "plain://item", target: "value" },
        ]);
        expect(props.items.remover).toEqual([
            { action: "removeLink", source: "this", predicate: "plain://item", target: "value" },
        ]);
        expect(json.constructor_actions).toEqual([
            { action: "addLink", source: "this", predicate: "plain://title", target: "x" },
        ]);
        expect(json.destructor_actions).toEqual([
            { action: "removeLink", source: "this", predicate: "plain://title", target: "*" },
        ]);
        // Read-only still declares writable: false outside mount mode.
        expect(props.items.writable).toBeUndefined();
    });
});

describe("declared call actions replace link actions per list", () => {
    @Model({ name: "Mixed", constructorActions: { interaction: "create", source: "lang://class/Mixed" } })
    class Mixed {
        @Property({ through: "mix://title", required: true, initial: "x" })
        title!: string;
    }

    it("declared constructor suppresses only the constructor list's link actions", () => {
        const json = (Mixed as any).generateSHACL().shape.toJSON();
        expect(json.constructor_actions).toEqual([
            { action: "call", source: "lang://class/Mixed", predicate: "create", target: "{}" },
        ]);
        // The destructor keeps its generated link writes — a call list
        // must stay homogeneous, and this one is all-link.
        expect(json.destructor_actions).toEqual([
            { action: "removeLink", source: "this", predicate: "mix://title", target: "*" },
        ]);
    });
});

describe("subjectConstructor stays a link list of its own", () => {
    const linkCtor = [
        { action: "addLink", source: "this", predicate: "leg://title", target: "x" },
    ];

    it("throws when a class sets both subjectConstructor and constructorActions", () => {
        @Model({
            name: "BothCtors",
            constructorActions: { interaction: "create", source: "lang://class/BothCtors" },
        })
        class BothCtors {
            @Property({ through: "both://title" })
            title!: string;
        }
        (BothCtors as any).prototype.subjectConstructor = linkCtor;
        expect(() => (BothCtors as any).generateSHACL()).toThrow(/subjectConstructor/);
    });

    it("throws when a mounted class sets subjectConstructor", () => {
        @Model({ name: "MountedCtor", mount: true, namespace: "mnt://" })
        class MountedCtor {
            @Property({ through: "mnt://title" })
            title!: string;
        }
        (MountedCtor as any).prototype.subjectConstructor = linkCtor;
        expect(() => (MountedCtor as any).generateSHACL()).toThrow(/subjectConstructor/);
    });

    it("still emits a lone subjectConstructor as the constructor list", () => {
        @Model({ name: "LegacyCtor", namespace: "leg://" })
        class LegacyCtor {
            @Property({ through: "leg://title" })
            title!: string;
        }
        (LegacyCtor as any).prototype.subjectConstructor = linkCtor;
        const json = (LegacyCtor as any).generateSHACL().shape.toJSON();
        expect(json.constructor_actions).toEqual(linkCtor);
    });
});
