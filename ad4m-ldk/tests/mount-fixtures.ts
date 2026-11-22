/**
 * Mount-mode model classes for the decorator → mountShapes() tests.
 *
 * `MountThing` mirrors the hand-written SHACL of the executor's
 * `rust-executor/src/languages/fixtures/mount_test_language.js` fixture
 * field-for-field — the parity gate of `core/src/model/mount-classes.test.ts`
 * (jest) and `ad4m-ldk/js/tests/mount-classes.test.ts` (tsx).
 *
 * `GoldenRecord` exercises every property kind a mounted class can
 * declare — including the four parity edges round 2 added to the Rust
 * derive: an alias map, a `datatype` hasMany (`node_kind` Literal),
 * namespace inference (a relation declares before every property, yet
 * the namespace still comes from the first property's `through`), and
 * an ordinary writable property with no setter. Its canonical
 * `generateSHACL().shape.toJSON()` is the golden fixture
 * `ad4m-ldk/tests/golden_record_shape.json`, which the Rust
 * `#[derive(Ad4mModel)]` parity test produces as well — one class, one
 * JSON, two producers.
 *
 * These classes do not extend `Ad4mModel`: a mount needs only the shape,
 * and `Ad4mModel`'s perspective machinery would pull the client into a
 * language bundle (`@coasys/ad4m/model` stays client-free on purpose).
 *
 * The file lives here rather than in core/ or ad4m-ldk/js/ so that one
 * tsconfig (`ad4m-ldk/tsconfig.test.json`, TSX_TSCONFIG_PATH) scopes both
 * the LDK test sources and this file: tsx picks a file's tsconfig by the
 * tsconfig dir covering it, and only this directory shares an ancestor
 * with the LDK tests while staying importable from core's jest suite.
 */
import {
    Model,
    Optional,
    ReadOnly,
    Flag,
    HasOne,
    HasMany,
} from "../../core/src/model/decorators";

@Model({
    name: "MountThing",
    namespace: "mounttest://",
    targetClass: "mounttest://Thing",
    mount: true,
    constructorActions: { interaction: "create", source: "{language}://class/MountThing" },
    destructorActions: { interaction: "delete" },
})
export class MountThing {
    @Flag({ through: "rdf://type", value: "mounttest://Thing" })
    type!: string;

    @Optional({ through: "mounttest://title", setter: { interaction: "update" } })
    title!: string;

    @Optional({ through: "mounttest://done", setter: { interaction: "update" } })
    done!: boolean;

    @Optional({ through: "mounttest://tag", collection: true, setter: { interaction: "update" } })
    tags!: string[];
}

@Model({
    name: "GoldenRecord",
    // No `namespace`: it derives from the first entry of the merged
    // property map (`rdf://type` → `rdf://`). `members` declares before
    // every property but is a `@HasMany` — it registers no property
    // entry — so it never enters the namespace rule. The emitted shape
    // still lists properties before relations.
    targetClass: "mountgold://Record",
    mount: true,
    constructorActions: [
        { interaction: "create", source: "{language}://class/GoldenRecord" },
        { interaction: "index", source: "{language}://class/GoldenRecord", params: { kind: "record" } },
    ],
    destructorActions: { interaction: "delete" },
})
export class GoldenRecord {
    @HasMany({
        through: "mountgold://member",
        adder: { interaction: "add_member" },
        remover: { interaction: "remove_member", params: { soft: true } },
    })
    members!: unknown[];

    @Flag({ through: "rdf://type", value: "mountgold://Record" })
    type!: string;

    @Optional({ through: "mountgold://title", setter: { interaction: "update" } })
    title!: string;

    @Optional({ through: "mountgold://tag", collection: true, setter: { interaction: "update" } })
    tags!: string[];

    // An ordinary writable property — `writable: true` — carrying an
    // alias map for abstract-class conformance.
    @Optional({ through: "mountgold://note", alias: { TextContent: "text" } })
    note!: string;

    @ReadOnly({ through: "mountgold://created" })
    created!: string;

    @HasOne({ through: "mountgold://owner", readOnly: true })
    owner!: unknown;

    // A `datatype` relation holds literal values — `node_kind` Literal.
    @HasMany({ through: "mountgold://score", datatype: "xsd://integer" })
    scores!: number[];
}
