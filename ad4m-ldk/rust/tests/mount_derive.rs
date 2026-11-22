//! `#[derive(Ad4mModel)]` parity: `GoldenRecord` here is the same
//! mounted class as `ad4m-ldk/tests/mount-fixtures.ts`, and its
//! `generate_shacl().shape` must equal the shared golden
//! `ad4m-ldk/tests/golden_record_shape.json` — the canonical
//! `generateSHACL().shape.toJSON()` the TS decorators emit. One class,
//! one JSON, two producers.

use ad4m_ldk::prelude::*;
use ad4m_ldk::types::{MountShapes, MountSnapshot};

/// Same class as the `GoldenRecord` in `ad4m-ldk/tests/mount-fixtures.ts`:
/// flag, optional literal, collection, an ordinary writable property
/// with an alias map, read-only, `hasOne`, `hasMany` with call
/// adder/remover, a `datatype` hasMany (`node_kind` Literal), call
/// setter, call constructor/destructor. `members` declares before every
/// property, yet the namespace still derives from the first property
/// entry (`rdf://type` → `rdf://`) and the shape still lists properties
/// before relations. `T` occurs only in the undecorated `scratch`
/// field — the derive's `split_for_impl` keeps the generated `impl`
/// generic, which this golden test also proves.
#[allow(dead_code)] // fields carry the shape only — never read in Rust
#[derive(Ad4mModel)]
#[ad4m(
    name = "GoldenRecord",
    target_class = "mountgold://Record",
    constructor = call("create", source = "{language}://class/GoldenRecord"),
    constructor = call("index", source = "{language}://class/GoldenRecord", params = "{\"kind\":\"record\"}"),
    destructor = call("delete"),
)]
struct GoldenRecord<T> {
    /// Declared first — a `kind = many` relation, so its `through`
    /// never enters the namespace rule.
    #[ad4m_relation(
        kind = many,
        through = "mountgold://member",
        adder = call("add_member"),
        remover = call("remove_member", params = "{\"soft\":true}"),
    )]
    members: Vec<String>,
    #[ad4m_property(through = "rdf://type", flag = "mountgold://Record")]
    r#type: String,
    #[ad4m_property(through = "mountgold://title", setter = call("update"))]
    title: String,
    #[ad4m_property(through = "mountgold://tag", collection, setter = call("update"))]
    tags: Vec<String>,
    #[ad4m_property(through = "mountgold://note", alias(TextContent = "text"))]
    note: String,
    #[ad4m_property(through = "mountgold://created", read_only)]
    created: String,
    #[ad4m_relation(kind = one, through = "mountgold://owner", read_only)]
    owner: String,
    /// A `datatype` relation holds literal values — `node_kind` Literal.
    #[ad4m_relation(kind = many, through = "mountgold://score", datatype = "xsd://integer")]
    scores: Vec<u32>,
    /// `T` occurs only here — a plain state field carries no `ad4m_*`
    /// attribute, so the shape never sees it.
    scratch: T,
}

#[test]
fn derived_shape_equals_ts_golden() {
    let gen = GoldenRecord::<()>::generate_shacl();
    assert_eq!(gen.name, "GoldenRecord");
    let golden: serde_json::Value =
        serde_json::from_str(include_str!("../../tests/golden_record_shape.json")).unwrap();
    assert_eq!(gen.shape, golden);
}

/// A `kind = one` relation registers a property shadow in TS (a
/// `HasOne` reads through its predicate as a property), so a HasOne
/// declared first still feeds the namespace — the merged property
/// map's first entry is its shadow, measured on the TS side.
#[allow(dead_code)]
#[derive(Ad4mModel)]
#[ad4m(name = "Shadow", target_class = "shadow://S")]
struct Shadow {
    #[ad4m_relation(kind = one, through = "shadowrel://owner", read_only)]
    owner: String,
    #[ad4m_property(through = "propns://title")]
    title: String,
}

#[test]
fn namespace_derives_from_first_property_or_hasone_through() {
    let gen = Shadow::generate_shacl();
    assert_eq!(gen.shape["node_shape_uri"], "shadowrel://ShadowShape");
}

/// No `through` scheme at all — the TS `ad4m://` fallback.
#[allow(dead_code)]
#[derive(Ad4mModel)]
#[ad4m(name = "Fallback")]
struct Fallback {
    #[ad4m_property(through = "literal")]
    title: String,
}

#[test]
fn namespace_falls_back_to_ad4m_scheme() {
    let gen = Fallback::generate_shacl();
    assert_eq!(gen.shape["node_shape_uri"], "ad4m://FallbackShape");
    assert_eq!(gen.shape["target_class"], "ad4m://Fallback");
}

/// The `mount.classes` envelope: `node_shape_uri` drops (the executor
/// derives it from `target_class`) and `{language}` resolves against the
/// language's own address — `from_classes_at` with an explicit address,
/// since `from_classes`'s host import only exists inside the runtime.
#[test]
fn mount_envelope_resolves_language() {
    let shapes =
        MountShapes::from_classes_at("1", &[GoldenRecord::<()>::generate_shacl()], "lang://abc");
    assert_eq!(shapes.version, "1");
    assert_eq!(shapes.shapes.len(), 1);
    let entry = &shapes.shapes[0];
    assert_eq!(entry.name, "GoldenRecord");
    let shape = &entry.shape;
    assert!(shape.get("node_shape_uri").is_none());
    assert_eq!(shape["target_class"], "mountgold://Record");
    let ctors = shape["constructor_actions"].as_array().unwrap();
    assert_eq!(ctors[0]["source"], "lang://abc://class/GoldenRecord");
    assert_eq!(ctors[0]["action"], "call");
    // `this` stays `this`; params text keeps its JSON.
    assert_eq!(shape["destructor_actions"][0]["source"], "this");
    assert_eq!(shape["destructor_actions"][0]["target"], "{}");
    let member = shape["properties"]
        .as_array()
        .unwrap()
        .iter()
        .find(|p| p["name"] == "members")
        .unwrap();
    assert_eq!(member["remover"][0]["target"], "{\"soft\":true}");
}

/// The placeholder walker is deliberately narrow — like the JS
/// `resolveMountCallSources`, only a call action's `source`/`target`
/// substitutes `{language}`; a `name`, `path` or `has_value` keeps it
/// verbatim.
#[allow(dead_code)]
#[derive(Ad4mModel)]
#[ad4m(
    name = "Keep",
    namespace = "keep://",
    target_class = "keep://Keep",
    constructor = call("create", source = "{language}://class/Keep"),
)]
struct Keep {
    #[ad4m_property(
        through = "keep://data/{language}",
        flag = "keep://flag-{language}",
        setter = call("update", source = "{language}://act"),
    )]
    p: String,
}

#[test]
fn mount_envelope_resolves_language_only_in_call_actions() {
    let shapes = MountShapes::from_classes_at("1", &[Keep::generate_shacl()], "lang://abc");
    let prop = &shapes.shapes[0].shape["properties"][0];
    assert_eq!(prop["path"], "keep://data/{language}");
    assert_eq!(prop["has_value"], "keep://flag-{language}");
    assert_eq!(prop["setter"][0]["source"], "lang://abc://act");
    let ctor = &shapes.shapes[0].shape["constructor_actions"][0];
    assert_eq!(ctor["source"], "lang://abc://class/Keep");
}

/// `capabilities: [mount]` on `ad4m_language!` — compile coverage for
/// the `mountShapes`/`mountSnapshot` shim arm.
struct MountLang;

impl Language for MountLang {
    fn name() -> &'static str {
        "mount-test"
    }
    fn version() -> &'static str {
        "0.0.1"
    }
    async fn init() -> LanguageResult<Self> {
        Ok(Self)
    }
}

impl MountCapability for MountLang {
    fn mount_shapes(&mut self) -> LanguageResult<MountShapes> {
        Ok(MountShapes::from_classes_at(
            "1",
            &[GoldenRecord::<()>::generate_shacl()],
            "lang://abc",
        ))
    }
    async fn mount_snapshot(&mut self) -> LanguageResult<MountSnapshot> {
        Ok(MountSnapshot {
            entries: Vec::new(),
        })
    }
}

ad4m_language! { language: MountLang, capabilities: [mount] }
