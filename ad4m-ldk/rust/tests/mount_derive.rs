//! `#[derive(Ad4mModel)]` parity: `GoldenRecord` here is the same
//! mounted class as `ad4m-ldk/tests/mount-fixtures.ts`, and its
//! `generate_shacl().shape` must equal the shared golden
//! `ad4m-ldk/tests/golden_record_shape.json` — the canonical
//! `generateSHACL().shape.toJSON()` the TS decorators emit. One class,
//! one JSON, two producers.

use ad4m_ldk::prelude::*;
use ad4m_ldk::types::{MountShapes, MountSnapshot};

/// Same class as the `GoldenRecord` in `ad4m-ldk/tests/mount-fixtures.ts`:
/// flag, optional literal, collection, read-only, `hasOne`, `hasMany`
/// with call adder/remover, call setter, call constructor/destructor.
#[derive(Ad4mModel)]
#[ad4m(
    name = "GoldenRecord",
    namespace = "mountgold://",
    target_class = "mountgold://Record",
    constructor = call("create", source = "{language}://class/GoldenRecord"),
    constructor = call("index", source = "{language}://class/GoldenRecord", params = "{\"kind\":\"record\"}"),
    destructor = call("delete"),
)]
struct GoldenRecord {
    #[ad4m_property(through = "rdf://type", flag = "mountgold://Record")]
    r#type: String,
    #[ad4m_property(through = "mountgold://title", setter = call("update"))]
    title: String,
    #[ad4m_property(through = "mountgold://tag", collection, setter = call("update"))]
    tags: Vec<String>,
    #[ad4m_property(through = "mountgold://created", read_only)]
    created: String,
    #[ad4m_relation(kind = one, through = "mountgold://owner", read_only)]
    owner: String,
    #[ad4m_relation(
        kind = many,
        through = "mountgold://member",
        adder = call("add_member"),
        remover = call("remove_member", params = "{\"soft\":true}"),
    )]
    members: Vec<String>,
    /// Plain state field — carries no `ad4m_*` attribute, so the shape
    /// never sees it.
    #[allow(dead_code)]
    scratch: String,
}

#[test]
fn derived_shape_equals_ts_golden() {
    let gen = GoldenRecord::generate_shacl();
    assert_eq!(gen.name, "GoldenRecord");
    let golden: serde_json::Value =
        serde_json::from_str(include_str!("../../tests/golden_record_shape.json")).unwrap();
    assert_eq!(gen.shape, golden);
}

/// The `mount.classes` envelope: `node_shape_uri` drops (the executor
/// derives it from `target_class`) and `{language}` resolves against the
/// language's own address — `from_classes_at` with an explicit address,
/// since `from_classes`'s host import only exists inside the runtime.
#[test]
fn mount_envelope_resolves_language() {
    let shapes = MountShapes::from_classes_at("1", &[GoldenRecord::generate_shacl()], "lang://abc");
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
            &[GoldenRecord::generate_shacl()],
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
