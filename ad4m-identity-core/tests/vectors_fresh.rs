//! The committed vectors equal the ones the generator builds from its fixed inputs.
//! Regenerate after an intended change with
//! `AD4M_IDENTITY_WRITE_VECTORS=1 cargo test -p ad4m-identity-core --all-features --test vectors_fresh`.
#![cfg(all(feature = "build", feature = "words", not(target_arch = "wasm32")))]

mod common;

use std::path::PathBuf;

fn vector_path(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("vectors")
        .join(format!("{name}.json"))
}

#[test]
fn vectors_are_fresh() {
    let write = std::env::var_os("AD4M_IDENTITY_WRITE_VECTORS").is_some();
    let built = common::build_vectors();
    assert_eq!(
        built.iter().map(|(n, _)| *n).collect::<Vec<_>>(),
        common::VECTOR_NAMES
    );
    for (name, value) in built {
        let text = serde_json::to_string_pretty(&value).unwrap() + "\n";
        let path = vector_path(name);
        if write {
            std::fs::write(&path, &text).unwrap();
        } else {
            let committed = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("{}: {e}", path.display()));
            assert_eq!(
                committed, text,
                "{name}.json is stale: regenerate with AD4M_IDENTITY_WRITE_VECTORS=1"
            );
        }
    }
}
