//! The store slot of a perspective: deferred perspectives, swaps, and what every clone
//! sees. Hydration from a stored perspective is tested with the start-up sweep, in
//! `sparql_store.rs` (`a_deferred_perspective_opens_its_store_swept_on_hydration`).

use super::*;
use crate::agent::signatures::TestSigner;
use crate::perspectives::sparql_store::SparqlStore;
use crate::types::{Link, LinkExpression, LinkStatus};

fn handle(name: &str) -> PerspectiveHandle {
    PerspectiveHandle {
        uuid: uuid::Uuid::new_v4().to_string(),
        name: Some(name.to_string()),
        ..Default::default()
    }
}

fn link(signer: &TestSigner, target: &str) -> LinkExpression {
    let signed = signer.sign(
        Link {
            source: "ad4m://s".to_string(),
            predicate: Some("ad4m://p".to_string()),
            target: target.to_string(),
        }
        .normalize(),
    );
    LinkExpression {
        author: signed.author,
        timestamp: signed.timestamp,
        data: signed.data,
        proof: signed.proof,
        status: Some(LinkStatus::Shared),
    }
}

fn store_with(signer: &TestSigner, targets: &[&str]) -> SparqlStore {
    let store = SparqlStore::new(None).unwrap();
    for t in targets {
        store.add_link(&link(signer, t)).unwrap();
    }
    store
}

fn link_count(p: &PerspectiveInstance) -> usize {
    p.store().get_all_links().unwrap().len()
}

#[test]
fn a_deferred_perspective_is_unhydrated_over_an_empty_store() {
    let p = PerspectiveInstance::new_deferred(handle("deferred"));
    assert!(!p.is_hydrated());
    assert_eq!(link_count(&p), 0);
}

#[test]
fn a_new_perspective_is_hydrated() {
    let p = PerspectiveInstance::new(handle("eager"), None);
    assert!(p.is_hydrated());
}

/// A swap on one clone is what every clone reads.
#[test]
fn every_clone_shares_the_store_slot() {
    let signer = TestSigner::generate();
    let p = PerspectiveInstance::new_deferred(handle("clones"));
    let clone = p.clone();
    clone.swap_store(store_with(&signer, &["ad4m://t/1", "ad4m://t/2"]));
    assert!(p.is_hydrated());
    assert_eq!(link_count(&p), 2);
    assert!(Arc::ptr_eq(&p.store(), &clone.store()));
}

/// A reader keeps the store it loaded, whole, across a swap.
#[test]
fn a_reader_keeps_its_store_across_a_swap() {
    let signer = TestSigner::generate();
    let p = PerspectiveInstance::new_deferred(handle("reader"));
    p.swap_store(store_with(&signer, &["ad4m://t/1"]));
    let held = p.store();
    p.swap_store(store_with(
        &signer,
        &["ad4m://t/1", "ad4m://t/2", "ad4m://t/3"],
    ));
    assert_eq!(held.get_all_links().unwrap().len(), 1);
    assert_eq!(link_count(&p), 3);
}

/// Shapes read from the store a swap replaces are not served from the new one.
#[test]
fn a_swap_drops_the_cached_shapes() {
    let p = PerspectiveInstance::new_deferred(handle("shapes"));
    let generation = p.shapes_generation();
    p.swap_store(SparqlStore::new(None).unwrap());
    assert!(p.shape_cache.read().unwrap().is_empty());
    assert!(
        p.shapes_generation() > generation,
        "what was derived from the shapes is dropped too"
    );
}

/// `while_deferred` runs while the perspective is deferred, not after it hydrated.
#[test]
fn while_deferred_runs_only_before_hydration() {
    let p = PerspectiveInstance::new_deferred(handle("while deferred"));
    assert_eq!(p.while_deferred(|| 7), Some(7));
    p.swap_store(SparqlStore::new(None).unwrap());
    assert_eq!(p.while_deferred(|| 7), None);
}

/// Hydrating a hydrated perspective changes nothing.
#[tokio::test]
async fn hydrating_a_hydrated_perspective_keeps_its_store() {
    let p = PerspectiveInstance::new(handle("hydrated"), None);
    let before = p.store();
    p.hydrate().await.unwrap();
    assert!(Arc::ptr_eq(&before, &p.store()));
}
