//! What an append costs the heap on a large held log: the batch steps on one copy of the held
//! fold (`Chain::appended`), so an append of 32 events allocates about one fold, whether it is
//! taken or fails at its last event, never one or two per event. Measured with a counting
//! allocator (every byte allocated, freed or not) on a log padded with wide scopes.
//!
//! One test in this binary: the allocator counts every thread.

mod common;

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

use ad4m_identity_core::build;
use ad4m_identity_core::link::OfferKind;
use ad4m_identity_core::{event_hash, BotScope, Event, Op};
use axum::http::StatusCode;
use identity_store::held::held_bytes_of;
use identity_store::log::MAX_EVENTS;
use identity_store::routes::MAX_BODY;
use identity_store::Settings;
use serde_json::json;

use common::agent::{Device, Person};
use common::TestStore;

struct Counting;

/// Every byte allocated since the start, freed or not.
static ALLOCATED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOCATED.fetch_add(layout.size(), Ordering::SeqCst);
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        ALLOCATED.fetch_add(new_size.saturating_sub(layout.size()), Ordering::SeqCst);
        unsafe { System.realloc(ptr, layout, new_size) }
    }
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// What an append may allocate besides its one copy of the fold: the request, its 32 small
/// events, the transaction and the answer.
const OVERHEAD: usize = 1 << 20;

/// The event after `prev` with `op`, signed as `key_id` (`None`: unsigned).
fn next_event(prev: &Event, op: Op, signer: Option<(&str, &ed25519_dalek::SigningKey)>) -> Event {
    let mut event = Event {
        format: prev.format.clone(),
        scid: prev.scid.clone(),
        seq: prev.seq + 1,
        prev: Some(event_hash(prev).unwrap()),
        op,
        proof: Vec::new(),
    };
    if let Some((key_id, key)) = signer {
        build::cosign(&mut event, key_id, key).unwrap();
    }
    event
}

#[tokio::test]
async fn an_append_on_a_held_log_copies_its_fold_once_whether_taken_or_refused_late() {
    let t = TestStore::with_settings(Settings {
        pow_bits: 8,
        log_fold_bytes: 8 << 20,
        ..Settings::default()
    });
    let mut alice = Person::new("alice");
    t.publish(&alice).await;
    let bot = Device::new("alice bot");
    let scope = BotScope {
        can: vec!["read".into()],
        expires: t.now() + 86_400,
    };
    let bot_id = t.link(&mut alice, &bot, OfferKind::Bot, Some(scope)).await;
    let scope_op = |repeats: usize| Op::Scope {
        key: bot_id.clone(),
        can: vec!["read".to_string(); repeats],
        expires: t.now() + 86_400,
    };
    // Eight scopes of one full body each: a fold of some megabytes, held.
    let master = alice.master(0);
    let mut repeats = 9_400;
    for _ in 0..8 {
        let event = loop {
            let event = next_event(
                alice.events.last().unwrap(),
                scope_op(repeats),
                Some(("m0", &master)),
            );
            if serde_json::to_vec(&json!({ "events": [&event] }))
                .unwrap()
                .len()
                <= MAX_BODY
            {
                break event;
            }
            repeats -= 50;
        };
        let reply = t.append_push(&mut alice, event).await;
        assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    }
    assert!(t.store.held.holds(&alice.scid()));
    let fold = held_bytes_of(&alice.chain);
    assert!(fold > 2 << 20, "{fold}");

    // 31 small scopes, then an event without a proof: refused at its last event.
    let mut batch: Vec<Event> = Vec::new();
    for _ in 0..MAX_EVENTS - 1 {
        let prev = batch.last().unwrap_or(alice.events.last().unwrap());
        batch.push(next_event(prev, scope_op(1), Some(("m0", &master))));
    }
    let revoke = Op::Revoke {
        revoke: vec!["d1".into()],
    };
    batch.push(next_event(batch.last().unwrap(), revoke, None));
    let refs: Vec<_> = batch.iter().collect();
    let folds = t.store.held.folds();
    let start = ALLOCATED.load(Ordering::SeqCst);
    let reply = t.append(&alice.scid(), &refs).await;
    let refused = ALLOCATED.load(Ordering::SeqCst) - start;
    assert_eq!(reply.status, StatusCode::BAD_REQUEST, "{}", reply.body);
    assert_eq!(reply.body["error"], "MissingProof", "{}", reply.body);
    eprintln!("a fold of {fold} B; a batch of 32 refused at its last event allocated {refused} B");
    assert!(
        refused <= fold + OVERHEAD,
        "{refused} B for a fold of {fold} B"
    );

    // The 31 scopes alone: taken, for the same one copy.
    let refs: Vec<_> = batch[..MAX_EVENTS - 1].iter().collect();
    let start = ALLOCATED.load(Ordering::SeqCst);
    let reply = t.append(&alice.scid(), &refs).await;
    let taken = ALLOCATED.load(Ordering::SeqCst) - start;
    assert_eq!(reply.status, StatusCode::OK, "{}", reply.body);
    eprintln!("a batch of 31 taken allocated {taken} B");
    assert!(taken <= fold + OVERHEAD, "{taken} B for a fold of {fold} B");
    assert_eq!(t.store.held.folds(), folds);
}
