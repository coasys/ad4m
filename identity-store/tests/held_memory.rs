//! What a held fold costs in memory (`src/held.rs`, `HELD_LOG_BYTES`): the live heap of a folded
//! log (`KelChain`, what the store holds), measured with a counting allocator, against the
//! estimate `held_bytes_of` the bound counts. The logs: many scopes, many rotations, and the two
//! single events that cost the most (an incept listing many keys, a scope naming an action many
//! times). `identity-store/AGENTS.md` states the numbers; run with `--nocapture` to see them.
//!
//! One test in this binary: the allocator counts every thread.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicIsize, Ordering};

use ad4m_hash_chain::{encode_ed25519_multikey, encode_proof_value, sha256};
use ad4m_identity_core::build::{self, Signer};
use ad4m_identity_core::{
    commitment, encode_x25519_multikey, event_hash, fold, BotScope, DeviceKey, Event, KeyLife, Op,
    BOT_ACTIONS,
};
use ed25519_dalek::SigningKey;
use identity_store::held::held_bytes_of;

struct Counting;

static LIVE: AtomicIsize = AtomicIsize::new(0);

unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        LIVE.fetch_add(layout.size() as isize, Ordering::SeqCst);
        unsafe { System.alloc(layout) }
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        LIVE.fetch_sub(layout.size() as isize, Ordering::SeqCst);
        unsafe { System.dealloc(ptr, layout) }
    }

    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        LIVE.fetch_add(new_size as isize - layout.size() as isize, Ordering::SeqCst);
        unsafe { System.realloc(ptr, layout, new_size) }
    }
}

#[global_allocator]
static ALLOCATOR: Counting = Counting;

/// The most heap a rotation takes in a held fold: what `identity-store/AGENTS.md` states.
const MAX_HELD_BYTES_PER_EVENT: usize = 512;

fn key(label: &str) -> SigningKey {
    SigningKey::from_bytes(&sha256(label.as_bytes()))
}

fn multikey(key: &SigningKey) -> String {
    encode_ed25519_multikey(key.verifying_key().as_bytes())
}

fn salt(n: usize) -> String {
    bs58::encode(&sha256(format!("salt {n}").as_bytes())[..16]).into_string()
}

/// Event #0 (master m0, one standing device key d1) and the `add` of a bot key d2 with a scope
/// of every action (an incept lists no bot key).
fn incept() -> Vec<Event> {
    let device = key("device");
    let standing = DeviceKey {
        id: "d1".into(),
        sign: multikey(&device),
        enc: enc_of(&device),
        life: KeyLife::Standing,
    };
    let next = commitment(&multikey(&key("master 1")), &salt(1));
    let mut first = build::incept_human(&key("master 0"), next, vec![standing]).unwrap();
    build::cosign(&mut first, "d1", &device).unwrap();
    let bot = key("bot");
    let op = Op::Add {
        add: vec![DeviceKey {
            id: "d2".into(),
            sign: multikey(&bot),
            enc: enc_of(&bot),
            life: KeyLife::Bot(BotScope {
                can: BOT_ACTIONS.iter().map(|a| a.to_string()).collect(),
                expires: 2_000_000_000,
            }),
        }],
        offer: bs58::encode([1u8; 16]).into_string(),
        co_sig: encode_proof_value(&Signer::sign(&bot, b"claim")),
    };
    let add = next_event(&first, op, "m0", &key("master 0"));
    vec![first, add]
}

fn enc_of(key: &SigningKey) -> String {
    encode_x25519_multikey(&key.verifying_key().to_montgomery().to_bytes())
}

/// The event after `prev` with `op`, signed by `signer` as `key_id` (built without folding, so
/// a long log builds in linear time).
fn next_event(prev: &Event, op: Op, key_id: &str, signer: &dyn Signer) -> Event {
    let mut event = Event {
        format: prev.format.clone(),
        scid: prev.scid.clone(),
        seq: prev.seq + 1,
        prev: Some(event_hash(prev).unwrap()),
        op,
        proof: Vec::new(),
    };
    build::cosign(&mut event, key_id, signer).unwrap();
    event
}

/// #0, the bot's `add` and `n` scope events re-scoping the bot d2, each naming every action.
fn scope_log(n: usize) -> Vec<Event> {
    let master = key("master 0");
    let mut events = incept();
    for i in 0..n {
        let op = Op::Scope {
            key: "d2".into(),
            can: BOT_ACTIONS.iter().map(|a| a.to_string()).collect(),
            expires: 2_000_000_000 + i as u64,
        };
        let event = next_event(events.last().unwrap(), op, "m0", &master);
        events.push(event);
    }
    events
}

/// #0, the bot's `add` and `n` rotations, each onto the committed next master.
fn rotation_log(n: usize) -> Vec<Event> {
    let mut events = incept();
    for i in 1..=n {
        let master = key(&format!("master {i}"));
        let op = Op::Rotate {
            master: multikey(&master),
            salt: salt(i),
            next: Some(commitment(
                &multikey(&key(&format!("master {}", i + 1))),
                &salt(i + 1),
            )),
            revoke: Vec::new(),
        };
        let event = next_event(events.last().unwrap(), op, &format!("m{i}"), &master);
        events.push(event);
    }
    events
}

/// The live heap a fold of `events` keeps, and what `held_bytes_of` makes of it.
fn measure(events: &[Event]) -> (usize, usize) {
    let before = LIVE.load(Ordering::SeqCst);
    let chain = fold(events).unwrap_or_else(|e| panic!("the log does not fold: {e}"));
    let after = LIVE.load(Ordering::SeqCst);
    assert_eq!(chain.head().seq as usize + 1, events.len());
    let estimate = held_bytes_of(&chain);
    drop(chain);
    (usize::try_from(after - before).unwrap(), estimate)
}

/// Event #0 listing `n` standing device keys, each co-signing: one event with a wide state.
fn wide_incept(n: usize) -> Vec<Event> {
    let devices: Vec<SigningKey> = (0..n).map(|i| key(&format!("device {i}"))).collect();
    let listed = devices
        .iter()
        .enumerate()
        .map(|(i, d)| DeviceKey {
            id: format!("d{}", i + 1),
            sign: multikey(d),
            enc: enc_of(d),
            life: KeyLife::Standing,
        })
        .collect();
    let next = commitment(&multikey(&key("master 1")), &salt(1));
    let mut first = build::incept_human(&key("master 0"), next, listed).unwrap();
    for (i, d) in devices.iter().enumerate() {
        build::cosign(&mut first, &format!("d{}", i + 1), d).unwrap();
    }
    vec![first]
}

/// #0, the bot's `add` and one `scope` naming one action `n` times.
fn long_scope(n: usize) -> Vec<Event> {
    let mut events = incept();
    let op = Op::Scope {
        key: "d2".into(),
        can: vec![BOT_ACTIONS[0].to_string(); n],
        expires: 2_000_000_000,
    };
    let event = next_event(events.last().unwrap(), op, "m0", &key("master 0"));
    events.push(event);
    events
}

#[test]
fn the_held_size_estimate_covers_the_heap_a_fold_takes() {
    let logs = [
        ("1 024 scopes", scope_log(1024)),
        ("4 096 scopes", scope_log(4096)),
        ("1 024 rotations", rotation_log(1024)),
        ("4 096 rotations", rotation_log(4096)),
        ("an incept of 150 keys", wide_incept(150)),
        ("a scope naming an action 2 000 times", long_scope(2000)),
    ];
    for (name, events) in &logs {
        let (heap, estimate) = measure(events);
        eprintln!(
            "{name}: {} events, heap {heap} B ({} B/event), estimate {estimate} B",
            events.len(),
            heap / events.len()
        );
        assert!(
            estimate >= heap,
            "{name}: estimate {estimate} < heap {heap}"
        );
        assert!(
            estimate <= 2 * heap,
            "{name}: estimate {estimate} > 2 × heap {heap}"
        );
    }
    // Per event, rotations cost the most of the common kinds: under 512 B of heap each.
    let (heap, _) = measure(&rotation_log(4096));
    assert!(heap / 4098 <= MAX_HELD_BYTES_PER_EVENT);
}
