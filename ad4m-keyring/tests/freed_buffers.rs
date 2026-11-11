//! No heap buffer the crate frees still holds a plaintext key.
//!
//! The test binary's allocator scans every buffer as it is freed for the watched keys. The
//! file holds one test, so no other test allocates while the watch is on.

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicBool, AtomicU8, AtomicUsize, Ordering};

use ad4m_keyring::{
    hkdf_subkey, open_from_x25519, seal_to_x25519, unwrap, wrap, x25519_public_of_ed25519,
    x25519_secret_of_ed25519, KeyRef, SecretKey, WrapContext,
};

const WATCHED: usize = 4;

struct Watching;

static ARMED: AtomicBool = AtomicBool::new(false);
static PATTERNS: [[AtomicU8; 32]; WATCHED] = [const { [const { AtomicU8::new(0) }; 32] }; WATCHED];
static IN_USE: AtomicUsize = AtomicUsize::new(0);
static HITS: AtomicUsize = AtomicUsize::new(0);

fn holds_a_watched_key(bytes: &[u8]) -> bool {
    let in_use = IN_USE.load(Ordering::SeqCst);
    PATTERNS[..in_use].iter().any(|pattern| {
        bytes.windows(32).any(|window| {
            window
                .iter()
                .zip(pattern.iter())
                .all(|(b, p)| *b == p.load(Ordering::Relaxed))
        })
    })
}

unsafe impl GlobalAlloc for Watching {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        System.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        if ARMED.load(Ordering::SeqCst) {
            let freed = std::slice::from_raw_parts(ptr, layout.size());
            if holds_a_watched_key(freed) {
                HITS.fetch_add(1, Ordering::SeqCst);
            }
        }
        System.dealloc(ptr, layout)
    }
}

#[global_allocator]
static ALLOCATOR: Watching = Watching;

fn watch(keys: &[&[u8; 32]]) {
    for (slot, key) in PATTERNS.iter().zip(keys) {
        for (cell, byte) in slot.iter().zip(key.iter()) {
            cell.store(*byte, Ordering::Relaxed);
        }
    }
    IN_USE.store(keys.len(), Ordering::SeqCst);
}

/// Runs `f` with the watch on and returns how many freed buffers held a watched key.
fn freed_copies(f: impl FnOnce()) -> usize {
    HITS.store(0, Ordering::SeqCst);
    ARMED.store(true, Ordering::SeqCst);
    f();
    ARMED.store(false, Ordering::SeqCst);
    HITS.load(Ordering::SeqCst)
}

#[test]
fn no_freed_buffer_holds_a_plaintext_key() {
    const DEK: [u8; 32] = *b"a data key, 32 bytes, recognised";
    const KEK: [u8; 32] = *b"the key that wraps the data key!";
    const ROOM: [u8; 32] = *b"a room key that is sealed to Bob";
    let keyring_id = [1u8; 16];
    let ctx = WrapContext {
        keyring_id: &keyring_id,
        wrapped: KeyRef::new("store", "executor", 1),
        wrapper: KeyRef::new("root", "", 1),
    };
    let bob_public = x25519_public_of_ed25519(
        &hex::decode("8a88e3dd7409f195fd52db2d3cba5d72ca6709bf1d94121bf3748801b40f6f5c")
            .unwrap()
            .try_into()
            .unwrap(),
    )
    .unwrap();
    let bob_secret = x25519_secret_of_ed25519(&[1u8; 32]);

    // The subkey's value, learnt before the watch so the watch knows what to look for.
    let subkey_value = *hkdf_subkey(
        &SecretKey::from_slice(&KEK).unwrap(),
        b"salt",
        "ad4m:test:v1",
    )
    .expose_secret();
    watch(&[&DEK, &KEK, &ROOM, &subkey_value]);

    // The watch catches a buffer freed without wiping.
    assert_eq!(
        freed_copies(|| drop(std::hint::black_box(DEK.to_vec()))),
        1,
        "the allocator does not see freed keys"
    );

    let hits = freed_copies(|| {
        let dek = SecretKey::from_slice(&DEK).unwrap();
        let kek = SecretKey::from_slice(&KEK).unwrap();
        let w = wrap(&dek, &kek, &ctx);
        let opened = unwrap(&w, &kek, &ctx).unwrap();
        assert_eq!(opened.expose_secret(), &DEK);
        let subkey = hkdf_subkey(&kek, b"salt", "ad4m:test:v1");
        assert_eq!(subkey.expose_secret(), &subkey_value);

        let room = SecretKey::from_slice(&ROOM).unwrap();
        let sealed = seal_to_x25519(&room, &bob_public).unwrap();
        let opened_room = open_from_x25519(&sealed, &bob_secret).unwrap();
        assert_eq!(opened_room.expose_secret(), &ROOM);
        // Moving a key moves a pointer: no copy is left behind to free.
        let moved = vec![opened, subkey, opened_room];
        drop(moved);
    });
    assert_eq!(hits, 0, "a freed buffer held a plaintext key");
}
