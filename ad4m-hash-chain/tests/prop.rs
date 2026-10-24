//! Property: for random valid chains, `fold` of all entries equals `genesis` then `append`
//! one by one, and equals `import` in random (shuffled) batch sizes (build plan 5.18.9).
#![cfg(not(target_arch = "wasm32"))]

mod common;

use ad4m_hash_chain::{fold, import, Chain, ImportOutcome};
use common::*;
use proptest::prelude::*;

fn chains() -> impl Strategy<Value = (Vec<TestEntry>, Vec<usize>, u64)> {
    (
        1u8..200,
        prop::collection::vec(("[a-zA-Z0-9 éü€😀\"\\\\]{0,12}", any::<bool>()), 0..24),
    )
        .prop_flat_map(|(seed, steps)| {
            let bodies: Vec<String> = steps.iter().map(|(b, _)| b.clone()).collect();
            let rotate: Vec<bool> = steps.iter().map(|(_, r)| *r).collect();
            let entries = make_rotating_chain("prop", seed, &bodies, &rotate);
            let n = entries.len();
            (
                Just(entries),
                prop::collection::vec(1usize..6, n),
                any::<u64>(),
            )
        })
}

/// A deterministic shuffle from `seed` (xorshift), so a failing case replays.
fn shuffle<T>(items: &mut [T], mut seed: u64) {
    seed |= 1;
    for i in (1..items.len()).rev() {
        seed ^= seed << 13;
        seed ^= seed >> 7;
        seed ^= seed << 17;
        items.swap(i, (seed % (i as u64 + 1)) as usize);
    }
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(64))]

    #[test]
    fn fold_equals_append_one_by_one_equals_import_in_batches((entries, sizes, seed) in chains()) {
        let folded = fold(&TestPolicy, &entries).unwrap();

        let mut appended = Chain::genesis(&TestPolicy, &entries[0]).unwrap();
        for e in &entries[1..] {
            appended.append(&TestPolicy, e).unwrap();
        }
        prop_assert_eq!(chain_json(&appended), chain_json(&folded));

        let mut imported = Chain::genesis(&TestPolicy, &entries[0]).unwrap();
        let mut at = 1;
        for (round, size) in (0u64..).zip(sizes) {
            if at >= entries.len() {
                break;
            }
            let end = (at + size).min(entries.len());
            let mut batch = entries[at..end].to_vec();
            // Some batches repeat an entry we already hold.
            if round % 3 == 2 {
                batch.push(entries[at - 1].clone());
            }
            shuffle(&mut batch, seed.wrapping_add(round));
            let outcome = import(&mut imported, &TestPolicy, &batch).unwrap();
            prop_assert_eq!(
                outcome,
                ImportOutcome::Appended { head: imported.head(), count: (end - at) as u32 }
            );
            at = end;
        }
        prop_assert_eq!(chain_json(&imported), chain_json(&folded));
        prop_assert_eq!(import(&mut imported, &TestPolicy, &entries).unwrap(), ImportOutcome::AlreadyHave);
    }
}
