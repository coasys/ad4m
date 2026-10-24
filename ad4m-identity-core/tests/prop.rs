//! Property test (AR-01 8.1, R11, R12): for random valid logs, `key_valid_at` and
//! `key_valid_at_time` match a reference model built from the event list alone.
#![cfg(all(feature = "build", not(target_arch = "wasm32")))]

mod common;

use ad4m_identity_core::{Event, KeyLife, Op};
use common::*;
use proptest::prelude::*;
use std::collections::BTreeMap;

/// (signing key, added, revoked, until-expiry, bot scopes as (seq, expires))
type ModelKey = (String, u64, Option<u64>, Option<u64>, Vec<(u64, u64)>);

#[derive(Default, Debug)]
struct Model {
    /// key id → its model
    keys: BTreeMap<String, ModelKey>,
    masters: u32,
}

fn model(events: &[Event]) -> Model {
    let mut m = Model::default();
    for e in events {
        let s = e.seq;
        match &e.op {
            Op::Incept { master, add, .. } => {
                m.keys
                    .insert("m0".into(), (master.clone(), 0, None, None, vec![]));
                m.masters = 1;
                for k in add {
                    let until = match k.life {
                        KeyLife::Until(x) => Some(x),
                        _ => None,
                    };
                    m.keys
                        .insert(k.id.clone(), (k.sign.clone(), 0, None, until, vec![]));
                }
            }
            Op::Add { add, .. } => {
                let k = &add[0];
                let (until, scopes) = match &k.life {
                    KeyLife::Standing => (None, vec![]),
                    KeyLife::Until(x) => (Some(*x), vec![]),
                    KeyLife::Bot(b) => (None, vec![(s, b.expires)]),
                };
                m.keys
                    .insert(k.id.clone(), (k.sign.clone(), s, None, until, scopes));
            }
            Op::Revoke { revoke } => {
                for id in revoke {
                    m.keys.get_mut(id).unwrap().2 = Some(s);
                }
            }
            Op::Scope { key, expires, .. } => {
                m.keys.get_mut(key).unwrap().4.push((s, *expires));
            }
            Op::Rotate {
                master,
                next,
                revoke,
                ..
            } => {
                let old = format!("m{}", m.masters - 1);
                m.keys.get_mut(&old).unwrap().2 = Some(s);
                let new = format!("m{}", m.masters);
                m.masters += 1;
                m.keys.insert(new, (master.clone(), s, None, None, vec![]));
                for id in revoke {
                    m.keys.get_mut(id).unwrap().2 = Some(s);
                }
                if next.is_none() {
                    for k in m.keys.values_mut() {
                        k.2.get_or_insert(s);
                    }
                }
            }
        }
    }
    m
}

fn live_devices(log: &Log, bots_only: bool) -> Vec<String> {
    log.chain
        .state()
        .keys
        .values()
        .filter(|r| r.revoked.is_none() && (!bots_only || !r.scopes.is_empty()))
        .map(|r| r.id.clone())
        .collect()
}

fn build_log(steps: &[(u8, u8, u8)], end: bool) -> Log {
    let mut log = Log::new();
    for &(kind, a, b) in steps {
        match kind % 4 {
            0 => {
                let life = match b % 3 {
                    0 => KeyLife::Standing,
                    1 => KeyLife::Until(1_000 + u64::from(a) * 10),
                    _ => bot(&["read"], 1_000 + u64::from(a) * 10),
                };
                log.add(life);
            }
            1 => {
                let live = live_devices(&log, false);
                if !live.is_empty() {
                    let id = &live[usize::from(a) % live.len()];
                    log.revoke(&[id]);
                }
            }
            2 => {
                let live = live_devices(&log, false);
                if b % 2 == 1 && !live.is_empty() {
                    let id = live[usize::from(a) % live.len()].clone();
                    log.rotate(&[&id], false);
                } else {
                    log.rotate(&[], false);
                }
            }
            _ => {
                let bots = live_devices(&log, true);
                if !bots.is_empty() {
                    let id = bots[usize::from(a) % bots.len()].clone();
                    log.scope(&id, &["write"], 1_000 + u64::from(b) * 10);
                }
            }
        }
    }
    if end {
        log.rotate(&[], true);
    }
    log
}

proptest! {
    #![proptest_config(ProptestConfig::with_cases(96))]

    #[test]
    fn key_validity_matches_the_reference_model(
        steps in prop::collection::vec((any::<u8>(), any::<u8>(), any::<u8>()), 0..14),
        end in any::<bool>(),
        now in 900u64..4_000,
    ) {
        let log = build_log(&steps, end);
        // Replaying the JSON gives the same chain.
        let replayed = ad4m_identity_core::fold_json(&log.json()).unwrap();
        prop_assert_eq!(replayed.state(), log.chain.state());
        let state = log.chain.state();
        let m = model(&log.events);
        prop_assert_eq!(state.ended, end);
        let last = log.seq() + 1;
        for (id, (sign, added, revoked, until, scopes)) in &m.keys {
            for s in 0..=last {
                let want = *added <= s && revoked.is_none_or(|r| s < r);
                prop_assert_eq!(state.key_valid_at(id, s), want, "{} at {}", id, s);
                prop_assert_eq!(state.key_valid_at(sign, s), want, "{} by key at {}", id, s);
                let in_time = until.is_none_or(|u| now < u)
                    && (scopes.is_empty()
                        || scopes.iter().rev().find(|(at, _)| *at <= s).is_some_and(|(_, e)| now < *e));
                prop_assert_eq!(state.key_valid_at_time(id, s, now), want && in_time, "{} at {} t {}", id, s, now);
            }
        }
        // Nothing outside the model is valid.
        prop_assert!(!state.key_valid_at("d99", 0));
        prop_assert_eq!(state.masters.len() as u32, m.masters);
    }
}
