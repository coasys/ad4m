//! The identity of the social DNA a run was settled under, and what
//! counts as the end of a run.

use crate::perspectives::flow_evaluator::canonical_json;
use crate::perspectives::shacl_parser::SHACLFlow;
use serde_json::Value;
use sha2::{Digest, Sha256};
/// Content hash of a flow definition — the name of the social organism.
///
/// `hex(SHA256(canonical_json(flow)))` with `states` and `transitions` sorted
/// first. The sort is required, not tidiness: a `SHACLFlow` is reconstructed
/// from graph links on every read, and link enumeration order is a store
/// artefact two replicas may disagree on. An order-sensitive hash would make
/// the same DNA hash differently on different replicas, and every receipt
/// would look minted under foreign DNA.
///
/// States sort by `name` and transitions by `(from_state, to_state)`, each
/// with the element's own canonical JSON as the final tiebreaker — two
/// transitions may legitimately share `(from_state, to_state)` and differ in
/// `action_name`, and a partial sort key would leave their relative order to
/// the store again.
///
/// Everything else is scalars, `Option`s and author-ordered lists inside
/// states (`requires`) whose order the DNA author controls and which
/// round-trips deterministically: author-ordered content is identity-bearing
/// and stays unsorted.
pub fn flow_dna_hash(flow: &SHACLFlow) -> anyhow::Result<String> {
    let mut value = serde_json::to_value(flow)?;
    if let Some(states) = value.get_mut("states").and_then(Value::as_array_mut) {
        states.sort_by_cached_key(|s| (string_at(s, "name"), canonical_json(s)));
    }
    if let Some(transitions) = value.get_mut("transitions").and_then(Value::as_array_mut) {
        transitions.sort_by_cached_key(|t| {
            (
                string_at(t, "from_state"),
                string_at(t, "to_state"),
                canonical_json(t),
            )
        });
    }
    Ok(hex::encode(Sha256::digest(
        canonical_json(&value).as_bytes(),
    )))
}

fn string_at(value: &Value, key: &str) -> String {
    value
        .get(key)
        .and_then(Value::as_str)
        .unwrap_or_default()
        .to_string()
}

/// Has this state no outgoing transitions? Terminal states are where a run
/// ends, and only a run that ended gets a receipt.
///
/// Read off `transitions` directly rather than through
/// `flow_context::render::reachable_next_states`, which additionally requires
/// the target state to exist in `states`: a transition pointing at a state
/// the definition forgot to declare is a broken flow, and treating its source
/// as terminal would mint a completion claim for a run that is merely stuck.
pub fn is_terminal_state(flow: &SHACLFlow, state: &str) -> bool {
    !flow.transitions.iter().any(|t| t.from_state == state)
}

#[cfg(test)]
mod tests {
    use super::super::test_support::*;
    use super::*;
    /// A `SHACLFlow` is reconstructed from graph links on every read, and two
    /// replicas may enumerate those links in different orders. If that order
    /// reached the hash, the same DNA would hash differently per replica and
    /// every receipt would look minted under foreign DNA.
    ///
    /// Red without the two `sort_by_cached_key` calls in `flow_dna_hash`.
    #[test]
    fn dna_hash_ignores_the_order_states_and_transitions_arrive_in() {
        let states = |a: Value, b: Value| serde_json::json!([a, b]);
        let open = serde_json::json!({ "name": "open", "value": 0.0 });
        let done = serde_json::json!({ "name": "done", "value": 1.0 });
        let finish = serde_json::json!({ "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] });
        let reopen = serde_json::json!({ "action_name": "Reopen", "from_state": "done", "to_state": "open", "actions": [] });

        let one = flow_json(
            states(open.clone(), done.clone()),
            serde_json::json!([finish.clone(), reopen.clone()]),
        );
        let other = flow_json(states(done, open), serde_json::json!([reopen, finish]));

        assert_eq!(
            flow_dna_hash(&one).expect("hash"),
            flow_dna_hash(&other).expect("hash"),
            "the same DNA read in a different link order must have the same name"
        );
    }

    /// Two transitions may legitimately share `(from_state, to_state)` and
    /// differ in `action_name`. Sorting on the endpoints alone leaves their
    /// relative order to the store, because a stable sort keeps ties as they
    /// came in.
    ///
    /// Red with `canonical_json(t)` dropped from the transition sort key.
    #[test]
    fn dna_hash_breaks_ties_between_transitions_with_the_same_endpoints() {
        let ship = serde_json::json!({ "action_name": "Ship", "from_state": "open", "to_state": "done", "actions": [] });
        let cancel = serde_json::json!({ "action_name": "Cancel", "from_state": "open", "to_state": "done", "actions": [] });
        let states = serde_json::json!([
            { "name": "open", "value": 0.0 },
            { "name": "done", "value": 1.0 },
        ]);

        let one = flow_json(
            states.clone(),
            serde_json::json!([ship.clone(), cancel.clone()]),
        );
        let other = flow_json(states, serde_json::json!([cancel, ship]));

        assert_eq!(
            flow_dna_hash(&one).expect("hash"),
            flow_dna_hash(&other).expect("hash"),
            "two transitions with the same endpoints must not leave the hash to link order"
        );
    }

    /// Editing the DNA is editing the space's identity — the hash has to move
    /// with it, or the `DnaChanged` signal the verifier owes a reader could
    /// never fire.
    ///
    /// Red if `flow_dna_hash` digests only the flow's identity rather than its
    /// content — e.g. `Sha256::digest(flow.flow_uri())` instead of the
    /// canonical JSON of the whole (sorted) definition.
    #[test]
    fn dna_hash_changes_when_the_quorum_rule_changes() {
        let with_rule = |n: u32| {
            flow_json(
                serde_json::json!([
                    { "name": "open", "value": 0.0 },
                    { "name": "done", "value": 1.0, "consensusRule": { "n": n } },
                ]),
                serde_json::json!([
                    { "action_name": "Finish", "from_state": "open", "to_state": "done", "actions": [] },
                ]),
            )
        };
        assert_ne!(
            flow_dna_hash(&with_rule(1)).expect("hash"),
            flow_dna_hash(&with_rule(2)).expect("hash"),
            "a different quorum rule is different social DNA"
        );
    }
}
