//! The membership replay ([`MembershipGraph`]): every vector under `vectors/membership/`, applied
//! link by link in the file's order and replayed at once, and the replay's independence of the
//! order of its input.
//!
//! A vector file holds `creator`, `steps` (each a judged `link` with the `changes` its `apply`
//! returns or the refusal it is `refused` with), then what the graph holds after the last step:
//! `rules`, `join`, `members` and `admits` (a self-join at `now` and its refusal, `null` when
//! it counts). Replaying the step links that were not backdated must give the same graph.
//! `tests/membership_wasm.rs` runs the same vectors through the WASM export.
#![cfg(not(target_arch = "wasm32"))]

use ad4m_identity_core::judgement::LinkVerdict;
use ad4m_identity_core::membership::{predicate, JudgedLink, MembershipGraph, Refusal};
use serde_json::{json, Value};

fn graph_json(graph: &MembershipGraph) -> Value {
    json!({"rules": graph.rules(), "join": graph.join(), "members": graph.members()})
}

fn admits_json(graph: &MembershipGraph, query: &Value) -> Value {
    let result = graph.admits(
        query["person"].as_str(),
        query["byBot"].as_bool().unwrap_or(false),
        query["invite"].as_str(),
        query["now"].as_u64().unwrap(),
    );
    match result {
        Ok(()) => Value::Null,
        Err(refusal) => json!(refusal),
    }
}

fn check(name: &str, text: &str) {
    let file: Value = serde_json::from_str(text).unwrap();
    let creator = file["creator"].as_str().unwrap();
    let mut graph = MembershipGraph::new(creator);
    let mut kept = Vec::new();
    for (i, step) in file["steps"].as_array().unwrap().iter().enumerate() {
        let link: JudgedLink = serde_json::from_value(step["link"].clone()).unwrap();
        let (got, want) = match graph.apply(&link) {
            Ok(changes) => (
                json!({ "changes": changes }),
                json!({"changes": step["changes"]}),
            ),
            Err(refusal) => (
                json!({ "refused": refusal }),
                json!({"refused": step["refused"]}),
            ),
        };
        assert_eq!(got, want, "{name}: step {i}");
        if got["refused"] != json!(Refusal::Backdated) {
            kept.push(link);
        }
    }
    let want = json!({"rules": file["rules"], "join": file["join"], "members": file["members"]});
    assert_eq!(graph_json(&graph), want, "{name}: after the steps");
    for query in file["admits"].as_array().unwrap() {
        assert_eq!(
            admits_json(&graph, query),
            query["refused"],
            "{name}: {query}"
        );
    }
    let (replayed, _) = MembershipGraph::replay(creator, &kept);
    assert_eq!(graph_json(&replayed), want, "{name}: replayed");
    for query in file["admits"].as_array().unwrap() {
        assert_eq!(
            admits_json(&replayed, query),
            query["refused"],
            "{name}: replayed {query}"
        );
    }
}

macro_rules! vectors {
    ($($test:ident => $file:literal),* $(,)?) => {
        $(
            #[test]
            fn $test() {
                check($file, include_str!(concat!("../vectors/membership/", $file, ".json")));
            }
        )*
        const FILES: &[&str] = &[$($file),*];
    };
}

vectors! {
    only_the_creators_first_rules_link_counts => "rules-creator-first",
    rules_without_join_read_as_invite => "rules-without-join",
    only_an_admin_changes_the_join_mode => "join-by-admin",
    a_self_join_counts_in_an_open_space => "self-join-open",
    a_self_join_needs_a_persons_own_key => "needs-identity",
    a_self_join_counts_with_an_unused_unexpired_invite => "self-join-invite",
    an_admin_or_the_person_removes => "removed-by",
    an_admin_removal_bars_an_open_rejoin => "admin-removal-bars-open",
    a_person_who_left_rejoins_an_open_space => "self-removal-readmits",
    a_link_backdated_before_its_authors_removal_counts_for_nothing => "backdated",
    a_bot_counts_with_its_bot_and_approved_by_links => "bot",
    a_link_whose_verdict_does_not_count_counts_for_nothing => "not-counted",
    a_link_that_arrives_out_of_order_replays_the_graph => "out-of-order",
    rows_that_do_not_fit_are_malformed_and_others_are_not_read => "shapes",
}

#[test]
fn every_vector_file_has_a_test() {
    let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/vectors/membership");
    let mut files: Vec<String> = std::fs::read_dir(dir)
        .unwrap()
        .map(|entry| entry.unwrap().file_name().into_string().unwrap())
        .collect();
    files.sort();
    let mut tested: Vec<String> = FILES.iter().map(|f| format!("{f}.json")).collect();
    tested.sort();
    assert_eq!(files, tested);
}

/// The people, keys and nodes the generated links draw from.
mod arbitrary {
    use super::*;
    use proptest::prelude::*;
    use sha2::{Digest, Sha256};

    pub const CREATOR: &str = "did:scid:coasys:1:Ada";
    pub const PEOPLE: [&str; 3] = [CREATOR, "did:scid:coasys:1:Bea", "did:scid:coasys:1:Cid"];
    pub const BOT: &str = "did:key:zBot";
    pub const INVITES: [&str; 2] = ["invite://one", "invite://two"];
    const SPACE: &str = ad4m_identity_core::membership::SPACE;
    const OPEN_RULES: &str =
        "literal:json:%7B%22add%22%3A%22members%22%2C%22guests%22%3A%22no%22%2C%22join%22%3A%22open%22%7D";
    const INVITE_RULES: &str = "literal:json:%7B%22add%22%3A%22admins%22%7D";

    /// One membership link of any row, by any of the people or the bot, with any verdict.
    fn link() -> impl Strategy<Value = JudgedLink> {
        let row = prop_oneof![
            Just((SPACE, predicate::RULES, OPEN_RULES)),
            Just((SPACE, predicate::RULES, INVITE_RULES)),
            Just((SPACE, predicate::JOIN, "literal:string:open")),
            Just((SPACE, predicate::JOIN, "literal:string:invite")),
            (0..3usize).prop_map(|p| (PEOPLE[p], predicate::ADMIN, SPACE)),
            (0..3usize).prop_map(|p| (PEOPLE[p], predicate::MEMBER, SPACE)),
            (0..3usize, 0..2usize).prop_map(|(p, i)| (PEOPLE[p], predicate::MEMBER, INVITES[i])),
            (0..2usize).prop_map(|i| (
                INVITES[i],
                predicate::INVITE,
                "literal:json:%7B%22expiresAt%22%3A12%7D"
            )),
            (0..3usize).prop_map(|p| (PEOPLE[p], predicate::REMOVED, SPACE)),
            Just((BOT, predicate::REMOVED, SPACE)),
            Just((BOT, predicate::BOT, SPACE)),
            (0..3usize).prop_map(|p| (BOT, predicate::APPROVED_BY, PEOPLE[p])),
            Just((SPACE, "coasys://membership/guest", "did:key:zGuest")),
        ];
        let verdict = prop_oneof![
            8 => Just(LinkVerdict::Valid),
            1 => Just(LinkVerdict::CantCheck),
            1 => Just(LinkVerdict::Flagged),
        ];
        // The author: a person's own key, a person's bot key, or a device-only key.
        let author = prop_oneof![
            6 => (0..3usize).prop_map(|p| (Some(PEOPLE[p]), false)),
            1 => (0..3usize).prop_map(|p| (Some(PEOPLE[p]), true)),
            1 => Just((None, false)),
        ];
        (row, verdict, author, 0..20u64).prop_map(
            |((source, predicate, target), verdict, (person, by_bot), timestamp)| {
                let author_key = match (person, by_bot) {
                    (_, true) => BOT.to_string(),
                    (Some(p), false) => format!("did:key:z{}", &p[p.len() - 3..]),
                    (None, _) => "did:key:zDevice".to_string(),
                };
                let identity = json!([source, predicate, target, author_key, timestamp]);
                JudgedLink {
                    hash: Sha256::digest(identity.to_string().as_bytes()).into(),
                    source: source.to_string(),
                    predicate: predicate.to_string(),
                    target: target.to_string(),
                    author_key,
                    timestamp,
                    verdict,
                    person: person.map(str::to_string),
                    by_bot,
                }
            },
        )
    }

    /// A history of up to 40 links and a permutation of it.
    pub fn shuffled_history() -> impl Strategy<Value = (Vec<JudgedLink>, Vec<JudgedLink>)> {
        proptest::collection::vec(link(), 0..40).prop_flat_map(|links| {
            let original = links.clone();
            Just(links)
                .prop_shuffle()
                .prop_map(move |shuffled| (original.clone(), shuffled))
        })
    }
}

/// What a graph holds, for comparing two: rules, join mode, members, and what a self-join of
/// each person meets, with and without each invite.
fn observed(graph: &MembershipGraph) -> Value {
    let mut admits = Vec::new();
    for person in arbitrary::PEOPLE {
        for invite in [
            None,
            Some(arbitrary::INVITES[0]),
            Some(arbitrary::INVITES[1]),
        ] {
            admits.push(json!(graph.admits(Some(person), false, invite, 10).err()));
        }
    }
    json!({"graph": graph_json(graph), "admits": admits})
}

mod order {
    use super::*;
    use proptest::prelude::*;

    fn refused_by_hash(links: &[JudgedLink], refused: &[(usize, Refusal)]) -> Vec<String> {
        let mut out: Vec<String> = refused
            .iter()
            .map(|(i, r)| format!("{}:{r:?}", hex::encode(links[*i].hash)))
            .collect();
        out.sort();
        out
    }

    proptest! {
        #[test]
        fn replay_does_not_depend_on_the_order_of_its_input(
            (links, shuffled) in arbitrary::shuffled_history()
        ) {
            let (a, refused_a) = MembershipGraph::replay(arbitrary::CREATOR, &links);
            let (b, refused_b) = MembershipGraph::replay(arbitrary::CREATOR, &shuffled);
            prop_assert_eq!(observed(&a), observed(&b));
            prop_assert_eq!(
                refused_by_hash(&links, &refused_a),
                refused_by_hash(&shuffled, &refused_b)
            );
        }

        #[test]
        fn links_applied_as_they_arrive_give_the_replay_of_those_not_backdated(
            (_, arrivals) in arbitrary::shuffled_history()
        ) {
            let mut graph = MembershipGraph::new(arbitrary::CREATOR);
            let mut kept = Vec::new();
            for link in &arrivals {
                if graph.apply(link) != Err(Refusal::Backdated) {
                    kept.push(link.clone());
                }
            }
            let (replayed, _) = MembershipGraph::replay(arbitrary::CREATOR, &kept);
            prop_assert_eq!(observed(&graph), observed(&replayed));
        }
    }
}
