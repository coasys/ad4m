//! The membership replay of 1,000 links (`cargo bench -p ad4m-identity-core --bench
//! membership_replay`): the median of 20 runs of `MembershipGraph::replay`, against the plan's
//! budget of 50 ms on a dev machine, and of the same links applied one by one in a shuffled
//! arrival order (every arrival that sorts before the last replays the kept links). It reports;
//! it asserts nothing, so it does not run in CI.

use std::time::{Duration, Instant};

use ad4m_identity_core::judgement::LinkVerdict;
use ad4m_identity_core::membership::{predicate, JudgedLink, MembershipGraph, SPACE};
use sha2::{Digest, Sha256};

const LINKS: usize = 1_000;
const RUNS: usize = 20;
const BUDGET: Duration = Duration::from_millis(50);
const CREATOR: &str = "did:scid:coasys:1:Creator";
const DID_PREFIX_LEN: usize = ad4m_identity_core::DID_PREFIX.len();
const OPEN_RULES: &str =
    "literal:json:%7B%22add%22%3A%22admins%22%2C%22guests%22%3A%22no%22%2C%22join%22%3A%22open%22%7D";

fn link(source: &str, predicate: &str, target: &str, person: &str, timestamp: u64) -> JudgedLink {
    let author_key = format!("did:key:z{}", &person[DID_PREFIX_LEN..]);
    let hash = Sha256::digest(format!(
        "{source} {predicate} {target} {author_key} {timestamp}"
    ));
    JudgedLink {
        hash: hash.into(),
        source: source.to_string(),
        predicate: predicate.to_string(),
        target: target.to_string(),
        author_key,
        timestamp,
        verdict: LinkVerdict::Valid,
        person: Some(person.to_string()),
        by_bot: false,
    }
}

/// A space's history: the rules, then people joining (some by invite), a few admins, some
/// leaving and some removed, until there are 1,000 links.
fn history() -> Vec<JudgedLink> {
    let mut links = vec![link(SPACE, predicate::RULES, OPEN_RULES, CREATOR, 1)];
    let mut t = 1;
    let mut i = 0;
    while links.len() < LINKS {
        t += 1;
        let person = format!("did:scid:coasys:1:P{i}");
        match i % 10 {
            0 => {
                let invite = format!("invite://{i}");
                links.push(link(
                    &invite,
                    predicate::INVITE,
                    "literal:json:%7B%22expiresAt%22%3A1000000%7D",
                    CREATOR,
                    t,
                ));
                links.push(link(&person, predicate::MEMBER, &invite, &person, t + 1));
            }
            1 => links.push(link(&person, predicate::ADMIN, SPACE, CREATOR, t)),
            7 => {
                links.push(link(&person, predicate::MEMBER, SPACE, &person, t));
                links.push(link(&person, predicate::REMOVED, SPACE, &person, t + 1));
            }
            8 => {
                links.push(link(&person, predicate::MEMBER, SPACE, &person, t));
                links.push(link(&person, predicate::REMOVED, SPACE, CREATOR, t + 1));
            }
            _ => links.push(link(&person, predicate::MEMBER, SPACE, &person, t)),
        }
        t += 1;
        i += 1;
    }
    links.truncate(LINKS);
    links
}

/// A fixed permutation of `links` (a linear congruential shuffle), so runs compare.
fn shuffled(links: &[JudgedLink]) -> Vec<JudgedLink> {
    let mut out = links.to_vec();
    let mut state: u64 = 0x2545_f491_4f6c_dd1d;
    for i in (1..out.len()).rev() {
        state = state
            .wrapping_mul(6_364_136_223_846_793_005)
            .wrapping_add(1);
        out.swap(i, (state >> 33) as usize % (i + 1));
    }
    out
}

fn median(mut runs: Vec<Duration>) -> Duration {
    runs.sort();
    runs[runs.len() / 2]
}

fn main() {
    let links = history();
    let arrivals = shuffled(&links);
    let replay = median(
        (0..RUNS)
            .map(|_| {
                let start = Instant::now();
                let (graph, _) = MembershipGraph::replay(CREATOR, &arrivals);
                std::hint::black_box(graph);
                start.elapsed()
            })
            .collect(),
    );
    let applied = median(
        (0..RUNS)
            .map(|_| {
                let start = Instant::now();
                let mut graph = MembershipGraph::new(CREATOR);
                for link in &arrivals {
                    let _ = std::hint::black_box(graph.apply(link));
                }
                start.elapsed()
            })
            .collect(),
    );
    let (graph, _) = MembershipGraph::replay(CREATOR, &links);
    let current = graph.members().iter().filter(|m| m.until.is_none()).count();
    println!("membership replay, {LINKS} links, {current} current members, median of {RUNS} runs");
    println!(
        "  replay:                    {replay:?} ({} the {BUDGET:?} budget)",
        if replay <= BUDGET { "within" } else { "OVER" }
    );
    println!("  apply in shuffled arrival: {applied:?}");
}
