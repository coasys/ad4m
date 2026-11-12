use super::*;
use crate::identity::cache::tests::TestLog;
use crate::identity::cache::{global_key_map, TrustedLog};
use crate::types::DecoratedLinkExpression;
use ad4m_identity_core::build::{add_unsigned, cosign, Signer};
use ad4m_identity_core::{BotScope, DeviceKey, KeyLife};
// ---------------------------------------------------------------------------
// A bot never counts as its owner — on stored links
// ---------------------------------------------------------------------------
//
// A link a bot key signs is judged valid with its owner as `person` and the
// bot mark `byBot`. The flow engine reads proposal links through
// `model_query`'s `__links` rows, not through `perspective.get`, so those rows
// must carry the bot mark too: without it, `flow_person` takes the owner as
// the signer and a bot's `acceptedBy → owner` is the owner's vote.

/// Person A's log, with a `write`-scoped bot key added at seq 1, published at trusted seq 1
/// as a head the store confirmed just now. Returns the log and the bot's key.
fn publish_owner_with_bot(seed: u8) -> (TestLog, ed25519_dalek::SigningKey) {
    let mut log = TestLog::new(seed);
    let bot = ed25519_dalek::SigningKey::from_bytes(&[seed.wrapping_add(1); 32]);
    let chain = log.chain();
    let bot_key = DeviceKey {
        id: chain.state().next_device_id(),
        sign: bot.key(),
        enc: ad4m_identity_core::encode_x25519_multikey(&[seed.wrapping_add(1); 32]),
        life: KeyLife::Bot(BotScope {
            can: vec!["write".into()],
            expires: 4_000_000_000,
        }),
    };
    let mut event = add_unsigned(&chain, bot_key, "offer", "co-sig").unwrap();
    cosign(&mut event, "m0", &log.master).unwrap();
    log.events.push(event);
    let map = global_key_map();
    map.publish(
        &log.scid(),
        TrustedLog {
            state: log.chain().state().clone(),
            trusted: 1,
        },
    );
    map.note_confirmed(&log.scid(), chrono::Utc::now().timestamp() as u64);
    (log, bot)
}

/// `acceptedBy → target` on `proposal_uri`, signed by `signer` anchored at `anchor` of its log.
fn anchored_vote(
    signer: &ed25519_dalek::SigningKey,
    proposal_uri: &str,
    target: &str,
    anchor: u64,
) -> LinkExpression {
    let key = signer.key();
    let data = Link {
        source: proposal_uri.to_string(),
        predicate: Some(ACCEPTED_BY_PREDICATE.to_string()),
        target: target.to_string(),
    }
    .normalize();
    let timestamp = chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Millis, true);
    let bytes =
        crate::agent::signatures::anchored_signing_bytes(&data, &timestamp, anchor).unwrap();
    LinkExpression {
        author: format!("did:key:{key}"),
        timestamp,
        data,
        proof: crate::types::ExpressionProof {
            key: format!("did:key:{key}#{key}"),
            signature: hex::encode(signer.sign(&bytes)),
            anchor: Some(anchor),
        },
        status: Some(LinkStatus::Shared),
    }
}

/// The `acceptedBy` row `author` signed on `proposal_uri`, as the engine's read-set has it.
async fn vote_row(f: &Fixture, proposal_uri: &str, author: &str) -> DecoratedLinkExpression {
    f.read_set()
        .await
        .proposals
        .into_iter()
        .find(|p| p.uri == proposal_uri)
        .expect("the proposal is in the read-set")
        .links
        .into_iter()
        .find(|l| l.author == author && l.data.predicate.as_deref() == Some(ACCEPTED_BY_PREDICATE))
        .expect("the vote is in the read-set")
}

/// Bob proposes `identified → scoped` under `{n: 2}`; a `write`-scoped bot of person A signs
/// `acceptedBy → A` and syncs in. The stored row keeps A as `person` (for display) and the bot
/// mark, and the fold does not count it as A's vote: the flow stays `identified`. A's own
/// device signing the same vote settles the edge.
#[tokio::test(flavor = "multi_thread")]
async fn a_stored_vote_by_the_owners_bot_is_not_the_owners_vote() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;

    let (log_a, bot) = publish_owner_with_bot(150);
    let person_a = ad4m_identity_core::did_of(&log_a.scid());
    let by_bot = anchored_vote(&bot, &proposal, &person_a, 1);
    f.perspective
        .add_link_expression(by_bot.clone(), LinkStatus::Shared, None)
        .await
        .expect("sync the bot's vote");

    let derived = f.derived().await;
    assert_eq!(
        derived.state, "identified",
        "a bot's vote naming its owner is not the owner's vote: Bob alone is 1 < n = 2, \
         settled: {:?}",
        derived.settled
    );
    let row = vote_row(&f, &proposal, &by_bot.author).await;
    assert_eq!(
        row.proof.valid,
        Some(true),
        "premise: the bot's link counts"
    );
    assert_eq!(
        row.person.as_deref(),
        Some(person_a.as_str()),
        "the row shows the bot's owner for display"
    );
    assert_eq!(
        row.by_bot,
        Some(true),
        "the stored row carries the bot mark"
    );

    // Control: A's own device signs the same vote, and it is A's.
    let by_device = anchored_vote(&log_a.device, &proposal, &person_a, 1);
    f.perspective
        .add_link_expression(by_device.clone(), LinkStatus::Shared, None)
        .await
        .expect("sync A's device vote");
    let row = vote_row(&f, &proposal, &by_device.author).await;
    assert_eq!(row.person.as_deref(), Some(person_a.as_str()));
    assert_eq!(row.by_bot, None, "a device's link carries no bot mark");
    assert_eq!(f.derived().await.state, "scoped", "A's own device is A");
    global_key_map().forget(&log_a.scid());
}

/// The same bot vote stored by an earlier revision: its row has the owner as `person` but no
/// bot mark and no judgement version. Until the re-check judges it again it is read
/// unclassified — no person, no bot mark — so it is not the owner's vote either.
#[tokio::test(flavor = "multi_thread")]
async fn a_stale_judgement_of_a_bot_vote_is_not_the_owners_vote() {
    let mut f = seed_satisfied_fixture(None).await;
    set_consensus_rule(&mut f, "delivery://Delivery.scoped", r#"{"n":2}"#).await;
    let bob = TestSigner::generate();
    let seal = seal_for(&f, "scoped").await;
    let proposal = sync_proposal_from(&mut f, &bob, "bob-1", "identified", "scoped", &seal).await;

    let (log_a, bot) = publish_owner_with_bot(152);
    let person_a = ad4m_identity_core::did_of(&log_a.scid());
    let by_bot = anchored_vote(&bot, &proposal, &person_a, 1);
    f.perspective
        .add_link_expression(by_bot.clone(), LinkStatus::Shared, None)
        .await
        .expect("sync the bot's vote");
    // The row an earlier revision wrote: the owner as person, no bot mark, no version.
    f.perspective
        .sparql_store
        .remove_bot_mark_and_version(&by_bot)
        .expect("drop the bot mark and the version");

    let derived = f.derived().await;
    assert_eq!(
        derived.state, "identified",
        "a stale bot vote is not the owner's vote, settled: {:?}",
        derived.settled
    );
    let row = vote_row(&f, &proposal, &by_bot.author).await;
    assert_eq!(
        row.person, None,
        "a stale judgement reads unclassified: no person"
    );
    assert_eq!(row.by_bot, None, "and no bot mark");
    global_key_map().forget(&log_a.scid());
}
