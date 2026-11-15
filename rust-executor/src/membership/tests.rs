//! The membership of a space on a `PerspectiveInstance` with an in-memory store: the owner
//! gate, the read filter, joins and bots (slice 3.B parts A3 and A4).

use super::*;
use crate::agent::AgentService;
use crate::db::Ad4mDb;
use crate::identity::cache::tests::TestLog;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::prolog_service::init_prolog_service;
use crate::test_utils::setup_wallet;
use crate::types::{ExpressionProof, PerspectiveDiff, PerspectiveHandle, PerspectiveState};
use ad4m_identity_core::build::{add_unsigned, cosign, Signer};
use ad4m_identity_core::{BotScope, DeviceKey, KeyLife};
use deno_core::error::AnyError;
use ed25519_dalek::SigningKey;

const OPEN: &str = "literal:json:%7B%22join%22%3A%22open%22%7D";

async fn setup() -> PerspectiveInstance {
    setup_wallet();
    Ad4mDb::init_global_instance(":memory:").unwrap();
    AgentService::init_global_test_instance();
    init_prolog_service().await;
    let instance = PerspectiveInstance::new(
        PerspectiveHandle {
            uuid: uuid::Uuid::new_v4().to_string(),
            name: Some("Membership".to_string()),
            shared_url: None,
            neighbourhood: None,
            state: PerspectiveState::Private,
            owners: None,
        },
        None,
    );
    instance
        .ensure_prolog_engine_pool()
        .await
        .expect("Failed to initialize prolog engine pool");
    instance
}

/// A person: their log (published as just confirmed), its device key, and their DID.
struct Person {
    log: TestLog,
    person: String,
}

impl Person {
    fn new(seed: u8) -> Self {
        let log = TestLog::new(seed);
        publish(&log);
        let person = ad4m_identity_core::did_of(&log.scid());
        Person { log, person }
    }

    /// A person with a bot key of write scope, added at seq 1.
    fn with_bot(seed: u8) -> (Self, SigningKey) {
        let mut log = TestLog::new(seed);
        let bot = SigningKey::from_bytes(&[seed.wrapping_add(1); 32]);
        let chain = log.chain();
        let bot_key = DeviceKey {
            id: chain.state().next_device_id(),
            sign: bot.key(),
            enc: ad4m_identity_core::encode_x25519_multikey(&[seed; 32]),
            life: KeyLife::Bot(BotScope {
                can: vec!["write".to_string()],
                expires: 4_000_000_000,
            }),
        };
        let mut event = add_unsigned(&chain, bot_key, "offer", "co-sig").unwrap();
        cosign(&mut event, "m0", &log.master).unwrap();
        log.events.push(event);
        publish(&log);
        let person = ad4m_identity_core::did_of(&log.scid());
        (Person { log, person }, bot)
    }

    /// A person with a second standing device key, added at seq 1.
    fn with_second_device(seed: u8) -> (Self, SigningKey) {
        let mut log = TestLog::new(seed);
        let second = SigningKey::from_bytes(&[seed.wrapping_add(50); 32]);
        let chain = log.chain();
        let key = DeviceKey {
            id: chain.state().next_device_id(),
            sign: second.key(),
            enc: ad4m_identity_core::encode_x25519_multikey(&[seed.wrapping_add(50); 32]),
            life: KeyLife::Standing,
        };
        let mut event = add_unsigned(&chain, key, "offer", "co-sig").unwrap();
        cosign(&mut event, "m0", &log.master).unwrap();
        log.events.push(event);
        publish(&log);
        let person = ad4m_identity_core::did_of(&log.scid());
        (Person { log, person }, second)
    }

    fn says(&self, source: &str, predicate: &str, target: &str, at: u64) -> LinkExpression {
        signed(
            &self.log.device,
            source,
            predicate,
            target,
            at,
            self.anchor(),
        )
    }

    fn anchor(&self) -> u64 {
        self.log.events.len() as u64 - 1
    }
}

impl Drop for Person {
    fn drop(&mut self) {
        crate::identity::cache::global_key_map().forget(&self.log.scid());
    }
}

fn publish(log: &TestLog) {
    use crate::identity::cache::{global_key_map, TrustedLog};
    let state = log.chain().state().clone();
    let trusted = log.events.len() as u64 - 1;
    global_key_map().publish(&log.scid(), TrustedLog { state, trusted });
    global_key_map().note_confirmed(&log.scid(), chrono::Utc::now().timestamp() as u64);
}

/// The time of the `at`-th step of a test: a minute ago plus `at` seconds, so each link
/// sorts after the one before it.
fn time(at: u64) -> u64 {
    chrono::Utc::now().timestamp_millis() as u64 - 60_000 + at * 1_000
}

fn did_key(signer: &SigningKey) -> String {
    format!("did:key:{}", signer.key())
}

/// A Shared link `signer` signed at step `at`, anchored at `anchor` of its log.
fn signed(
    signer: &SigningKey,
    source: &str,
    predicate: &str,
    target: &str,
    at: u64,
    anchor: u64,
) -> LinkExpression {
    let key = signer.key();
    let data = Link {
        source: source.into(),
        predicate: Some(predicate.into()),
        target: target.into(),
    }
    .normalize();
    let timestamp = iso(time(at));
    let bytes =
        crate::agent::signatures::anchored_signing_bytes(&data, &timestamp, anchor).unwrap();
    LinkExpression {
        author: format!("did:key:{key}"),
        timestamp,
        data,
        proof: ExpressionProof {
            key: format!("did:key:{key}#{key}"),
            signature: hex::encode(signer.sign(&bytes)),
            anchor: Some(anchor),
        },
        status: Some(LinkStatus::Shared),
    }
}

async fn write(perspective: &PerspectiveInstance, link: LinkExpression) -> Result<(), AnyError> {
    perspective
        .persist_link_diff(&PerspectiveDiff::from_additions(vec![link]), false)
        .await
        .map(|_| ())
}

async fn refused(perspective: &PerspectiveInstance, link: LinkExpression) -> Refusal {
    let error = write(perspective, link)
        .await
        .expect_err("the gate refuses it");
    refusal_of(&error).unwrap_or_else(|| panic!("not a refusal: {error:?}"))
}

/// The space's rules, by `creator`, at step 0.
fn rules(creator: &Person, target: &str) -> LinkExpression {
    creator.says(SPACE, predicate::RULES, target, 0)
}

fn role(perspective: &PerspectiveInstance, person: &str) -> Option<MemberInfo> {
    perspective
        .sparql_store
        .membership()
        .members()
        .into_iter()
        .find(|m| m.person == person)
}

/// The stored verdict of the link `source -[ad4m://says]-> target`.
fn verdict_of(perspective: &PerspectiveInstance, target: &str) -> Option<LinkVerdict> {
    perspective
        .sparql_store
        .get_all_links(&crate::perspectives::sparql_store::EvalStop::none())
        .unwrap()
        .into_iter()
        .find(|l| l.data.target == target)
        .and_then(|l| l.verdict)
}

fn checked(person: &Person) -> CheckedLink {
    CheckedLink {
        verdict: LinkVerdict::Valid,
        person: Some(person.person.clone()),
        by_bot: false,
    }
}

#[test]
fn iso_and_ms_round_trip() {
    let ms = 1_763_164_800_123;
    assert_eq!(ms_of(&iso(ms)), Some(ms));
    assert_eq!(ms_of("not a time"), None);
}

#[test]
fn the_links_each_call_writes() {
    let rules = links::rules(AddRule::Admins, JoinMode::Open);
    assert_eq!(rules.predicate.as_deref(), Some(predicate::RULES));
    assert_eq!(
        rules.target,
        OPEN.replace("%7B", "%7B%22add%22%3A%22admins%22%2C")
    );
    let (invite, link) = links::invite(NO_EXPIRY);
    assert!(invite.starts_with(INVITE_PREFIX));
    assert_eq!(link.source, invite);
    assert_eq!(
        link.target,
        "literal:json:%7B%22expiresAt%22%3A9007199254740991%7D"
    );
    assert_eq!(
        links::join_mode(JoinMode::Invite).target,
        "literal:string:invite"
    );
    assert!(links::add_member("did:scid:coasys:1:x", MemberRole::Bot).is_err());
    assert_eq!(
        invite_url("ad4m://nh", "invite://i"),
        "coasys://join?url=ad4m%3A%2F%2Fnh&invite=invite%3A%2F%2Fi"
    );
}

/// The links `neighbourhood.create` writes in one batch, so with one timestamp: whichever way
/// their hashes sort, none is refused and the creator is the first admin. (A creator's `admin`
/// link beside the `rules` link was refused `NotAdmin` whenever its hash sorted first.)
#[test]
fn the_genesis_links_count_in_any_hash_order() {
    let creator = "did:scid:coasys:1:Ada";
    let genesis = links::genesis(AddRule::Admins, JoinMode::Open);
    let judged = |order: &[usize]| -> Vec<JudgedLink> {
        genesis
            .iter()
            .zip(order)
            .map(|(link, &rank)| JudgedLink {
                hash: [rank as u8 + 1; 32],
                source: link.source.clone(),
                predicate: link.predicate.clone().unwrap_or_default(),
                target: link.target.clone(),
                author_key: "did:key:zAda".to_string(),
                timestamp: 1_000,
                verdict: LinkVerdict::Valid,
                person: Some(creator.to_string()),
                by_bot: false,
            })
            .collect()
    };
    let mut orders: Vec<Vec<usize>> = vec![(0..genesis.len()).collect()];
    orders.push(orders[0].iter().rev().copied().collect());
    for order in orders {
        let links = judged(&order);
        let (graph, refused) = MembershipGraph::replay(creator, &links);
        assert!(refused.is_empty(), "hash order {order:?} refused {refused:?}");
        assert_eq!(graph.role_of(creator), Some(Role::Admin), "hash order {order:?}");
        assert!(graph.rules().is_some());
    }
}

/// In an open space a person's own `member` link admits them; `neighbourhood.join` would
/// write it, and a member already writes nothing.
#[tokio::test]
async fn a_self_join_counts_in_an_open_space() {
    let perspective = setup().await;
    let ada = Person::new(160);
    let bea = Person::new(161);
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    assert_eq!(
        role(&perspective, &ada.person).unwrap().role,
        MemberRole::Admin
    );

    let membership = perspective.sparql_store.membership();
    let join = join_link(membership, &checked(&bea), None, time(1)).unwrap();
    assert_eq!(join, Some(links::self_join(&bea.person, None)));
    write(
        &perspective,
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();
    assert_eq!(
        role(&perspective, &bea.person).unwrap().role,
        MemberRole::Member
    );
    assert_eq!(
        (perspective.sparql_store.membership().take_changes().0).len(),
        0,
        "the write published what it changed"
    );
    assert_eq!(
        join_link(membership, &checked(&bea), None, time(2)),
        Ok(None)
    );
}

/// An invite-only space refuses a self-join without an invite; an admin's invite admits
/// one person, and not a second.
#[tokio::test]
async fn an_invite_only_space_refuses_a_self_join_without_an_invite() {
    let perspective = setup().await;
    let ada = Person::new(162);
    let bea = Person::new(163);
    let cyd = Person::new(164);
    write(&perspective, rules(&ada, "literal:json:%7B%7D"))
        .await
        .unwrap();

    let membership = perspective.sparql_store.membership();
    assert_eq!(
        join_link(membership, &checked(&bea), None, time(1)),
        Err(Refusal::InviteOnly)
    );
    assert_eq!(
        refused(
            &perspective,
            bea.says(&bea.person, predicate::MEMBER, SPACE, 1)
        )
        .await,
        Refusal::InviteOnly
    );
    assert!(role(&perspective, &bea.person).is_none());

    let (invite, made) = links::invite(time(100));
    let made = ada.says(&made.source, predicate::INVITE, &made.target, 2);
    write(&perspective, made).await.unwrap();
    assert_eq!(
        join_link(membership, &checked(&bea), Some(&invite), time(3)),
        Ok(Some(links::self_join(&bea.person, Some(&invite))))
    );
    write(
        &perspective,
        bea.says(&bea.person, predicate::MEMBER, &invite, 3),
    )
    .await
    .unwrap();
    assert!(role(&perspective, &bea.person).is_some());
    let invites = membership.invites();
    assert_eq!(invites.len(), 1);
    assert_eq!(invites[0].used_by.as_deref(), Some(bea.person.as_str()));
    assert!(invites[0].expires_at.is_some());

    assert_eq!(
        join_link(membership, &checked(&cyd), Some(&invite), time(4)),
        Err(Refusal::InviteUsed)
    );
    assert_eq!(
        refused(
            &perspective,
            cyd.says(&cyd.person, predicate::MEMBER, &invite, 4)
        )
        .await,
        Refusal::InviteUsed
    );
    assert_eq!(
        JoinRefusalReason::from(Refusal::InviteUsed),
        JoinRefusalReason::InviteUsed
    );
}

/// A person an admin removed cannot join an open space again on their own; one who left
/// can.
#[tokio::test]
async fn an_admin_removal_bars_a_rejoin_and_a_leave_does_not() {
    let perspective = setup().await;
    let ada = Person::new(165);
    let bea = Person::new(166);
    let cyd = Person::new(167);
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    write(
        &perspective,
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();
    write(
        &perspective,
        cyd.says(&cyd.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();

    // Bea is not an admin: she cannot remove Cyd.
    assert_eq!(
        refused(
            &perspective,
            bea.says(&cyd.person, predicate::REMOVED, SPACE, 2)
        )
        .await,
        Refusal::NotAdmin
    );
    write(
        &perspective,
        ada.says(&bea.person, predicate::REMOVED, SPACE, 2),
    )
    .await
    .unwrap();
    assert!(role(&perspective, &bea.person).is_none());
    let membership = perspective.sparql_store.membership();
    assert_eq!(
        join_link(membership, &checked(&bea), None, time(3)),
        Err(Refusal::Removed)
    );
    assert_eq!(
        refused(
            &perspective,
            bea.says(&bea.person, predicate::MEMBER, SPACE, 3)
        )
        .await,
        Refusal::Removed
    );
    assert_eq!(
        JoinRefusalReason::from(Refusal::Removed),
        JoinRefusalReason::NotAMember
    );

    // Cyd leaves, then comes back.
    write(
        &perspective,
        cyd.says(&cyd.person, predicate::REMOVED, SPACE, 4),
    )
    .await
    .unwrap();
    assert!(role(&perspective, &cyd.person).is_none());
    write(
        &perspective,
        cyd.says(&cyd.person, predicate::MEMBER, SPACE, 5),
    )
    .await
    .unwrap();
    assert!(role(&perspective, &cyd.person).is_some());
}

/// A key with no identity, or a bot's key, cannot join: `needs-identity`.
#[tokio::test]
async fn a_join_needs_an_identity() {
    let perspective = setup().await;
    let ada = Person::new(168);
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    let membership = perspective.sparql_store.membership();
    let no_person = CheckedLink {
        verdict: LinkVerdict::Valid,
        person: None,
        by_bot: false,
    };
    let a_bot = CheckedLink {
        verdict: LinkVerdict::Valid,
        person: Some(ada.person.clone()),
        by_bot: true,
    };
    for caller in [no_person, a_bot] {
        assert_eq!(
            join_link(membership, &caller, None, time(1)),
            Err(Refusal::NeedsIdentity)
        );
    }
    assert_eq!(
        JoinRefusalReason::from(Refusal::NeedsIdentity),
        JoinRefusalReason::NeedsIdentity
    );
    // A space with no rules here governs nothing yet: the join writes nothing.
    let ungoverned = SpaceMembership::new();
    assert_eq!(
        join_link(&ungoverned, &checked(&ada), None, time(1)),
        Ok(None)
    );
}

/// A non-member's link is stored `notMember` once the space has rules, and `valid` again
/// while they are a member; the creator's own link stays valid throughout.
#[tokio::test]
async fn the_read_filter_turns_off_and_on() {
    let perspective = setup().await;
    let ada = Person::new(169);
    let bea = Person::new(170);
    write(
        &perspective,
        bea.says("ad4m://bea", "ad4m://says", "ad4m://bea-1", 0),
    )
    .await
    .unwrap();
    write(
        &perspective,
        ada.says("ad4m://ada", "ad4m://says", "ad4m://ada-1", 0),
    )
    .await
    .unwrap();
    assert_eq!(
        verdict_of(&perspective, "ad4m://bea-1"),
        Some(LinkVerdict::Valid)
    );

    // Invite only: Bea is not a member, so her link turns off.
    write(
        &perspective,
        ada.says(SPACE, predicate::RULES, "literal:json:%7B%7D", 1),
    )
    .await
    .unwrap();
    assert_eq!(
        verdict_of(&perspective, "ad4m://bea-1"),
        Some(LinkVerdict::NotMember)
    );
    assert_eq!(
        verdict_of(&perspective, "ad4m://ada-1"),
        Some(LinkVerdict::Valid)
    );
    let shown = perspective
        .sparql_store
        .get_all_links(&crate::perspectives::sparql_store::EvalStop::none())
        .unwrap()
        .into_iter()
        .filter(|l| l.data.target == "ad4m://bea-1")
        .map(|l| (l.person, l.proof.valid))
        .collect::<Vec<_>>();
    assert_eq!(shown, vec![(None, Some(false))], "no person, not counted");
    // A new link of hers is stored off from the start.
    write(
        &perspective,
        bea.says("ad4m://bea", "ad4m://says", "ad4m://bea-2", 2),
    )
    .await
    .unwrap();
    assert_eq!(
        verdict_of(&perspective, "ad4m://bea-2"),
        Some(LinkVerdict::NotMember)
    );

    // Ada adds her: both links turn on.
    write(
        &perspective,
        ada.says(&bea.person, predicate::MEMBER, SPACE, 3),
    )
    .await
    .unwrap();
    for target in ["ad4m://bea-1", "ad4m://bea-2"] {
        assert_eq!(verdict_of(&perspective, target), Some(LinkVerdict::Valid));
    }

    // Ada removes her: off again.
    write(
        &perspective,
        ada.says(&bea.person, predicate::REMOVED, SPACE, 4),
    )
    .await
    .unwrap();
    assert_eq!(
        verdict_of(&perspective, "ad4m://bea-1"),
        Some(LinkVerdict::NotMember)
    );

    // The store reopens on the same replay: a rebuild gives the same members.
    let before = perspective.sparql_store.membership().members();
    let rebuilt = SpaceMembership::new();
    rebuilt.rebuild(&perspective.sparql_store.membership_links().unwrap());
    assert_eq!(rebuilt.members(), before);
    assert!(rebuilt.governed());
}

/// A membership link never ends: removing one is refused (R12).
#[tokio::test]
async fn a_membership_link_cannot_be_removed() {
    let mut perspective = setup().await;
    let ada = Person::new(171);
    let link = rules(&ada, OPEN);
    write(&perspective, link.clone()).await.unwrap();
    assert!(perspective.remove_link(link, None).await.is_err());
    assert!(perspective.sparql_store.membership().governed());
}

/// Only the creator's first `rules` link counts: a second person's is refused, and so is
/// a member link before any rules.
#[tokio::test]
async fn the_first_rules_link_names_the_creator() {
    let perspective = setup().await;
    let ada = Person::new(172);
    let bea = Person::new(173);
    assert_eq!(
        refused(
            &perspective,
            ada.says(&bea.person, predicate::MEMBER, SPACE, 0)
        )
        .await,
        Refusal::NotAdmin
    );
    assert!(!perspective.sparql_store.membership().governed());
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    assert_eq!(
        refused(&perspective, bea.says(SPACE, predicate::RULES, OPEN, 1)).await,
        Refusal::NotCreator
    );
}

/// A4: a member's bot joins with the member's `bot` and `approved-by` links, as a bot of
/// that member; its links count while it is a member.
#[tokio::test]
async fn add_bot_by_a_member_makes_a_bot_of_theirs() {
    let perspective = setup().await;
    let ada = Person::new(174);
    let (bea, bot) = Person::with_bot(175);
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    write(
        &perspective,
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();
    // The bot's link before it is a member: off.
    let bot_link = signed(&bot, "ad4m://bot", "ad4m://says", "ad4m://bot-1", 2, 1);
    write(&perspective, bot_link).await.unwrap();
    assert_eq!(
        verdict_of(&perspective, "ad4m://bot-1"),
        Some(LinkVerdict::NotMember)
    );

    let [proposed, approved] = links::add_bot(&did_key(&bot), &bea.person);
    let diff = PerspectiveDiff::from_additions(vec![
        bea.says(&proposed.source, predicate::BOT, &proposed.target, 3),
        bea.says(
            &approved.source,
            predicate::APPROVED_BY,
            &approved.target,
            3,
        ),
    ]);
    perspective.persist_link_diff(&diff, false).await.unwrap();
    let member = role(&perspective, &did_key(&bot)).unwrap();
    assert_eq!(member.role, MemberRole::Bot);
    assert_eq!(member.bot_of.as_deref(), Some(bea.person.as_str()));
    assert_eq!(
        verdict_of(&perspective, "ad4m://bot-1"),
        Some(LinkVerdict::Valid)
    );
}

/// A4: a bot key cannot add a bot: its links speak for no person.
#[tokio::test]
async fn add_bot_signed_by_a_bot_key_is_refused() {
    let perspective = setup().await;
    let (ada, bot) = Person::with_bot(176);
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    let other = SigningKey::from_bytes(&[9; 32]);
    let link = signed(&bot, &did_key(&other), predicate::BOT, SPACE, 1, 1);
    assert_eq!(refused(&perspective, link).await, Refusal::NeedsIdentity);
    let link = signed(
        &bot,
        &did_key(&other),
        predicate::APPROVED_BY,
        &ada.person,
        1,
        1,
    );
    assert_eq!(refused(&perspective, link).await, Refusal::NeedsIdentity);
}

/// A4: the owner removes their bot; another member cannot.
#[tokio::test]
async fn remove_bot_by_its_owner_and_not_by_another_member() {
    let perspective = setup().await;
    let ada = Person::new(177);
    let (bea, bot) = Person::with_bot(178);
    let cyd = Person::new(179);
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    for (person, at) in [(&bea, 1), (&cyd, 1)] {
        write(
            &perspective,
            person.says(&person.person, predicate::MEMBER, SPACE, at),
        )
        .await
        .unwrap();
    }
    let [proposed, approved] = links::add_bot(&did_key(&bot), &bea.person);
    let diff = PerspectiveDiff::from_additions(vec![
        bea.says(&proposed.source, predicate::BOT, &proposed.target, 2),
        bea.says(
            &approved.source,
            predicate::APPROVED_BY,
            &approved.target,
            2,
        ),
    ]);
    perspective.persist_link_diff(&diff, false).await.unwrap();

    let removal = links::removed(&did_key(&bot));
    assert_eq!(
        refused(
            &perspective,
            cyd.says(&removal.source, predicate::REMOVED, SPACE, 3)
        )
        .await,
        Refusal::NotAdmin
    );
    assert!(role(&perspective, &did_key(&bot)).is_some());
    write(
        &perspective,
        bea.says(&removal.source, predicate::REMOVED, SPACE, 4),
    )
    .await
    .unwrap();
    assert!(role(&perspective, &did_key(&bot)).is_none());
}

/// The gate refuses a whole diff: an ordinary link beside a refused membership link is not
/// written either.
#[tokio::test]
async fn a_refused_membership_link_refuses_its_diff() {
    let perspective = setup().await;
    let ada = Person::new(180);
    let bea = Person::new(181);
    write(&perspective, rules(&ada, "literal:json:%7B%7D"))
        .await
        .unwrap();
    let diff = PerspectiveDiff::from_additions(vec![
        bea.says("ad4m://bea", "ad4m://says", "ad4m://beside", 1),
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    ]);
    let error = perspective
        .persist_link_diff(&diff, false)
        .await
        .unwrap_err();
    assert_eq!(refusal_of(&error), Some(Refusal::InviteOnly));
    assert_eq!(verdict_of(&perspective, "ad4m://beside"), None);
}

/// A7: the membership links the link language syncs go to the space's membership, one by one
/// as the server counted them: each counts or is refused on its own, a link this store holds
/// already counts again, and a link that is not a membership link is refused as malformed and
/// not written. Each result names the link by its hash.
#[tokio::test]
async fn membership_links_from_the_link_language_go_to_the_membership() {
    let perspective = setup().await;
    let ada = Person::new(182);
    let bea = Person::new(183);
    let cyd = Person::new(184);
    let rules = rules(&ada, OPEN);
    let join = bea.says(&bea.person, predicate::MEMBER, SPACE, 1);
    let forged = cyd.says(&cyd.person, predicate::ADMIN, SPACE, 2);
    let plain = ada.says("ad4m://ada", "ad4m://says", "ad4m://plain", 3);
    let sent = vec![rules.clone(), join.clone(), forged.clone(), plain.clone()];
    let results = perspective
        .membership_links_from_link_language(sent.clone())
        .await;

    let hashes: Vec<String> = sent
        .iter()
        .map(|l| hex::encode(crate::perspectives::sparql_store::link_hash(l)))
        .collect();
    let got: Vec<(String, bool, Option<Refusal>)> = results
        .into_iter()
        .map(|r| (r.hash, r.counted, r.reason))
        .collect();
    assert_eq!(
        got,
        vec![
            (hashes[0].clone(), true, None),
            (hashes[1].clone(), true, None),
            (hashes[2].clone(), false, Some(Refusal::NotAdmin)),
            (hashes[3].clone(), false, Some(Refusal::Malformed)),
        ]
    );
    assert_eq!(
        role(&perspective, &bea.person).unwrap().role,
        MemberRole::Member
    );
    assert!(role(&perspective, &cyd.person).is_none());
    assert_eq!(verdict_of(&perspective, "ad4m://plain"), None);
    let stored = perspective.sparql_store.membership_links().unwrap();
    assert_eq!(stored.len(), 2, "the counted links are stored: {stored:?}");
    assert!(stored.iter().all(|l| l.status == Some(LinkStatus::Shared)));
    // Bea's join was published as a membership change.
    let (changes, _) = perspective.sparql_store.membership().take_changes();
    assert!(changes.is_empty(), "published already: {changes:?}");

    // The same links again: what is held counts, the forged link is refused again.
    let again = perspective
        .membership_links_from_link_language(vec![rules, join, forged])
        .await;
    let counted: Vec<bool> = again.iter().map(|r| r.counted).collect();
    assert_eq!(counted, vec![true, true, false]);
    assert_eq!(
        perspective.sparql_store.membership_links().unwrap().len(),
        2
    );
}

/// A7: whether the space admits a key now, as the link language asks before it seals a room
/// key or grants one: a current member's device key or a member bot's key is admitted; a
/// person who is not a member is not; a key with no identity needs one.
#[tokio::test]
async fn a_key_is_admitted_while_its_person_is_a_member() {
    let perspective = setup().await;
    let ada = Person::new(185);
    let (bea, bot) = Person::with_bot(186);
    let cyd = Person::new(188);
    let unknown = SigningKey::from_bytes(&[189; 32]);
    let admits = |key: String| {
        let perspective = perspective.clone();
        async move { membership_admits(&perspective, &key).await }
    };
    // No rules yet: nobody is a member.
    assert_eq!(
        admits(did_key(&ada.log.device)).await,
        MembershipAdmission::refused(JoinRefusalReason::NotAMember)
    );
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    write(
        &perspective,
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();
    assert_eq!(
        admits(did_key(&ada.log.device)).await,
        MembershipAdmission::ADMITTED
    );
    assert_eq!(
        admits(did_key(&bea.log.device)).await,
        MembershipAdmission::ADMITTED
    );
    assert_eq!(
        admits(did_key(&cyd.log.device)).await,
        MembershipAdmission::refused(JoinRefusalReason::NotAMember)
    );
    assert_eq!(
        admits(did_key(&unknown)).await,
        MembershipAdmission::refused(JoinRefusalReason::NeedsIdentity)
    );
    assert_eq!(
        admits("not a did".to_string()).await,
        MembershipAdmission::refused(JoinRefusalReason::NeedsIdentity)
    );
    // Bea's bot: not a member until she adds it.
    assert_eq!(
        admits(did_key(&bot)).await,
        MembershipAdmission::refused(JoinRefusalReason::NotAMember)
    );
    let [proposed, approved] = links::add_bot(&did_key(&bot), &bea.person);
    let diff = PerspectiveDiff::from_additions(vec![
        bea.says(&proposed.source, predicate::BOT, &proposed.target, 2),
        bea.says(
            &approved.source,
            predicate::APPROVED_BY,
            &approved.target,
            2,
        ),
    ]);
    perspective.persist_link_diff(&diff, false).await.unwrap();
    assert_eq!(admits(did_key(&bot)).await, MembershipAdmission::ADMITTED);
    // Removed: no longer admitted.
    write(
        &perspective,
        ada.says(&bea.person, predicate::REMOVED, SPACE, 3),
    )
    .await
    .unwrap();
    assert_eq!(
        admits(did_key(&bea.log.device)).await,
        MembershipAdmission::refused(JoinRefusalReason::NotAMember)
    );
}

const POLL: &str = "ad4m://poll";
const VOTE: &str = "ad4m://vote";

/// The votes on [`POLL`], counted once per person at `at` (step; now without it).
fn tally(perspective: &PerspectiveInstance, at: Option<u64>) -> PersonCount {
    let links = perspective
        .sparql_store
        .query_links(
            &crate::perspectives::sparql_store::EvalStop::none(),
            Some(POLL),
            Some(VOTE),
            None,
            None,
            None,
            None,
        )
        .unwrap();
    count_by_person(
        &links,
        perspective.sparql_store.membership(),
        at.map(|step| time(step) + 500),
    )
}

fn targets(pairs: &[(&str, u32)]) -> std::collections::BTreeMap<String, u32> {
    pairs.iter().map(|(t, n)| (t.to_string(), *n)).collect()
}

/// A9: a person who votes from two device keys counts once, for their latest vote.
#[tokio::test]
async fn two_keys_of_one_person_count_once() {
    let perspective = setup().await;
    let ada = Person::new(210);
    let (bea, second) = Person::with_second_device(211);
    let cyd = Person::new(212);
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    for person in [&bea, &cyd] {
        write(
            &perspective,
            person.says(&person.person, predicate::MEMBER, SPACE, 1),
        )
        .await
        .unwrap();
    }
    write(&perspective, bea.says(POLL, VOTE, "ad4m://opt-a", 2))
        .await
        .unwrap();
    write(
        &perspective,
        signed(&second, POLL, VOTE, "ad4m://opt-b", 3, bea.anchor()),
    )
    .await
    .unwrap();
    write(&perspective, cyd.says(POLL, VOTE, "ad4m://opt-a", 3))
        .await
        .unwrap();

    let count = tally(&perspective, None);
    assert_eq!(
        count.by_target,
        targets(&[("ad4m://opt-a", 1), ("ad4m://opt-b", 1)])
    );
    assert_eq!(count.counted, 2);
    assert_eq!(count.eligible, 3, "Ada, Bea and Cyd");
    assert_eq!(count.skipped, SkipCounts::default());

    // At step 2 only Bea's first vote was cast.
    let then = tally(&perspective, Some(2));
    assert_eq!(then.by_target, targets(&[("ad4m://opt-a", 1)]));
    assert_eq!(then.counted, 1);
}

/// A9: a bot's vote stays shown but is not counted: it goes to `bots`.
#[tokio::test]
async fn a_bots_vote_is_skipped_into_bots() {
    let perspective = setup().await;
    let ada = Person::new(213);
    let (bea, bot) = Person::with_bot(214);
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    write(
        &perspective,
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();
    let [proposed, approved] = links::add_bot(&did_key(&bot), &bea.person);
    let diff = PerspectiveDiff::from_additions(vec![
        bea.says(&proposed.source, predicate::BOT, &proposed.target, 2),
        bea.says(
            &approved.source,
            predicate::APPROVED_BY,
            &approved.target,
            2,
        ),
    ]);
    perspective.persist_link_diff(&diff, false).await.unwrap();
    write(
        &perspective,
        signed(&bot, POLL, VOTE, "ad4m://opt-a", 3, bea.anchor()),
    )
    .await
    .unwrap();
    write(&perspective, bea.says(POLL, VOTE, "ad4m://opt-b", 3))
        .await
        .unwrap();
    assert_eq!(
        verdict_of(&perspective, "ad4m://opt-a"),
        Some(LinkVerdict::Valid),
        "the bot's vote is shown"
    );

    let count = tally(&perspective, None);
    assert_eq!(count.by_target, targets(&[("ad4m://opt-b", 1)]));
    assert_eq!(count.counted, 1);
    assert_eq!(count.skipped.bots, 1);
    assert_eq!(count.eligible, 2, "Ada and Bea; the bot is not a person");
}

/// A9: a member's vote counts until an admin removes them; then it moves to `nonMembers`.
/// Counted at a time before the vote, it is left out, and both members were eligible then.
#[tokio::test]
async fn a_removed_members_vote_moves_into_non_members() {
    let perspective = setup().await;
    let ada = Person::new(215);
    let bea = Person::new(216);
    write(&perspective, rules(&ada, OPEN)).await.unwrap();
    write(
        &perspective,
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();
    write(&perspective, bea.says(POLL, VOTE, "ad4m://opt-a", 2))
        .await
        .unwrap();
    let count = tally(&perspective, None);
    assert_eq!(count.by_target, targets(&[("ad4m://opt-a", 1)]));
    assert_eq!((count.counted, count.eligible), (1, 2));

    write(
        &perspective,
        ada.says(&bea.person, predicate::REMOVED, SPACE, 3),
    )
    .await
    .unwrap();
    let count = tally(&perspective, None);
    assert!(count.by_target.is_empty());
    assert_eq!(count.counted, 0);
    assert_eq!(count.eligible, 1, "only Ada");
    assert_eq!(
        count.skipped,
        SkipCounts {
            non_members: 1,
            ..SkipCounts::default()
        }
    );

    let before = tally(&perspective, Some(1));
    assert_eq!(before.counted, 0);
    assert_eq!(before.skipped, SkipCounts::default());
    assert_eq!(before.eligible, 2, "Bea was a member at step 1");
}

/// A space registered with the executor: a neighbourhood (`shared`) or a private perspective,
/// named `name`. Unregistered when dropped.
struct Space {
    perspective: PerspectiveInstance,
}

impl Space {
    async fn open(name: &str, shared: bool) -> Self {
        let uuid = uuid::Uuid::new_v4().to_string();
        let perspective = PerspectiveInstance::new(
            PerspectiveHandle {
                uuid: uuid.clone(),
                name: Some(name.to_string()),
                shared_url: shared.then(|| format!("neighbourhood://{uuid}")),
                neighbourhood: None,
                state: PerspectiveState::Synced,
                owners: None,
            },
            None,
        );
        perspective
            .ensure_prolog_engine_pool()
            .await
            .expect("prolog engine pool");
        crate::perspectives::register_perspective(uuid, perspective.clone());
        Space { perspective }
    }

    async fn uuid(&self) -> String {
        self.perspective.persisted.lock().await.uuid.clone()
    }
}

impl Drop for Space {
    fn drop(&mut self) {
        if let Ok(handle) = self.perspective.persisted.try_lock() {
            crate::perspectives::unregister_perspective(&handle.uuid);
        }
    }
}

/// The main agent speaks for `person` from now on.
fn bind_main_agent(person: &Person) {
    let did = crate::agent::did_for_context(&crate::agent::AgentContext::main_agent()).unwrap();
    crate::db_backend::db_backend()
        .set_agent_identity(&did, &person.log.scid(), 1)
        .unwrap();
}

/// Bea's device key revoked in her log, as the key cache reports it.
fn revoke_device(person: &mut Person) -> crate::identity::verdict::VerdictChange {
    person.log.revoke_d1();
    publish(&person.log);
    crate::identity::verdict::VerdictChange {
        key: person.log.device.key(),
        scid: person.log.scid(),
        verdict: crate::identity::verdict::KeyVerdict::Revoked,
    }
}

fn now_ms() -> u64 {
    chrono::Utc::now().timestamp_millis() as u64
}

/// A9: a revoked device key gets one row per space its person holds here: a neighbourhood
/// the main agent (bound to the person) owns, of which, if governed, the person is a member.
/// A private perspective, and a governed space the person is not a member of, get none. A
/// space whose link language never reports (Holochain) keeps both times empty.
#[tokio::test]
async fn a_revocation_records_a_row_per_held_space() {
    let _ = setup().await;
    let ada = Person::new(217);
    let mut bea = Person::new(218);
    let garden = Space::open("Garden", true).await;
    let closed = Space::open("Closed", true).await;
    let private = Space::open("Private", false).await;
    write(&garden.perspective, rules(&ada, OPEN)).await.unwrap();
    write(
        &garden.perspective,
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();
    write(&closed.perspective, rules(&ada, OPEN)).await.unwrap();
    bind_main_agent(&bea);

    let held = reach::held_spaces(&bea.log.scid()).await;
    let garden_uuid = garden.uuid().await;
    assert!(held.contains(&(garden_uuid.clone(), "Garden".to_string())));
    let (closed_uuid, private_uuid) = (closed.uuid().await, private.uuid().await);
    assert!(!held.iter().any(|(uuid, _)| *uuid == closed_uuid));
    assert!(!held.iter().any(|(uuid, _)| *uuid == private_uuid));

    let change = revoke_device(&mut bea);
    let now = now_ms();
    assert!(reach::record_revocation(&change, now).await.unwrap());
    // The cache fires it again after a restart: recorded once.
    assert!(!reach::record_revocation(&change, now + 1).await.unwrap());
    let did_key = did_key(&bea.log.device);
    let rows = reach::reach_of(&did_key, now).unwrap();
    let garden_row = rows
        .iter()
        .find(|r| r.perspective_uuid == garden_uuid)
        .expect("a row for the garden");
    assert_eq!(
        *garden_row,
        reach::RevocationReach {
            did_key: did_key.clone(),
            perspective_uuid: garden_uuid,
            name: "Garden".into(),
            dropped_at: None,
            rotated_at: None,
        }
    );
    assert_eq!(rows.len(), held.len());

    // A master key's change, or one that is not a revocation, records nothing.
    let other = crate::identity::verdict::VerdictChange {
        key: ada.log.device.key(),
        scid: ada.log.scid(),
        verdict: crate::identity::verdict::KeyVerdict::Expired,
    };
    assert!(!reach::record_revocation(&other, now).await.unwrap());
    let master = crate::identity::verdict::VerdictChange {
        key: bea.log.master.key(),
        scid: bea.log.scid(),
        verdict: crate::identity::verdict::KeyVerdict::Revoked,
    };
    assert!(!reach::record_revocation(&master, now).await.unwrap());
}

/// A9: the space's link language reports the drop and the rotation (`roomKeyEvent`): each
/// time lands on the key's row once.
#[tokio::test]
async fn a_room_key_event_fills_the_times() {
    let _ = setup().await;
    let ada = Person::new(219);
    let mut bea = Person::new(220);
    let garden = Space::open("Garden", true).await;
    write(&garden.perspective, rules(&ada, OPEN)).await.unwrap();
    write(
        &garden.perspective,
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();
    bind_main_agent(&bea);
    let change = revoke_device(&mut bea);
    let did_key = did_key(&bea.log.device);
    let uuid = garden.uuid().await;
    assert!(reach::record_revocation(&change, now_ms()).await.unwrap());

    let before = now_ms();
    garden
        .perspective
        .publish_room_key_event(RoomKeyEvent {
            kind: RoomKeyEventKind::Dropped,
            removed_keys: vec![did_key.clone()],
            version: None,
        })
        .await;
    garden
        .perspective
        .publish_room_key_event(RoomKeyEvent {
            kind: RoomKeyEventKind::Rotated,
            removed_keys: vec![did_key.clone(), "did:key:zSomeoneElse".into()],
            version: Some(2),
        })
        .await;
    let row = reach::reach_of(&did_key, now_ms())
        .unwrap()
        .into_iter()
        .find(|r| r.perspective_uuid == uuid)
        .expect("the garden's row");
    let dropped = row.dropped_at.clone().expect("dropped");
    let rotated = row.rotated_at.clone().expect("rotated");
    assert!(ms_of(&dropped).unwrap() >= before);
    assert!(ms_of(&rotated).unwrap() >= ms_of(&dropped).unwrap());

    // A second drop keeps the first time.
    garden
        .perspective
        .publish_room_key_event(RoomKeyEvent {
            kind: RoomKeyEventKind::Dropped,
            removed_keys: vec![did_key.clone()],
            version: None,
        })
        .await;
    let again = reach::reach_of(&did_key, now_ms())
        .unwrap()
        .into_iter()
        .find(|r| r.perspective_uuid == uuid)
        .unwrap();
    assert_eq!(again.dropped_at, Some(dropped));
}

/// A9: rows live 30 days: older ones are not read, and the prune deletes them.
#[tokio::test]
async fn rows_older_than_30_days_are_pruned() {
    let _ = setup().await;
    let ada = Person::new(221);
    // Two device keys of Bea's, revoked 31 and 29 days ago (an agent speaks for one person
    // for good, so one person).
    let (mut bea, second) = Person::with_second_device(222);
    let garden = Space::open("Garden", true).await;
    write(&garden.perspective, rules(&ada, OPEN)).await.unwrap();
    write(
        &garden.perspective,
        bea.says(&bea.person, predicate::MEMBER, SPACE, 1),
    )
    .await
    .unwrap();
    let now = now_ms();
    let day = 24 * 60 * 60 * 1000;
    bind_main_agent(&bea);
    let old_key = did_key(&bea.log.device);
    let old = revoke_device(&mut bea);
    assert!(reach::record_revocation(&old, now - 31 * day)
        .await
        .unwrap());
    let recent = crate::identity::verdict::VerdictChange {
        key: second.key(),
        scid: bea.log.scid(),
        verdict: crate::identity::verdict::KeyVerdict::Revoked,
    };
    assert!(reach::record_revocation(&recent, now - 29 * day)
        .await
        .unwrap());

    let recent_key = did_key(&second);
    let uuid = garden.uuid().await;
    let in_garden = |key: &str, at: u64| {
        reach::reach_of(key, at)
            .unwrap()
            .iter()
            .any(|row| row.perspective_uuid == uuid)
    };
    assert!(!in_garden(&old_key, now));
    assert!(
        in_garden(&old_key, now - 2 * day),
        "read within 30 days of its record, the old row is there"
    );
    assert!(in_garden(&recent_key, now));
    assert!(reach::prune(now).unwrap() >= 1);
    assert!(!in_garden(&old_key, now - 2 * day), "pruned");
    assert!(in_garden(&recent_key, now));
}
