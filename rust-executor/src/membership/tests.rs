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
