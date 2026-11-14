use std::sync::Arc;

use serde_json::json;
use uuid::Uuid;

use super::*;
use crate::languages::language_runtime::LanguageOperation;
use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
use crate::mcp::tools::test_support::{operator, setup};
use crate::perspectives::{register_perspective, unregister_perspective};
use crate::types::{DecoratedNeighbourhoodExpression, PerspectiveHandle};

const CREATOR: &str = "did:scid:coasys:1:QmCreator";

fn server_link_meta(applied: Option<serde_json::Value>) -> LanguageMeta {
    LanguageMeta {
        name: "Server Link Language".into(),
        possible_template_params: Some(vec!["SERVER_URL".into(), "UID".into(), "CREATOR".into()]),
        template_applied_params: applied.map(|a| a.to_string()),
        ..Default::default()
    }
}

fn p_diff_sync_meta() -> LanguageMeta {
    LanguageMeta {
        name: "perspectiveDiffSync".into(),
        possible_template_params: Some(vec!["uid".into(), "name".into(), "description".into()]),
        template_applied_params: Some(json!({ "uid": "u", "name": "n" }).to_string()),
        ..Default::default()
    }
}

/// A perspective registered in memory only (no link language attaches), owned by nobody.
fn registered(shared_url: Option<&str>, neighbourhood: Option<(&str, &str)>) -> String {
    let uuid = Uuid::new_v4().to_string();
    let neighbourhood = neighbourhood.map(|(link_language, author)| {
        let mut expression = DecoratedNeighbourhoodExpression {
            author: author.into(),
            timestamp: "2026-11-01T10:00:00.000Z".into(),
            ..Default::default()
        };
        expression.data.link_language = link_language.into();
        expression
    });
    let handle = PerspectiveHandle {
        uuid: uuid.clone(),
        name: Some("a space".into()),
        shared_url: shared_url.map(str::to_string),
        neighbourhood,
        state: PerspectiveState::Synced,
        ..Default::default()
    };
    register_perspective(uuid.clone(), PerspectiveInstance::new(handle, None));
    uuid
}

/// Puts a language language in place that answers `expressionGet(address)` with `metas`'
/// meta for that address (else `null`), until dropped.
struct FakeLanguageLanguage {
    address: String,
    previous: Option<String>,
}

impl FakeLanguageLanguage {
    async fn answering(metas: Vec<(String, LanguageMeta)>) -> Self {
        let controller = LanguageController::global_instance();
        let address = format!("QmFakeLanguageLanguage{}", Uuid::new_v4().simple());
        let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel();
        controller
            .set_runtime_for_test(
                &address,
                Some(LanguageRuntimeHandle::from_channel(address.clone(), tx)),
            )
            .await;
        let previous = controller
            .system_addresses
            .lock()
            .await
            .language_language
            .replace(address.clone());
        tokio::spawn(async move {
            while let Some(request) = requests.recv().await {
                let LanguageOperation::Execute(script, _) = &request.operation else {
                    continue;
                };
                let answer = metas
                    .iter()
                    .find(|(asked, _)| script.contains(&format!("expressionGet(\"{asked}\")")))
                    .map(|(_, meta)| {
                        json!({ "author": "did:test:publisher", "data": meta }).to_string()
                    })
                    .unwrap_or_else(|| "null".to_string());
                let _ = request.response_tx.send(Ok(answer));
            }
        });
        FakeLanguageLanguage { address, previous }
    }

    async fn stop(self) {
        let controller = LanguageController::global_instance();
        controller.system_addresses.lock().await.language_language = self.previous;
        controller.set_runtime_for_test(&self.address, None).await;
    }
}

fn create_params(uuid: &str, sync: SyncChoice) -> CreateNeighbourhoodParams {
    CreateNeighbourhoodParams {
        perspective_uuid: uuid.into(),
        sync,
        rules: RulesInput::default(),
    }
}

#[test]
fn the_room_id_is_the_one_the_language_and_the_link_server_compute() {
    // The vectors of `template-uniqueness.test.ts` (server-link-language) and
    // `roomIdOf` in link-server/src/membership.ts.
    assert_eq!(
        room_id_of(CREATOR, "3f2a-uid"),
        "G6zF1MMHd8RamYLF7ZGBVpCbDhfigXNvt8mP9Dv5BBhY"
    );
    assert_ne!(room_id_of("ab", "c"), room_id_of("a", "bc"));
}

#[test]
fn the_server_link_template_and_its_languages_are_creator_templates() {
    assert!(is_creator_template(&server_link_meta(None)));
    assert!(is_server_link_template(&server_link_meta(None)));
    let made = server_link_meta(Some(json!({
        "CREATOR": CREATOR, "SERVER_URL": "https://l.example", "UID": "u",
    })));
    assert!(is_creator_template(&made));
    assert!(!is_server_link_template(&made));
    // A language made from it whose meta lost the declared params still names its creator.
    let bare = LanguageMeta {
        template_applied_params: Some(json!({ "CREATOR": CREATOR }).to_string()),
        ..Default::default()
    };
    assert!(is_creator_template(&bare));
    assert!(!is_creator_template(&p_diff_sync_meta()));
    assert!(!is_creator_template(&LanguageMeta::default()));
}

#[test]
fn a_server_link_language_names_its_server_room_and_creator() {
    let meta = server_link_meta(Some(json!({
        "CREATOR": CREATOR, "SERVER_URL": "https://l.example", "UID": "3f2a-uid",
    })));
    assert_eq!(
        choice_from_meta(
            &meta,
            "QmLink",
            "did:key:z6MkAuthor",
            "2026-11-01T10:00:00.000Z"
        ),
        LinkLanguageChoice {
            kind: ChoiceKind::LinkServer,
            link_language: "QmLink".into(),
            server_url: Some("https://l.example".into()),
            room_id: Some("G6zF1MMHd8RamYLF7ZGBVpCbDhfigXNvt8mP9Dv5BBhY".into()),
            creator: CREATOR.into(),
            fixed_at: "2026-11-01T10:00:00.000Z".into(),
        }
    );
    let holochain = choice_from_meta(&p_diff_sync_meta(), "QmDiff", "did:key:z6MkAuthor", "t");
    assert_eq!(holochain.kind, ChoiceKind::Holochain);
    assert_eq!(holochain.creator, "did:key:z6MkAuthor");
    assert_eq!((holochain.server_url, holochain.room_id), (None, None));
}

#[test]
fn the_wire_shapes_are_the_ones_the_sdk_sends_and_reads() {
    let params: CreateNeighbourhoodParams = serde_json::from_value(json!({
        "perspectiveUuid": "p",
        "sync": { "kind": "ownLinkServer", "url": "https://l.example" },
        "rules": { "add": "members", "join": "invite" },
    }))
    .unwrap();
    assert_eq!(
        params.sync,
        SyncChoice::OwnLinkServer {
            url: "https://l.example".into()
        }
    );
    assert_eq!(params.rules.add, SpaceAddRule::Members);
    assert_eq!(params.rules.join, JoinMode::Invite);
    for (sync, wire) in [
        (SyncChoice::Default, json!({ "kind": "default" })),
        (SyncChoice::Holochain, json!({ "kind": "holochain" })),
    ] {
        assert_eq!(serde_json::to_value(&sync).unwrap(), wire);
    }
    // Rules are required: no default on the wire (R56's default is the SDK's and MCP's).
    assert!(serde_json::from_value::<CreateNeighbourhoodParams>(json!({
        "perspectiveUuid": "p", "sync": { "kind": "default" },
    }))
    .is_err());
    assert_eq!(
        RulesInput::default(),
        RulesInput {
            add: SpaceAddRule::Admins,
            join: JoinMode::Open
        }
    );
}

#[tokio::test]
async fn holochain_is_coming_soon() {
    let error = create(
        create_params("no-such-perspective", SyncChoice::Holochain),
        &AgentContext::main_agent(),
    )
    .await
    .unwrap_err();
    assert!(matches!(error, SpaceError::ComingSoon(_)), "{error}");
    assert!(error.to_string().starts_with("coming-soon: "), "{error}");
}

#[tokio::test]
async fn a_missing_perspective_is_not_found() {
    let error = create(
        create_params("no-such-perspective", SyncChoice::Default),
        &AgentContext::main_agent(),
    )
    .await
    .unwrap_err();
    assert!(matches!(error, SpaceError::NotFound(_)), "{error}");
}

#[tokio::test]
async fn a_caller_without_a_person_needs_an_identity() {
    // The test main agent is a bare did:key: it speaks for no person.
    setup();
    let uuid = registered(None, None);
    let error = create(
        create_params(&uuid, SyncChoice::Default),
        &AgentContext::main_agent(),
    )
    .await
    .unwrap_err();
    assert!(matches!(error, SpaceError::NeedsIdentity(_)), "{error}");
    assert!(error.to_string().starts_with("needs-identity: "), "{error}");
    assert_eq!(recorded_choice(&uuid), Ok(None), "nothing recorded");
    unregister_perspective(&uuid);
}

#[tokio::test]
async fn the_choice_is_recorded_once_and_a_second_create_fails() {
    setup();
    let uuid = registered(None, None);
    let first = choice_from_meta(
        &server_link_meta(Some(json!({
            "CREATOR": CREATOR, "SERVER_URL": "https://l.example", "UID": "u1",
        }))),
        "QmFirst",
        CREATOR,
        "2026-11-01T10:00:00.000Z",
    );
    assert_eq!(record_choice(&uuid, &first), Ok(true));
    let second = LinkLanguageChoice {
        link_language: "QmSecond".into(),
        ..first.clone()
    };
    assert_eq!(record_choice(&uuid, &second), Ok(false));
    assert_eq!(recorded_choice(&uuid), Ok(Some(first.clone())));

    // A perspective with a recorded choice is a space: `create` refuses before it asks who
    // the caller is or makes anything.
    let error = create(
        create_params(&uuid, SyncChoice::Default),
        &AgentContext::main_agent(),
    )
    .await
    .unwrap_err();
    assert!(matches!(error, SpaceError::AlreadyCreated(_)), "{error}");
    assert_eq!(
        recorded_choice(&uuid),
        Ok(Some(first)),
        "the first choice stays"
    );
    unregister_perspective(&uuid);
}

#[tokio::test]
async fn a_shared_perspective_or_one_being_created_cannot_be_created_again() {
    setup();
    let shared = registered(Some("neighbourhood://QmShared"), None);
    let error = create(
        create_params(&shared, SyncChoice::Default),
        &AgentContext::main_agent(),
    )
    .await
    .unwrap_err();
    assert!(matches!(error, SpaceError::AlreadyCreated(_)), "{error}");
    assert!(
        error.to_string().contains("neighbourhood://QmShared"),
        "{error}"
    );
    unregister_perspective(&shared);

    let uuid = registered(None, None);
    let claim = CreateClaim::take(&uuid).expect("the first claim");
    let error = create(
        create_params(&uuid, SyncChoice::Default),
        &AgentContext::main_agent(),
    )
    .await
    .unwrap_err();
    assert!(matches!(error, SpaceError::AlreadyCreated(_)), "{error}");
    drop(claim);
    // Given back: the next create gets past the claim (and stops at the caller's identity).
    let error = create(
        create_params(&uuid, SyncChoice::Default),
        &AgentContext::main_agent(),
    )
    .await
    .unwrap_err();
    assert!(matches!(error, SpaceError::NeedsIdentity(_)), "{error}");
    unregister_perspective(&uuid);
}

#[tokio::test]
async fn publish_refuses_the_server_link_template_and_its_languages() {
    setup();
    let template = format!("QmServerLinkTemplate{}", Uuid::new_v4().simple());
    let made = format!("QmServerLinkMade{}", Uuid::new_v4().simple());
    let diff = format!("QmDiffSync{}", Uuid::new_v4().simple());
    let language_language = FakeLanguageLanguage::answering(vec![
        (template.clone(), server_link_meta(None)),
        (
            made.clone(),
            server_link_meta(Some(json!({
                "CREATOR": CREATOR, "SERVER_URL": "https://l.example", "UID": "u",
            }))),
        ),
        (diff.clone(), p_diff_sync_meta()),
    ])
    .await;

    for language in [&template, &made] {
        let refused = refuse_creator_template(language).await.unwrap_err();
        assert!(refused.contains("use neighbourhood.create"), "{refused}");
    }
    assert_eq!(refuse_creator_template(&diff).await, Ok(()));
    assert_eq!(refuse_creator_template("QmUnknown").await, Ok(()));

    // Through the method: 400 before anything is published.
    let uuid = registered(None, None);
    let answer = crate::mcp::tools::call_method_as(
        Arc::new(operator()),
        "neighbourhood.publish",
        json!({ "perspectiveUuid": uuid, "linkLanguage": template, "meta": { "links": [] } }),
    )
    .await
    .unwrap_err();
    assert_eq!(answer.code, 400, "{}", answer.message);
    assert!(
        answer.message.contains("use neighbourhood.create"),
        "{}",
        answer.message
    );
    let handle = crate::perspectives::get_perspective(&uuid)
        .unwrap()
        .persisted
        .lock()
        .await
        .clone();
    assert_eq!(handle.shared_url, None, "nothing was published");

    language_language.stop().await;
    unregister_perspective(&uuid);
}

#[tokio::test]
async fn the_mcp_publish_tool_makes_a_server_link_space_through_create() {
    use crate::mcp::tools::neighbourhoods::NeighbourhoodPublishParams;
    use crate::mcp::tools::test_support::{handler_as, status};
    use rmcp::handler::server::wrapper::Parameters;

    setup();
    let template = format!("QmServerLinkTemplate{}", Uuid::new_v4().simple());
    let language_language =
        FakeLanguageLanguage::answering(vec![(template.clone(), server_link_meta(None))]).await;
    let uuid = registered(None, None);
    let answer = handler_as(operator())
        .neighbourhood_publish_from_perspective(Parameters(NeighbourhoodPublishParams {
            perspective_uuid: uuid.clone(),
            link_language: template,
            name: "n".into(),
        }))
        .await;
    language_language.stop().await;
    unregister_perspective(&uuid);
    // `create`'s answer for a caller without a person, not a clone's failure.
    assert_eq!(status(&answer), Some(403), "{answer}");
    assert!(answer.contains("needs-identity"), "{answer}");
}

#[tokio::test]
async fn the_methods_answer_coming_soon_and_needs_identity_with_their_codes() {
    setup();
    let uuid = registered(None, None);
    let call = |sync: serde_json::Value| {
        crate::mcp::tools::call_method_as(
            Arc::new(operator()),
            "neighbourhood.create",
            json!({
                "perspectiveUuid": uuid,
                "sync": sync,
                "rules": { "add": "admins", "join": "open" },
            }),
        )
    };
    let holochain = call(json!({ "kind": "holochain" })).await.unwrap_err();
    assert_eq!(holochain.code, 501, "{}", holochain.message);
    assert!(
        holochain.message.starts_with("coming-soon"),
        "{}",
        holochain.message
    );
    let no_person = call(json!({ "kind": "default" })).await.unwrap_err();
    assert_eq!(no_person.code, 403, "{}", no_person.message);
    assert!(
        no_person.message.starts_with("needs-identity"),
        "{}",
        no_person.message
    );
    unregister_perspective(&uuid);
}

#[tokio::test]
async fn a_joined_space_gets_the_choice_its_link_language_names_recorded_on_first_read() {
    setup();
    let link_language = format!("QmJoinedLink{}", Uuid::new_v4().simple());
    let language_language = FakeLanguageLanguage::answering(vec![(
        link_language.clone(),
        server_link_meta(Some(json!({
            "CREATOR": CREATOR, "SERVER_URL": "https://l.example", "UID": "3f2a-uid",
        }))),
    )])
    .await;
    let uuid = registered(
        Some("neighbourhood://QmJoined"),
        Some((&link_language, "did:key:z6MkPublisher")),
    );
    let private = registered(None, None);

    let choice = link_language_choice(&uuid)
        .await
        .unwrap()
        .expect("a choice");
    assert_eq!(choice.kind, ChoiceKind::LinkServer);
    assert_eq!(choice.creator, CREATOR);
    assert_eq!(
        choice.room_id.as_deref(),
        Some("G6zF1MMHd8RamYLF7ZGBVpCbDhfigXNvt8mP9Dv5BBhY")
    );
    assert_eq!(choice.fixed_at, "2026-11-01T10:00:00.000Z");
    assert_eq!(link_language_choice(&private).await, Ok(None));

    // Recorded: read again without the language language.
    language_language.stop().await;
    assert_eq!(link_language_choice(&uuid).await, Ok(Some(choice.clone())));

    // In the list, as a space the caller joined (its person is not the creator).
    let holder = Holder {
        person: Some("did:scid:coasys:1:QmJoiner".into()),
        did: Some("did:key:z6MkJoiner".into()),
    };
    let perspectives = [&uuid, &private]
        .iter()
        .map(|u| crate::perspectives::get_perspective(u).unwrap())
        .collect();
    let holdings = list(perspectives, &holder).await;
    assert_eq!(
        holdings,
        vec![SpaceHolding {
            perspective_uuid: uuid.clone(),
            neighbourhood_url: "neighbourhood://QmJoined".into(),
            name: "a space".into(),
            how: HoldingHow::Joined,
            since: "2026-11-01T10:00:00.000Z".into(),
            choice,
            sync_state: PerspectiveState::Synced,
            my_role: None,
        }]
    );
    unregister_perspective(&uuid);
    unregister_perspective(&private);
}

#[test]
fn a_holding_is_made_by_its_creator_and_joined_otherwise() {
    let handle = PerspectiveHandle {
        uuid: "p".into(),
        name: None,
        shared_url: Some("neighbourhood://QmSpace".into()),
        neighbourhood: Some(DecoratedNeighbourhoodExpression {
            timestamp: "2026-11-01T09:00:00.000Z".into(),
            ..Default::default()
        }),
        state: PerspectiveState::LinkLanguageInstalledButNotSynced,
        ..Default::default()
    };
    let choice = LinkLanguageChoice {
        kind: ChoiceKind::LinkServer,
        link_language: "QmLink".into(),
        server_url: Some("https://l.example".into()),
        room_id: Some("room".into()),
        creator: CREATOR.into(),
        fixed_at: "2026-11-01T10:00:00.000Z".into(),
    };
    let members = vec![
        membership::MemberInfo {
            person: CREATOR.into(),
            role: MemberRole::Admin,
            bot_of: None,
            since: "2026-11-01T10:00:01.000Z".into(),
        },
        membership::MemberInfo {
            person: "did:scid:coasys:1:QmMember".into(),
            role: MemberRole::Member,
            bot_of: None,
            since: "2026-11-02T08:00:00.000Z".into(),
        },
    ];

    let creator = Holder {
        person: Some(CREATOR.into()),
        did: Some("did:key:z6MkCreatorDevice".into()),
    };
    let made = holding(&handle, choice.clone(), &members, &creator).unwrap();
    assert_eq!(made.how, HoldingHow::Made);
    assert_eq!(made.since, choice.fixed_at);
    assert_eq!(made.my_role, Some(MemberRole::Admin));
    assert_eq!(made.name, "neighbourhood://QmSpace", "no name: the URL");
    assert_eq!(
        made.sync_state,
        PerspectiveState::LinkLanguageInstalledButNotSynced
    );

    let member = Holder {
        person: Some("did:scid:coasys:1:QmMember".into()),
        did: None,
    };
    let joined = holding(&handle, choice.clone(), &members, &member).unwrap();
    assert_eq!(joined.how, HoldingHow::Joined);
    assert_eq!(joined.since, "2026-11-02T08:00:00.000Z");
    assert_eq!(joined.my_role, Some(MemberRole::Member));

    // A Holochain space's creator is the publishing key.
    let holochain = LinkLanguageChoice {
        kind: ChoiceKind::Holochain,
        server_url: None,
        room_id: None,
        creator: "did:key:z6MkCreatorDevice".into(),
        ..choice.clone()
    };
    assert_eq!(
        holding(&handle, holochain, &[], &creator).unwrap().how,
        HoldingHow::Made
    );

    let private = PerspectiveHandle {
        shared_url: None,
        ..handle.clone()
    };
    assert_eq!(holding(&private, choice, &members, &creator), None);
}

#[test]
fn every_error_names_its_code() {
    for (error, code) in [
        (SpaceError::ComingSoon("m".into()), "coming-soon: m"),
        (SpaceError::NeedsIdentity("m".into()), "needs-identity: m"),
        (SpaceError::AlreadyCreated("m".into()), "already-created: m"),
        (SpaceError::NotFound("m".into()), "not-found: m"),
        (SpaceError::Invalid("m".into()), "invalid: m"),
        (SpaceError::Unavailable("m".into()), "unavailable: m"),
        (SpaceError::Failed("m".into()), "failed: m"),
    ] {
        assert_eq!(error.to_string(), code);
    }
}
