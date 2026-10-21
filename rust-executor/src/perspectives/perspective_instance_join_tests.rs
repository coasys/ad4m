//! Publishing and joining a neighbourhood must not wait on its link language:
//! telling it a new owner (`peersSetLocal`) is a Holochain zome call in
//! p-diff-sync, queued behind everything else that language's runtime is
//! doing; fetching one another executor has just published can take until
//! its deadline; loading one installs a Holochain app. The perspective's
//! `ensure_link_language` does those, and its state reports them.

use super::*;
use crate::agent::{AgentContext, AgentService};
use crate::db::Ad4mDb;
use crate::languages::capability::{register_capabilities, remove_capabilities, Capability};
use crate::languages::language_runtime::LanguageOperation;
use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
use crate::languages::LanguageController;
use crate::test_utils::setup_wallet;
use std::collections::HashSet;

/// Join a neighbourhood this executor already has, as `context`, against a
/// link language that never answers: the join returns, `joiner` is an owner,
/// and the registration still reaches the language afterwards.
async fn join_returns_before_the_link_language(context: AgentContext, joiner: String) {
    let creator = "did:key:creator".to_string();

    // A link language served by this test, which never answers.
    let address = format!("QmSilentPeers{}", Uuid::new_v4().simple());
    let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel();
    LanguageController::global_instance()
        .set_runtime_for_test(
            &address,
            Some(LanguageRuntimeHandle::from_channel(address.clone(), tx)),
        )
        .await;
    register_capabilities(&address, HashSet::from([Capability::PeersLocal]));

    let url = format!("neighbourhood://{}", Uuid::new_v4());
    let handle = PerspectiveHandle {
        uuid: Uuid::new_v4().to_string(),
        name: Some("joined".to_string()),
        shared_url: Some(url.clone()),
        neighbourhood: Some(crate::types::DecoratedNeighbourhoodExpression {
            data: crate::types::domain::Neighbourhood {
                link_language: address.clone(),
                ..Default::default()
            },
            ..Default::default()
        }),
        state: PerspectiveState::Synced,
        owners: Some(vec![creator.clone()]),
    };
    Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
    let instance = PerspectiveInstance::new(handle, None);
    *instance.link_language.write().await = Some(Language::new(address.clone()));
    crate::perspectives::register_perspective(instance.uuid.clone(), instance.clone());

    let joined = tokio::time::timeout(
        Duration::from_secs(5),
        crate::neighbourhoods::install_neighbourhood_with_context(url, &context),
    )
    .await
    .expect("the join waited on the link language")
    .unwrap();
    assert_eq!(joined.owners, Some(vec![creator.clone(), joiner.clone()]));

    // The registration still reaches the language.
    let request = tokio::time::timeout(Duration::from_secs(5), requests.recv())
        .await
        .expect("the new owner was never registered with the link language")
        .unwrap();
    let LanguageOperation::Execute(script, _) = request.operation else {
        panic!("not a script: {:?}", request.operation);
    };
    assert_eq!(
        script,
        format!(
            "await language.peersSetLocal({})",
            serde_json::to_string(&[creator, joiner]).unwrap()
        )
    );

    crate::perspectives::unregister_perspective(&instance.uuid);
    LanguageController::global_instance()
        .set_runtime_for_test(&address, None)
        .await;
    remove_capabilities(&address);
}

#[tokio::test(start_paused = true)]
async fn the_main_agent_joining_does_not_wait_for_the_link_language() {
    setup_wallet();
    Ad4mDb::init_global_instance(":memory:").unwrap();
    AgentService::init_global_test_instance();
    join_returns_before_the_link_language(AgentContext::main_agent(), crate::agent::did()).await;
}

/// The branch a managed user's join takes, which is what staging hit.
#[tokio::test(start_paused = true)]
async fn a_user_joining_does_not_wait_for_the_link_language() {
    setup_wallet();
    crate::test_utils::setup_agent();
    Ad4mDb::init_global_instance(":memory:").unwrap();
    let email = format!("joiner.{}@example.org", Uuid::new_v4());
    let did = crate::test_utils::create_test_user(&email);

    join_returns_before_the_link_language(AgentContext::managed(did.clone()), did).await;

    let profile = AgentService::with_global_instance(|a| a.user_profile_path(&email)).unwrap();
    if let Some(user_dir) = profile.parent() {
        let _ = std::fs::remove_dir_all(user_dir);
    }
}

/// Serve `address` from this test, as the system language `role` names:
/// `answer` gives the reply to a script, or `None` for one never answered.
async fn serve_system_language(
    role: fn(&mut crate::languages::SystemLanguageAddresses) -> &mut Option<String>,
    answer: fn(&str) -> Option<String>,
) -> (String, Option<String>) {
    let controller = LanguageController::global_instance();
    let address = format!("QmFakeSystemLanguage{}", Uuid::new_v4().simple());
    let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel::<
        crate::languages::language_runtime::LanguageRuntimeRequest,
    >();
    controller
        .set_runtime_for_test(
            &address,
            Some(LanguageRuntimeHandle::from_channel(address.clone(), tx)),
        )
        .await;
    tokio::spawn(async move {
        let mut unanswered = Vec::new();
        while let Some(request) = requests.recv().await {
            let LanguageOperation::Execute(script, _) = &request.operation else {
                continue;
            };
            match answer(script) {
                Some(reply) => {
                    let _ = request.response_tx.send(Ok(reply));
                }
                None => unanswered.push(request),
            }
        }
    });
    let previous = role(&mut *controller.system_addresses.lock().await).replace(address.clone());
    (address, previous)
}

async fn stop_serving_system_language(
    role: fn(&mut crate::languages::SystemLanguageAddresses) -> &mut Option<String>,
    (address, previous): (String, Option<String>),
) {
    let controller = LanguageController::global_instance();
    *role(&mut *controller.system_addresses.lock().await) = previous;
    controller.set_runtime_for_test(&address, None).await;
}

fn neighbourhood_language(
    sys: &mut crate::languages::SystemLanguageAddresses,
) -> &mut Option<String> {
    &mut sys.neighbourhood_language
}

fn language_language(sys: &mut crate::languages::SystemLanguageAddresses) -> &mut Option<String> {
    &mut sys.language_language
}

/// A neighbourhood store holding one neighbourhood, whose link language is
/// `QmLinkLanguageNobodyHas`.
fn one_neighbourhood(script: &str) -> Option<String> {
    Some(if script.contains("isImmutableExpression") {
        "false".to_string()
    } else if script.contains("expressionCreate") {
        "\"QmTheNeighbourhood\"".to_string()
    } else if script.contains("expressionGet") {
        serde_json::to_string(&crate::types::DecoratedNeighbourhoodExpression {
            data: crate::types::domain::Neighbourhood {
                link_language: "QmLinkLanguageNobodyHas".to_string(),
                ..Default::default()
            },
            ..Default::default()
        })
        .unwrap()
    } else {
        "null".to_string()
    })
}

/// A first join on this executor returns at once, in NeighbourhoodJoinInitiated,
/// while the language language has not answered for its link language (it
/// used to wait for the install: up to 30 s for the source, more for the meta).
#[tokio::test(start_paused = true)]
async fn a_first_join_does_not_wait_for_the_link_language_install() {
    setup_wallet();
    Ad4mDb::init_global_instance(":memory:").unwrap();
    AgentService::init_global_test_instance();
    let nh = serve_system_language(neighbourhood_language, one_neighbourhood).await;
    let ll = serve_system_language(language_language, |_| None).await;

    let joined = tokio::time::timeout(
        Duration::from_secs(1),
        crate::neighbourhoods::install_neighbourhood_with_context(
            format!("neighbourhood://QmJoined{}", Uuid::new_v4().simple()),
            &AgentContext::main_agent(),
        ),
    )
    .await;

    stop_serving_system_language(language_language, ll).await;
    stop_serving_system_language(neighbourhood_language, nh).await;
    let joined = joined
        .expect("the join waited for the link language")
        .unwrap();
    assert_eq!(joined.state, PerspectiveState::NeighbourhoodJoinInitiated);
    crate::perspectives::remove_perspective(&joined.uuid).await;
}

/// Publishing names a link language that is on disk without loading it:
/// loading one starts its runtime and, for Holochain, installs its app. The
/// perspective's `ensure_link_language` loads it afterwards.
#[tokio::test(start_paused = true)]
async fn publishing_does_not_load_the_link_language() {
    setup_wallet();
    Ad4mDb::init_global_instance(":memory:").unwrap();
    AgentService::init_global_test_instance();
    let controller = LanguageController::global_instance();
    let bundle = format!("export const name = 'never loaded {}';", Uuid::new_v4());
    let link_language = controller.calculate_language_hash(&bundle);
    let dir = crate::test_utils::test_languages_directory().join(&link_language);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("bundle.js"), &bundle).unwrap();

    let handle = PerspectiveHandle::new_from_name("to publish".to_string());
    Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
    let instance = PerspectiveInstance::new(handle, None);
    crate::perspectives::register_perspective(instance.uuid.clone(), instance.clone());
    let nh = serve_system_language(neighbourhood_language, one_neighbourhood).await;

    let published = tokio::time::timeout(
        Duration::from_secs(1),
        crate::neighbourhoods::neighbourhood_publish_from_perspective_with_context(
            &instance.uuid,
            link_language.clone(),
            Perspective::default(),
            &AgentContext::main_agent(),
        ),
    )
    .await;

    stop_serving_system_language(neighbourhood_language, nh).await;
    crate::perspectives::unregister_perspective(&instance.uuid);
    let url = published
        .expect("the publish waited for the link language")
        .unwrap();
    assert_eq!(url, "neighbourhood://QmTheNeighbourhood");
    assert!(!controller.is_language_loaded(&link_language).await);
    let handle = instance.persisted.lock().await.clone();
    assert_eq!(
        handle.state,
        PerspectiveState::NeighbourhoodCreationInitiated
    );
}
