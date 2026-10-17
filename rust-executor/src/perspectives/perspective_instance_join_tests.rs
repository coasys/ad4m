//! Joining a neighbourhood this executor already has must not wait on its
//! link language: telling it the new owner (`peersSetLocal`) is a Holochain
//! zome call in p-diff-sync, queued behind everything else that language's
//! runtime is doing.

use super::*;
use crate::agent::AgentService;
use crate::db::Ad4mDb;
use crate::languages::capability::{register_capabilities, remove_capabilities, Capability};
use crate::languages::language_runtime::LanguageOperation;
use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
use crate::languages::LanguageController;
use crate::test_utils::setup_wallet;
use std::collections::HashSet;

#[tokio::test(start_paused = true)]
async fn joining_an_existing_neighbourhood_does_not_wait_for_the_link_language() {
    setup_wallet();
    Ad4mDb::init_global_instance(":memory:").unwrap();
    AgentService::init_global_test_instance();
    let creator = "did:key:creator".to_string();
    let joiner = crate::agent::did();

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
        crate::neighbourhoods::install_neighbourhood_with_context(
            url,
            &crate::agent::AgentContext::main_agent(),
        ),
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
