//! Telling the link language which agents on this executor own the
//! perspective (`peersSetLocal`), so it reports them as local, not remote.
//!
//! Link languages differ here: p-diff-sync adds to its local list,
//! server-link-language replaces it. With a replacing language, whichever
//! registration lands last is what it reports, so registrations must not
//! overtake each other.

use super::PerspectiveInstance;
use crate::languages::language::Language;

impl PerspectiveInstance {
    /// Tell `language` this executor's owners (the main agent's DID for a
    /// perspective without any), unless it was last told exactly `told`.
    /// Returns the owners the language now has, or `None` if telling it
    /// failed, so the next registration tries again.
    ///
    /// Reads them under `local_agents_mutex` and holds it until the language
    /// answers, so no registration lands after a later one with an older
    /// list.
    pub(super) async fn register_local_agents(
        &self,
        language: &mut Language,
        told: Option<&[String]>,
    ) -> Option<Vec<String>> {
        let _registering = self.local_agents_mutex.lock().await;
        let agents = match self.persisted.lock().await.owners.clone() {
            Some(owners) if !owners.is_empty() => owners,
            _ => vec![crate::agent::did()],
        };
        if told == Some(agents.as_slice()) {
            return Some(agents);
        }
        log::info!("Setting local agents for link language: {:?}", agents);
        match language.set_local_agents(agents.clone()).await {
            Ok(_) => Some(agents),
            Err(e) => {
                log::error!("Failed to set local agents on link language: {:?}", e);
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::super::PerspectiveHandle;
    use super::*;
    use crate::agent::AgentService;
    use crate::db::Ad4mDb;
    use crate::languages::capability::{register_capabilities, remove_capabilities, Capability};
    use crate::languages::language_runtime::{LanguageOperation, LanguageRuntimeRequest};
    use crate::languages::language_runtime_handle::LanguageRuntimeHandle;
    use crate::languages::LanguageController;
    use crate::perspectives::update_perspective_with;
    use crate::test_utils::setup_wallet;
    use crate::types::PerspectiveState;
    use std::collections::HashSet;
    use std::time::Duration;
    use tokio::time::sleep;
    use uuid::Uuid;

    /// A `peersSetLocal` call the fake link language below has received and
    /// not yet answered.
    struct SetLocal {
        agents: Vec<String>,
        reply: tokio::sync::oneshot::Sender<Result<String, String>>,
    }

    impl SetLocal {
        fn of(request: LanguageRuntimeRequest) -> Self {
            let LanguageOperation::Execute(script, _) = request.operation else {
                panic!("not a script: {:?}", request.operation);
            };
            let agents = script
                .strip_prefix("await language.peersSetLocal(")
                .and_then(|s| s.strip_suffix(')'))
                .unwrap_or_else(|| panic!("not a peersSetLocal call: {}", script));
            SetLocal {
                agents: serde_json::from_str(agents).unwrap(),
                reply: request.response_tx,
            }
        }

        /// Apply it as server-link-language does: replace the local list.
        fn apply(self, local: &mut Vec<String>) {
            *local = self.agents;
            self.reply.send(Ok("null".to_string())).unwrap();
        }
    }

    /// Lal's schedule on #1439: B joins while attach registers [A], so the
    /// post-attach refresh sends [A, B]; C joins while that refresh is in
    /// flight. C's registration must not end up overwritten by [A, B].
    #[tokio::test(start_paused = true)]
    async fn a_join_during_the_owner_refresh_stays_local() {
        setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();
        AgentService::init_global_test_instance();
        let a = "did:key:owner-a".to_string();
        let b = "did:key:owner-b".to_string();
        // C joins through `install_neighbourhood_with_context` as the main agent.
        let c = crate::agent::did();

        // A link language served by this test instead of a runtime.
        let address = format!("QmReplacingPeers{}", Uuid::new_v4().simple());
        let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel();
        LanguageController::global_instance()
            .set_runtime_for_test(
                &address,
                Some(LanguageRuntimeHandle::from_channel(address.clone(), tx)),
            )
            .await;
        register_capabilities(&address, HashSet::from([Capability::PeersLocal]));

        let url = format!("neighbourhood://{}", Uuid::new_v4());
        let nh = crate::types::DecoratedNeighbourhoodExpression {
            data: crate::types::domain::Neighbourhood {
                link_language: address.clone(),
                ..Default::default()
            },
            ..Default::default()
        };
        let handle = PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("owners".to_string()),
            shared_url: Some(url.clone()),
            neighbourhood: Some(nh.clone()),
            state: PerspectiveState::NeighbourhoodJoinInitiated,
            owners: Some(vec![a.clone()]),
        };
        Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
        let instance = PerspectiveInstance::new(handle, None);
        crate::perspectives::register_perspective(instance.uuid.clone(), instance.clone());

        let attaching = instance.clone();
        let language = Language::new(address.clone());
        let attach =
            tokio::spawn(async move { attaching.attach_link_language(&nh, language).await });

        let mut local = Vec::new();
        let first = SetLocal::of(requests.recv().await.unwrap());
        assert_eq!(first.agents, [a.clone()]);
        // B joins now: no link language is exposed for its join to tell.
        update_perspective_with(&instance.uuid, |h| h.add_owner(&b))
            .await
            .unwrap();
        first.apply(&mut local);

        let refresh = SetLocal::of(requests.recv().await.unwrap());
        assert_eq!(refresh.agents, [a.clone(), b.clone()]);
        // C joins while the refresh is in flight.
        let join = tokio::spawn(async move {
            crate::neighbourhoods::install_neighbourhood_with_context(
                url,
                &crate::agent::AgentContext::main_agent(),
            )
            .await
        });
        // On the paused clock this returns once the join can get no further.
        sleep(Duration::from_millis(100)).await;

        // The runtime answers in arrival order; what reorders registrations
        // is the dispatch before it (`execute_on_language` awaits two locks),
        // which a test cannot stop in. Landing whatever the join sent before
        // the refresh still in flight stands in for that.
        let mut in_flight = vec![refresh];
        while let Ok(request) = requests.try_recv() {
            in_flight.push(SetLocal::of(request));
        }
        for request in in_flight.into_iter().rev() {
            request.apply(&mut local);
        }

        // Answer the rest in order until attach and the join are done.
        let done = async {
            attach.await.unwrap();
            join.await.unwrap().unwrap();
        };
        tokio::pin!(done);
        loop {
            tokio::select! {
                biased;
                Some(request) = requests.recv() => SetLocal::of(request).apply(&mut local),
                _ = &mut done => break,
            }
        }
        assert!(requests.try_recv().is_err());

        let owners = instance.persisted.lock().await.owners.clone().unwrap();
        assert_eq!(owners, [a, b, c]);
        assert_eq!(
            local, owners,
            "the link language's local agents after both registrations"
        );

        crate::perspectives::unregister_perspective(&instance.uuid);
        LanguageController::global_instance()
            .set_runtime_for_test(&address, None)
            .await;
        remove_capabilities(&address);
    }

    /// A registration the link language refused is not taken as done: the
    /// post-attach refresh sends the same owners again (CodeRabbit on #1439).
    #[tokio::test(start_paused = true)]
    async fn attach_retries_a_refused_registration() {
        setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();
        AgentService::init_global_test_instance();
        let a = "did:key:owner-a".to_string();

        let address = format!("QmRefusingPeers{}", Uuid::new_v4().simple());
        let (tx, mut requests) = tokio::sync::mpsc::unbounded_channel();
        LanguageController::global_instance()
            .set_runtime_for_test(
                &address,
                Some(LanguageRuntimeHandle::from_channel(address.clone(), tx)),
            )
            .await;
        register_capabilities(&address, HashSet::from([Capability::PeersLocal]));

        let nh = crate::types::DecoratedNeighbourhoodExpression {
            data: crate::types::domain::Neighbourhood {
                link_language: address.clone(),
                ..Default::default()
            },
            ..Default::default()
        };
        let handle = PerspectiveHandle {
            uuid: Uuid::new_v4().to_string(),
            name: Some("refused".to_string()),
            shared_url: Some(format!("neighbourhood://{}", Uuid::new_v4())),
            neighbourhood: Some(nh.clone()),
            state: PerspectiveState::NeighbourhoodJoinInitiated,
            owners: Some(vec![a.clone()]),
        };
        Ad4mDb::with_global_instance(|db| db.add_perspective(&handle)).unwrap();
        let instance = PerspectiveInstance::new(handle, None);

        let attaching = instance.clone();
        let language = Language::new(address.clone());
        let attach =
            tokio::spawn(async move { attaching.attach_link_language(&nh, language).await });

        let first = SetLocal::of(requests.recv().await.unwrap());
        assert_eq!(first.agents, [a.clone()]);
        first
            .reply
            .send(Err("peersSetLocal failed".to_string()))
            .unwrap();

        let mut local = Vec::new();
        let retry = tokio::time::timeout(Duration::from_secs(5), requests.recv())
            .await
            .expect("attach did not register the owners again")
            .unwrap();
        let retry = SetLocal::of(retry);
        assert_eq!(retry.agents, [a.clone()]);
        retry.apply(&mut local);
        attach.await.unwrap();
        assert!(requests.try_recv().is_err());
        assert_eq!(local, [a]);

        LanguageController::global_instance()
            .set_runtime_for_test(&address, None)
            .await;
        remove_capabilities(&address);
    }
}
