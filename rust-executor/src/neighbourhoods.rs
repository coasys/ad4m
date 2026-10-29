use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use uuid::Uuid;

use crate::agent::{did_for_context, AgentContext};
use crate::languages::LanguageController;
use crate::logging::span;
use crate::perspectives::{
    add_perspective, all_perspectives, get_perspective, update_perspective_with,
};
use crate::types::*;
use crate::types::{Neighbourhood, Perspective, PerspectiveHandle, PerspectiveState};

pub async fn _neighbourhood_publish_from_perspective(
    uuid: &str,
    link_language: String,
    meta: Perspective,
) -> Result<String, AnyError> {
    neighbourhood_publish_from_perspective_with_context(
        uuid,
        link_language,
        meta,
        &AgentContext::main_agent(),
    )
    .await
}

pub async fn neighbourhood_publish_from_perspective_with_context(
    uuid: &str,
    link_language: String,
    meta: Perspective,
    context: &AgentContext,
) -> Result<String, AnyError> {
    let _total = span(format!("publish {}", link_language));
    let perspective = get_perspective(uuid).ok_or(anyhow!("Perspective not found"))?;

    // The creator's link language loads before the URL exists: loading it is
    // what claims the neighbourhood's backing store (a server-link room makes
    // its first member the admin), so a joiner must not get there first. If
    // `apply_template` is still installing it, this waits for that install.
    let installing = span(format!("publish.linkLanguageInstall {}", link_language));
    LanguageController::install_language(link_language.clone()).await?;
    drop(installing);

    let neighbourhood = Neighbourhood {
        link_language,
        meta,
    };

    // Create neighbourhood with context
    let creating = span("publish.createNeighbourhood");
    let neighbourhood_address =
        LanguageController::create_neighbourhood_with_context(neighbourhood, context).await?;
    drop(creating);

    let neighbourhood_url = format!("neighbourhood://{}", neighbourhood_address);
    let reading = span("publish.readNeighbourhood");
    let neighbourhood_exp = LanguageController::get_neighbourhood(neighbourhood_address)
        .await?
        .ok_or(anyhow!("Could not retrieve NeigbourhoodExpression which was just created. Problem with Neighbourhood language"))?;
    drop(reading);

    // Add shared perspective to original perspective and then update controller.
    // Initialize owners list with the creator's DID if not already set
    let creator_did = did_for_context(context)?;
    let updating = span("publish.updateHandle");
    update_perspective_with(&perspective.uuid, |h| {
        h.shared_url = Some(neighbourhood_url.clone());
        h.neighbourhood = Some(neighbourhood_exp);
        h.state = PerspectiveState::NeighbourhoodCreationInitiated;
        if h.owners.is_none() {
            h.owners = Some(vec![creator_did.clone()]);
        } else if !h.is_owned_by(&creator_did) {
            h.add_owner(&creator_did);
        }
    })
    .await
    .map_err(|e| anyhow!(e))?;
    drop(updating);

    // Ensure any existing shared links are committed to the link language
    // This is critical for early links created before neighbourhood sharing
    // We need to do this after the neighbourhood is created but before other agents join
    perspective.ensure_public_links_are_shared().await;
    Ok(neighbourhood_url)
}

pub async fn _install_neighbourhood(url: String) -> Result<PerspectiveHandle, AnyError> {
    install_neighbourhood_with_context(url, &crate::agent::AgentContext::main_agent()).await
}

/// Tell `perspective`'s link language its owners without waiting for it.
/// In p-diff-sync that is a Holochain zome call queued behind everything else
/// the language's runtime is doing, which on a busy executor holds a join RPC
/// past the client's timeout. The join is already recorded (database and
/// owners list); `update_local_agents` reads the owners when it sends, logs a
/// failure, and the next registration (another join, or the link language's
/// own attach) sends the full owners list again.
fn register_owners_in_background(
    perspective: &crate::perspectives::perspective_instance::PerspectiveInstance,
) {
    let perspective = perspective.clone();
    tokio::spawn(async move { perspective.update_local_agents().await });
}

pub async fn install_neighbourhood_with_context(
    url: String,
    context: &crate::agent::AgentContext,
) -> Result<PerspectiveHandle, AnyError> {
    let _total = span(format!("join {}", url));
    let perspectives = all_perspectives();

    // Check if neighbourhood already exists
    for p in perspectives.iter() {
        let handle = p.persisted.lock().await.clone();
        if handle.shared_url == Some(url.clone()) {
            // Neighbourhood exists - add this user as owner if it's a user context
            log::info!(
                "Adding agent {:?} to existing neighbourhood {}",
                context,
                url
            );
            if let Some(user_did) = context.managed_did().map(str::to_string) {
                // Update database
                crate::db::Ad4mDb::with_global_instance(|db| {
                    db.add_owner_to_neighbourhood(&url, &user_did)
                })?;

                // Add user to owners list, under the perspective's lock so
                // concurrent joins cannot overwrite each other's owner
                let handle = update_perspective_with(&p.uuid, |h| h.add_owner(&user_did))
                    .await
                    .map_err(|e| anyhow!(e))?;

                // Update link language with new owners list
                register_owners_in_background(p);

                log::info!(
                    "Added agent {} to existing neighbourhood {}. Registering the new owners with the link language in the background.",
                    user_did,
                    url
                );
                return Ok(handle.clone());
            } else {
                // Main agent trying to join existing neighbourhood
                // Add main agent to owners list for access control
                let main_agent_did = crate::agent::did_for_context(context)?;

                // Update database
                crate::db::Ad4mDb::with_global_instance(|db| {
                    db.add_owner_to_neighbourhood(&url, &main_agent_did)
                })?;

                // Add main agent to owners list, under the perspective's lock
                let handle = update_perspective_with(&p.uuid, |h| h.add_owner(&main_agent_did))
                    .await
                    .map_err(|e| anyhow!(e))?;

                // Update link language with new owners list
                register_owners_in_background(p);

                log::info!(
                    "Added main agent to existing neighbourhood {}. Registering the new owners with the link language in the background.",
                    url
                );
                return Ok(handle.clone());
            }
        }
    }

    let expression_ref = ExpressionRef::try_from(url.to_string())?;
    let neighbourhood_exp =
        LanguageController::get_neighbourhood(expression_ref.expression).await?;
    if neighbourhood_exp.is_none() {
        return Err(anyhow!("Could not find neighbourhood with URL {}", url));
    }
    log::info!(
        "Core.install_neighbourhood(): Got neighbourhood {:?}",
        neighbourhood_exp
    );
    let neighbourhood = neighbourhood_exp.unwrap();

    // The link language is installed by the new perspective's
    // `ensure_link_language`, not here: fetching a language another executor
    // has just published can take until its deadline, and loading a Holochain
    // one installs an app. The state goes on to LinkLanguageInstalledButNotSynced
    // and Synced, or LinkLanguageFailedToInstall, and each change is an event.
    let state = PerspectiveState::NeighbourhoodJoinInitiated;

    log::info!(
        "Core.install_neighbourhood(): Creating perspective {}, {:?}, {:?}",
        url,
        neighbourhood,
        state
    );

    let owner_did = did_for_context(context)?;

    let handle = PerspectiveHandle {
        uuid: Uuid::new_v4().to_string(),
        name: Some(url.clone()),
        shared_url: Some(url.clone()),
        neighbourhood: Some(neighbourhood.clone()),
        state,
        owners: Some(vec![owner_did.clone()]), // Initialize owners list with creator
    };
    add_perspective(handle.clone(), Some(true))
        .await
        .map_err(|e| anyhow!(e))?;

    log::info!(
        "Created new perspective for neighbourhood {}. Link language will handle DID mapping when accessed by user {}",
        url,
        owner_did
    );

    Ok(handle)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::AgentService;
    use crate::db::Ad4mDb;

    /// Users joining one neighbourhood at the same moment each add an owner;
    /// none may be lost, in memory or in the database (staging, 16 Oct: one
    /// of three joiners got 403 on its own perspective).
    #[tokio::test(flavor = "multi_thread", worker_threads = 8)]
    async fn concurrent_joins_keep_every_owner() {
        crate::test_utils::setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();
        AgentService::init_global_test_instance();
        let url = "neighbourhood://concurrent-joins".to_string();
        let mut handle = PerspectiveHandle::new_from_name("joined by many".to_string());
        handle.shared_url = Some(url.clone());
        let link_language = "concurrent-joins-link-language".to_string();
        handle.neighbourhood = Some(DecoratedNeighbourhoodExpression {
            author: "did:test:creator".to_string(),
            timestamp: "0".to_string(),
            data: crate::types::domain::Neighbourhood {
                link_language: link_language.clone(),
                meta: crate::types::domain::Perspective::default(),
            },
            proof: DecoratedExpressionProof::default(),
        });
        add_perspective(handle.clone(), None).await.unwrap();

        let emails: Vec<String> = (0..50).map(|i| format!("joiner{i}@test")).collect();
        let dids: Vec<String> = emails
            .iter()
            .map(|email| crate::test_utils::create_test_user(email))
            .collect();
        let joins = dids.iter().cloned().map(|did| {
            let url = url.clone();
            tokio::spawn(async move {
                let context = AgentContext::managed(did);
                install_neighbourhood_with_context(url, &context)
                    .await
                    .unwrap();
            })
        });
        for joined in futures::future::join_all(joins).await {
            joined.unwrap();
        }

        let in_memory = get_perspective(&handle.uuid)
            .unwrap()
            .persisted
            .lock()
            .await
            .clone();
        let stored = Ad4mDb::with_global_instance(|db| db.get_all_perspectives())
            .unwrap()
            .into_iter()
            .find(|s| s.uuid == handle.uuid)
            .unwrap();
        // Signals reach owners through the link language's cached handle.
        let cached = crate::perspectives::cached_perspective_handle(&link_language).unwrap();
        for (email, did) in emails.iter().zip(&dids) {
            assert!(
                in_memory.owners.as_ref().unwrap().contains(&did),
                "lost {email} in memory"
            );
            assert!(
                stored.owners.as_ref().unwrap().contains(&did),
                "lost {email} in the db"
            );
            assert!(
                cached.owners.as_ref().unwrap().contains(&did),
                "lost {email} in the signal routing cache"
            );
        }
        // Other tests expect the global perspective map to start empty.
        crate::perspectives::remove_perspective(&handle.uuid).await;
    }

    /// The main agent joining a neighbourhood a user already holds becomes an owner beside
    /// the user, in memory and in the database.
    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn the_main_agent_joining_keeps_the_existing_owner() {
        crate::test_utils::setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();
        AgentService::init_global_test_instance();
        let url = "neighbourhood://main-agent-joins".to_string();
        let user = crate::test_utils::create_test_user("held-by-user@test");
        let mut handle = PerspectiveHandle::new_from_name("held by a user".to_string());
        handle.shared_url = Some(url.clone());
        handle.owners = Some(vec![user.clone()]);
        add_perspective(handle.clone(), None).await.unwrap();

        install_neighbourhood_with_context(url, &AgentContext::main_agent())
            .await
            .unwrap();

        let main = did_for_context(&AgentContext::main_agent()).unwrap();
        let in_memory = get_perspective(&handle.uuid)
            .unwrap()
            .persisted
            .lock()
            .await
            .clone();
        let stored = Ad4mDb::with_global_instance(|db| db.get_all_perspectives())
            .unwrap()
            .into_iter()
            .find(|s| s.uuid == handle.uuid)
            .unwrap();
        for owners in [in_memory.owners.unwrap(), stored.owners.unwrap()] {
            assert!(owners.contains(&user), "the user stays an owner");
            assert!(owners.contains(&main), "the main agent becomes an owner");
        }
        crate::perspectives::remove_perspective(&handle.uuid).await;
    }

    /// The link language's install sets the perspective's state while users join (CI,
    /// 18 Oct: #1439's e10 waited 60 s for a state a join had written back over). Neither
    /// change may overwrite the other.
    #[tokio::test(flavor = "multi_thread", worker_threads = 8)]
    async fn a_state_change_during_joins_keeps_the_state_and_every_owner() {
        crate::test_utils::setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();
        AgentService::init_global_test_instance();
        let url = "neighbourhood://state-during-joins".to_string();
        let mut handle = PerspectiveHandle::new_from_name("installs while joined".to_string());
        handle.shared_url = Some(url.clone());
        handle.state = PerspectiveState::NeighbourhoodJoinInitiated;
        add_perspective(handle.clone(), None).await.unwrap();
        let p = get_perspective(&handle.uuid).unwrap();

        let emails: Vec<String> = (0..50).map(|i| format!("stater{i}@test")).collect();
        let dids: Vec<String> = emails
            .iter()
            .map(|email| crate::test_utils::create_test_user(email))
            .collect();
        let mut tasks: Vec<_> = dids
            .iter()
            .cloned()
            .map(|did| {
                let url = url.clone();
                tokio::spawn(async move {
                    let context = AgentContext::managed(did);
                    install_neighbourhood_with_context(url, &context)
                        .await
                        .unwrap();
                })
            })
            .collect();
        let installer = p.clone();
        tasks.insert(
            25,
            tokio::spawn(async move {
                installer
                    .update_perspective_state(PerspectiveState::LinkLanguageInstalledButNotSynced)
                    .await
                    .unwrap();
            }),
        );
        for task in futures::future::join_all(tasks).await {
            task.unwrap();
        }

        let persisted = p.persisted.lock().await.clone();
        assert_eq!(
            persisted.state,
            PerspectiveState::LinkLanguageInstalledButNotSynced,
            "a join wrote back a stale state"
        );
        for (email, did) in emails.iter().zip(&dids) {
            assert!(
                persisted.owners.as_ref().unwrap().contains(&did),
                "lost {email}"
            );
        }
        crate::perspectives::remove_perspective(&handle.uuid).await;
    }
}
