use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use uuid::Uuid;

use crate::agent::{did_for_context, AgentContext};
use crate::languages::LanguageController;
use crate::perspectives::perspective_instance::PerspectiveInstance;
use crate::perspectives::{
    add_perspective, all_perspectives, get_perspective, update_perspective, update_perspective_with,
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
    let perspective = get_perspective(uuid).ok_or(anyhow!("Perspective not found"))?;

    LanguageController::install_language(link_language.clone()).await?;

    let neighbourhood = Neighbourhood {
        link_language,
        meta,
    };

    // Create neighbourhood with context
    let neighbourhood_address =
        LanguageController::create_neighbourhood_with_context(neighbourhood, context).await?;

    let neighbourhood_url = format!("neighbourhood://{}", neighbourhood_address);
    let neighbourhood_exp = LanguageController::get_neighbourhood(neighbourhood_address)
        .await?
        .ok_or(anyhow!("Could not retrieve NeigbourhoodExpression which was just created. Problem with Neighbourhood language"))?;

    let mut perspective_handle = perspective.persisted.lock().await.clone();
    // Add shared perspective to original perspective and then update controller
    perspective_handle.shared_url = Some(neighbourhood_url.clone());
    perspective_handle.neighbourhood = Some(neighbourhood_exp);
    perspective_handle.state = PerspectiveState::NeighbourhoodCreationInitiated;

    // Initialize owners list with the creator's DID if not already set
    let creator_did = did_for_context(context)?;
    if perspective_handle.owners.is_none() {
        perspective_handle.owners = Some(vec![creator_did.clone()]);
    } else if !perspective_handle.is_owned_by(&creator_did) {
        perspective_handle.add_owner(&creator_did);
    }

    update_perspective(&perspective_handle)
        .await
        .map_err(|e| anyhow!(e))?;

    // Ensure any existing shared links are committed to the link language
    // This is critical for early links created before neighbourhood sharing
    // We need to do this after the neighbourhood is created but before other agents join
    perspective.ensure_public_links_are_shared().await;
    Ok(neighbourhood_url)
}

pub async fn _install_neighbourhood(url: String) -> Result<PerspectiveHandle, AnyError> {
    install_neighbourhood_with_context(url, &crate::agent::AgentContext::main_agent()).await
}

/// Adds `did` as an owner of the neighbourhood perspective `p` and tells its
/// link language the new owners.
async fn add_owner(
    p: &PerspectiveInstance,
    url: &str,
    did: &str,
) -> Result<PerspectiveHandle, AnyError> {
    crate::db::Ad4mDb::with_global_instance(|db| db.add_owner_to_neighbourhood(url, did))?;
    let handle = update_perspective_with(&p.uuid, |h| h.add_owner(did))
        .await
        .map_err(|e| anyhow!(e))?;
    if let Some(owners) = &handle.owners {
        p.update_local_agents(owners.clone()).await;
    }
    Ok(handle)
}

pub async fn install_neighbourhood_with_context(
    url: String,
    context: &crate::agent::AgentContext,
) -> Result<PerspectiveHandle, AnyError> {
    let perspectives = all_perspectives();

    // Check if neighbourhood already exists
    for p in perspectives.iter() {
        let handle = p.persisted.lock().await.clone();
        if handle.shared_url == Some(url.clone()) {
            // Neighbourhood exists: add the joining agent as an owner.
            let did = match &context.user_email {
                Some(user_email) => crate::agent::AgentService::get_user_did_by_email(user_email)?,
                None => crate::agent::did_for_context(context)?,
            };
            let handle = add_owner(p, &url, &did).await?;
            log::info!(
                "Added {} to existing neighbourhood {}. Link language has been updated with new owners.",
                did,
                url
            );
            return Ok(handle);
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

    // Install the link language before checking its availability.
    // This fetches the bundle from the language-language and loads it on the JS side.
    if let Err(e) =
        LanguageController::install_language(neighbourhood.data.link_language.clone()).await
    {
        log::warn!(
            "Failed to install link language {}: {}",
            neighbourhood.data.link_language,
            e
        );
    }

    let state = if LanguageController::language_by_address(neighbourhood.data.link_language.clone())
        .await?
        .is_some()
    {
        PerspectiveState::LinkLanguageInstalledButNotSynced
    } else {
        PerspectiveState::LinkLanguageFailedToInstall
    };

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
    use crate::db::Ad4mDb;

    /// Users joining one neighbourhood at the same moment each add an owner;
    /// none may be lost, in memory or in the database (staging, 16 Oct: one
    /// of three joiners got 403 on its own perspective).
    #[tokio::test(flavor = "multi_thread", worker_threads = 8)]
    async fn concurrent_joins_keep_every_owner() {
        crate::test_utils::setup_wallet();
        Ad4mDb::init_global_instance(":memory:").unwrap();
        crate::agent::AgentService::init_global_test_instance();
        let url = "neighbourhood://concurrent-joins";
        let mut handle = PerspectiveHandle::new_from_name("joined by many".to_string());
        handle.shared_url = Some(url.to_string());
        add_perspective(handle.clone(), None).await.unwrap();
        let p: PerspectiveInstance = get_perspective(&handle.uuid).unwrap();

        let dids: Vec<String> = (0..100).map(|i| format!("did:key:z6Mkjoiner{i}")).collect();
        let joins = dids.iter().cloned().map(|did| {
            let p = p.clone();
            tokio::spawn(async move { add_owner(&p, url, &did).await.unwrap() })
        });
        futures::future::join_all(joins).await;

        let in_memory = p.persisted.lock().await.clone();
        let stored = Ad4mDb::with_global_instance(|db| db.get_all_perspectives())
            .unwrap()
            .into_iter()
            .find(|s| s.uuid == handle.uuid)
            .unwrap();
        for did in &dids {
            assert!(
                in_memory.owners.as_ref().unwrap().contains(did),
                "lost {did} in memory"
            );
            assert!(
                stored.owners.as_ref().unwrap().contains(did),
                "lost {did} in the db"
            );
        }
    }
}
