use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use uuid::Uuid;

use crate::agent::{did_for_context, AgentContext};
use crate::graphql::graphql_types::{
    Neighbourhood, Perspective, PerspectiveHandle, PerspectiveState,
};
use crate::languages::LanguageController;
use crate::perspectives::{add_perspective, all_perspectives, get_perspective, update_perspective};
use crate::types::*;

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

pub async fn install_neighbourhood_with_context(
    url: String,
    context: &crate::agent::AgentContext,
) -> Result<PerspectiveHandle, AnyError> {
    let perspectives = all_perspectives();

    // Check if neighbourhood already exists
    for p in perspectives.iter() {
        let mut handle = p.persisted.lock().await.clone();
        if handle.shared_url == Some(url.clone()) {
            // Neighbourhood exists - add this user as owner if it's a user context
            log::info!(
                "Adding user {:?} to existing neighbourhood {}",
                context.user_email,
                url
            );
            if let Some(user_email) = &context.user_email {
                let user_did = crate::agent::AgentService::get_user_did_by_email(user_email)?;

                // Update database
                crate::db::Ad4mDb::with_global_instance(|db| {
                    db.add_owner_to_neighbourhood(&url, &user_did)
                })?;

                // Add user to owners list
                // Update in-memory handle
                handle.add_owner(&user_did);

                update_perspective(&handle).await.map_err(|e| anyhow!(e))?;

                // Update link language with new owners list
                if let Some(owners) = &handle.owners {
                    p.update_local_agents(owners.clone()).await;
                }

                log::info!(
                    "Added user {} to existing neighbourhood {}. Link language has been updated with new owners.",
                    user_email,
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

                // Add main agent to owners list in memory
                handle.add_owner(&main_agent_did);

                update_perspective(&handle).await.map_err(|e| anyhow!(e))?;

                // Update link language with new owners list
                if let Some(owners) = &handle.owners {
                    p.update_local_agents(owners.clone()).await;
                }

                log::info!(
                    "Added main agent to existing neighbourhood {}. Link language has been updated with new owners.",
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
