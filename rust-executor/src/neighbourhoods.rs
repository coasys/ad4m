use deno_core::anyhow::anyhow;
use deno_core::error::AnyError;
use uuid::Uuid;

use crate::graphql::graphql_types::{
    Neighbourhood, Perspective, PerspectiveHandle, PerspectiveState,
};
use crate::languages::LanguageController;
use crate::perspectives::{add_perspective, all_perspectives, get_perspective, update_perspective};
use crate::types::*;

pub async fn neighbourhood_publish_from_perspective(
    uuid: &str,
    link_language: String,
    meta: Perspective,
) -> Result<String, AnyError> {
    let perspective = get_perspective(uuid).ok_or(anyhow!("Perspective not found"))?;

    LanguageController::install_language(link_language.clone()).await?;

    let neighbourhood = Neighbourhood {
        link_language,
        meta,
    };

    // Create neighbourhood
    let neighbourhood_address = LanguageController::create_neighbourhood(neighbourhood).await?;

    let neighbourhood_url = format!("neighbourhood://{}", neighbourhood_address);
    let neighbourhood_exp = LanguageController::get_neighbourhood(neighbourhood_address)
        .await?
        .ok_or(anyhow!("Could not retrieve NeigbourhoodExpression which was just created. Problem with Neighbourhood language"))?;

    let mut perspective_handle = perspective.persisted.lock().await.clone();
    // Add shared perspective to original perspective and then update controller
    perspective_handle.shared_url = Some(neighbourhood_url.clone());
    perspective_handle.neighbourhood = Some(neighbourhood_exp);
    perspective_handle.state = PerspectiveState::Synced;
    update_perspective(&perspective_handle)
        .await
        .map_err(|e| anyhow!(e))?;
    Ok(neighbourhood_url)
}

pub async fn install_neighbourhood(url: String) -> Result<PerspectiveHandle, AnyError> {
    let perspectives = all_perspectives();

    for p in perspectives.iter() {
        let handle = p.persisted.lock().await;
        if handle.shared_url == Some(url.clone()) {
            return Err(anyhow!("Neighbourhood with URL {} already installed", url));
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

    let handle = PerspectiveHandle {
        uuid: Uuid::new_v4().to_string(),
        name: Some(url.clone()),
        shared_url: Some(url.clone()),
        neighbourhood: Some(neighbourhood),
        state,
    };
    add_perspective(handle.clone(), Some(true))
        .await
        .map_err(|e| anyhow!(e))?;

    Ok(handle)
}
