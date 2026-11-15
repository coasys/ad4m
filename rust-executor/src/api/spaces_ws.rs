//! Spaces by person (`crate::spaces`, slice 3.B part A8): `neighbourhood.create`,
//! `neighbourhood.linkLanguageChoice` and `spaces.list`.
//!
//! The two `neighbourhood.*` methods name a perspective and declare `Guard::Owns` on it, as
//! `neighbourhood.publish` does. `spaces.list` names none: it answers the perspectives the
//! caller may read, as `perspective.all` does.

use serde_json::Value;
use std::sync::Arc;

use crate::agent::capabilities::*;
use crate::helpers::can_access_perspective_with_did;
use crate::spaces::{
    self, CreateNeighbourhoodParams, CreateNeighbourhoodResult, Holder, LinkLanguageChoice,
    LinkLanguageChoiceParams, SpaceError, SpaceHolding,
};
use crate::types::RequestContext;

use super::guards::{Guard, IdParam, Owner};
use super::ws_handler::{HandlerMap, NoParams, WsRpcError};

fn neighbourhood_create(_: Vec<String>) -> Capability {
    NEIGHBOURHOOD_CREATE_CAPABILITY.clone()
}
fn neighbourhood_read(_: Vec<String>) -> Capability {
    NEIGHBOURHOOD_READ_CAPABILITY.clone()
}

/// The perspective a space method names.
const SPACE_PERSPECTIVE: IdParam = IdParam::Field(&["perspectiveUuid"]);

impl From<SpaceError> for WsRpcError {
    fn from(e: SpaceError) -> Self {
        let message = e.to_string();
        match e {
            SpaceError::ComingSoon(_) => WsRpcError::not_implemented(message),
            SpaceError::NeedsIdentity(_) => WsRpcError::forbidden(message),
            SpaceError::AlreadyCreated(_) => WsRpcError::conflict(message),
            SpaceError::NotFound(_) => WsRpcError::not_found(message),
            SpaceError::Invalid(_) => WsRpcError::bad_request(message),
            SpaceError::Unavailable(_) => WsRpcError::unavailable(message),
            SpaceError::Failed(_) => WsRpcError::internal(message),
        }
    }
}

async fn create(params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    let body: CreateNeighbourhoodParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    let created = spaces::create(body, &ctx.acts_as()).await?;
    Ok(serde_json::to_value(created)?)
}

async fn link_language_choice(
    params: Value,
    _ctx: Arc<RequestContext>,
) -> Result<Value, WsRpcError> {
    let body: LinkLanguageChoiceParams = serde_json::from_value(params)
        .map_err(|e| WsRpcError::bad_request(format!("Invalid params: {}", e)))?;
    Ok(serde_json::to_value(
        spaces::link_language_choice(&body.perspective_uuid).await?,
    )?)
}

async fn list(_params: Value, ctx: Arc<RequestContext>) -> Result<Value, WsRpcError> {
    // The perspectives the caller may read, as `perspective.all` narrows them for a caller
    // that is not the operator: the operator's holdings are the main agent's.
    let readable = pointers_covered(&ctx.session.capabilities(), PERSPECTIVE, READ);
    let mut perspectives = Vec::new();
    for perspective in crate::perspectives::all_perspectives() {
        let handle = perspective.persisted.lock().await.clone();
        if can_access_perspective_with_did(ctx.user_did(), &handle)
            && readable
                .as_ref()
                .is_none_or(|allowed| allowed.contains(&handle.uuid))
        {
            perspectives.push(perspective);
        }
    }
    let holder = Holder::of(&ctx.acts_as()).await;
    Ok(serde_json::to_value(
        spaces::list(perspectives, &holder).await,
    )?)
}

pub fn register_ws_handlers(map: &mut HandlerMap) {
    // Makes the perspective a space as the caller's person: `needs-identity` without one.
    map.method::<CreateNeighbourhoodParams, CreateNeighbourhoodResult>(
        "neighbourhood.create",
        create,
    )
    .long()
    .guard(Guard::Owns {
        cap: neighbourhood_create,
        id: SPACE_PERSPECTIVE,
        owner: Owner::Perspective,
    });
    // `null` for a perspective that is not a space.
    map.method::<LinkLanguageChoiceParams, Option<LinkLanguageChoice>>(
        "neighbourhood.linkLanguageChoice",
        link_language_choice,
    )
    .read()
    .guard(Guard::Owns {
        cap: neighbourhood_read,
        id: SPACE_PERSPECTIVE,
        owner: Owner::Perspective,
    });
    map.method::<NoParams, Vec<SpaceHolding>>("spaces.list", list)
        .read()
        .guard(Guard::CapAnyPointer(perspective_query_capability(vec![
            WILD_CARD.to_string(),
        ])));
}
