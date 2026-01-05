#![allow(non_snake_case)]
use coasys_juniper::{graphql_value, FieldResult};
use futures::stream;
use futures::stream::Stream;
use std::pin::Pin;

use crate::{
    pubsub::{
        get_global_pubsub, subscribe_and_process, AGENT_STATUS_CHANGED_TOPIC, AGENT_UPDATED_TOPIC,
        AI_MODEL_LOADING_STATUS, AI_TRANSCRIPTION_TEXT_TOPIC, APPS_CHANGED,
        EXCEPTION_OCCURRED_TOPIC, NEIGHBOURHOOD_SIGNAL_TOPIC, PERSPECTIVE_ADDED_TOPIC,
        PERSPECTIVE_LINK_ADDED_TOPIC, PERSPECTIVE_LINK_REMOVED_TOPIC,
        PERSPECTIVE_LINK_UPDATED_TOPIC, PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC,
        PERSPECTIVE_REMOVED_TOPIC, PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC, PERSPECTIVE_UPDATED_TOPIC,
        RUNTIME_MESSAGED_RECEIVED_TOPIC, RUNTIME_NOTIFICATION_TRIGGERED_TOPIC,
    },
    types::{DecoratedLinkExpression, TriggeredNotification},
};

use super::graphql_types::*;
use crate::agent::capabilities::*;

pub struct Subscription;

/// Helper function to get agent DID filter with proper security enforcement.
/// For main agent, returns the DID if available (or None if not yet initialized).
/// For user agents, requires DID resolution to succeed - returns an error if it fails.
/// This prevents data leakage by ensuring user agents always have valid DIDs.
fn get_agent_did_filter(
    auth_token: String,
    subscription_name: &str,
) -> Result<Option<String>, coasys_juniper::FieldError> {
    use crate::agent::{did_for_context, AgentContext};
    let agent_context = AgentContext::from_auth_token(auth_token);

    if agent_context.is_main_agent {
        // Main agent can use None filter (sees all) or DID if available
        let agent_did = did_for_context(&agent_context).ok();
        if let Some(ref did) = agent_did {
            log::debug!(
                "{} subscription: Main agent filtering by DID={}",
                subscription_name,
                did
            );
        }
        Ok(agent_did)
    } else {
        // User agent - MUST have valid DID, otherwise abort to prevent data leakage
        match did_for_context(&agent_context) {
            Ok(did) => {
                log::debug!(
                    "{} subscription: User agent filtering by DID={}",
                    subscription_name,
                    did
                );
                Ok(Some(did))
            }
            Err(e) => {
                log::error!(
                    "{} subscription: Failed to get DID for user context: {}",
                    subscription_name,
                    e
                );
                Err(coasys_juniper::FieldError::new(
                    format!("Failed to resolve agent DID: {}", e),
                    graphql_value!(null),
                ))
            }
        }
    }
}

/// Helper function to get composite filter for neighbourhood signals.
/// Creates a filter in the format "perspective_uuid|agent_did" with proper security enforcement.
fn get_neighbourhood_signal_filter(
    auth_token: String,
    perspective_uuid: String,
) -> Result<Option<String>, coasys_juniper::FieldError> {
    use crate::agent::{did_for_context, AgentContext};
    let agent_context = AgentContext::from_auth_token(auth_token);

    if agent_context.is_main_agent {
        // Main agent can use perspective-only filter (can see all signals for perspective)
        let agent_did = did_for_context(&agent_context).ok();
        if let Some(ref did) = agent_did {
            Ok(Some(format!("{}|{}", perspective_uuid, did)))
        } else {
            // Main agent without DID - use perspective-only filter
            Ok(Some(perspective_uuid))
        }
    } else {
        // User agent - MUST have valid DID, otherwise abort to prevent signal leakage
        match did_for_context(&agent_context) {
            Ok(did) => Ok(Some(format!("{}|{}", perspective_uuid, did))),
            Err(e) => {
                log::error!(
                    "neighbourhood_signal subscription: Failed to get DID for user context: {}",
                    e
                );
                Err(coasys_juniper::FieldError::new(
                    format!("Failed to resolve agent DID: {}", e),
                    graphql_value!(null),
                ))
            }
        }
    }
}

#[coasys_juniper::graphql_subscription(context = RequestContext)]
impl Subscription {
    async fn agent_status_changed(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<AgentStatus>> + Send>> {
        match check_capability(&context.capabilities, &AGENT_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let pubsub = get_global_pubsub().await;
                let topic = &AGENT_STATUS_CHANGED_TOPIC;
                subscribe_and_process::<AgentStatus>(pubsub, topic.to_string(), None).await
            }
        }
    }

    async fn agent_apps_changed(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<Option<Apps>>> + Send>> {
        match check_capability(&context.capabilities, &AGENT_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let pubsub = get_global_pubsub().await;
                let topic = &APPS_CHANGED;
                subscribe_and_process::<Option<Apps>>(pubsub, topic.to_string(), None).await
            }
        }
    }

    async fn agent_updated(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<Agent>> + Send>> {
        match check_capability(&context.capabilities, &AGENT_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let pubsub = get_global_pubsub().await;
                let topic = &AGENT_UPDATED_TOPIC;
                subscribe_and_process::<Agent>(pubsub, topic.to_string(), None).await
            }
        }
    }

    async fn exception_occurred(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<ExceptionInfo>> + Send>> {
        match check_capability(
            &context.capabilities,
            &RUNTIME_EXCEPTION_SUBSCRIBE_CAPABILITY,
        ) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let pubsub = get_global_pubsub().await;
                let topic = &EXCEPTION_OCCURRED_TOPIC;
                subscribe_and_process::<ExceptionInfo>(pubsub, topic.to_string(), None).await
            }
        }
    }

    async fn neighbourhood_signal(
        &self,
        context: &RequestContext,
        perspectiveUUID: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveExpression>> + Send>> {
        match check_capability(&context.capabilities, &NEIGHBOURHOOD_READ_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let filter = match get_neighbourhood_signal_filter(
                    context.auth_token.clone(),
                    perspectiveUUID.clone(),
                ) {
                    Ok(filter) => filter,
                    Err(e) => return Box::pin(stream::once(async move { Err(e) })),
                };

                let pubsub = get_global_pubsub().await;
                let topic = &NEIGHBOURHOOD_SIGNAL_TOPIC;
                log::debug!(
                    "neighbourhood_signal subscription: perspective={}, filter={:?}",
                    perspectiveUUID,
                    filter
                );

                subscribe_and_process::<NeighbourhoodSignalFilter>(
                    pubsub,
                    topic.to_string(),
                    filter,
                )
                .await
            }
        }
    }

    async fn perspective_added(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveHandle>> + Send>> {
        match check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let filter = match get_agent_did_filter(
                    context.auth_token.clone(),
                    "PERSPECTIVE_ADDED",
                ) {
                    Ok(filter) => filter,
                    Err(e) => return Box::pin(stream::once(async move { Err(e) })),
                };

                let pubsub = get_global_pubsub().await;
                let topic = &PERSPECTIVE_ADDED_TOPIC;
                log::info!(
                    "📬 PERSPECTIVE_ADDED subscription: filter={:?}",
                    filter
                );

                subscribe_and_process::<PerspectiveWithOwner>(pubsub, topic.to_string(), filter)
                    .await
            }
        }
    }

    async fn perspective_link_added(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<DecoratedLinkExpression>> + Send>> {
        match check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                // Check if user has access to this perspective before subscribing
                let user_email = user_email_from_token(context.auth_token.clone());

                // Get the perspective and verify access
                if let Some(perspective) = crate::perspectives::get_perspective(&uuid) {
                    let handle = perspective.persisted.lock().await.clone();
                    if !crate::graphql::query_resolvers::can_access_perspective(
                        &user_email,
                        &handle,
                    ) {
                        // User doesn't have access to this perspective
                        return Box::pin(stream::once(async move {
                            Err(coasys_juniper::FieldError::new(
                                "Access denied: You don't have permission to subscribe to this perspective",
                                graphql_value!(null),
                            ))
                        }));
                    }
                } else {
                    // Perspective doesn't exist
                    return Box::pin(stream::once(async move {
                        Err(coasys_juniper::FieldError::new(
                            "Perspective not found",
                            graphql_value!(null),
                        ))
                    }));
                }

                let filter = match get_agent_did_filter(
                    context.auth_token.clone(),
                    "PERSPECTIVE_LINK_ADDED",
                ) {
                    Ok(filter) => filter,
                    Err(e) => return Box::pin(stream::once(async move { Err(e) })),
                };

                let pubsub = get_global_pubsub().await;
                let topic = &PERSPECTIVE_LINK_ADDED_TOPIC;

                subscribe_and_process::<PerspectiveLinkWithOwner>(pubsub, topic.to_string(), filter)
                    .await
            }
        }
    }

    async fn perspective_link_removed(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<DecoratedLinkExpression>> + Send>> {
        match check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                // Check if user has access to this perspective before subscribing
                let user_email = user_email_from_token(context.auth_token.clone());

                // Get the perspective and verify access
                if let Some(perspective) = crate::perspectives::get_perspective(&uuid) {
                    let handle = perspective.persisted.lock().await.clone();
                    if !crate::graphql::query_resolvers::can_access_perspective(
                        &user_email,
                        &handle,
                    ) {
                        return Box::pin(stream::once(async move {
                            Err(coasys_juniper::FieldError::new(
                                "Access denied: You don't have permission to subscribe to this perspective",
                                graphql_value!(null),
                            ))
                        }));
                    }
                } else {
                    return Box::pin(stream::once(async move {
                        Err(coasys_juniper::FieldError::new(
                            "Perspective not found",
                            graphql_value!(null),
                        ))
                    }));
                }

                let filter = match get_agent_did_filter(
                    context.auth_token.clone(),
                    "PERSPECTIVE_LINK_REMOVED",
                ) {
                    Ok(filter) => filter,
                    Err(e) => return Box::pin(stream::once(async move { Err(e) })),
                };

                let pubsub = get_global_pubsub().await;
                let topic = &PERSPECTIVE_LINK_REMOVED_TOPIC;

                subscribe_and_process::<PerspectiveLinkWithOwner>(pubsub, topic.to_string(), filter)
                    .await
            }
        }
    }

    async fn perspective_link_updated(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<LinkUpdated>> + Send>> {
        match check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                // Check if user has access to this perspective before subscribing
                let user_email = user_email_from_token(context.auth_token.clone());

                // Get the perspective and verify access
                if let Some(perspective) = crate::perspectives::get_perspective(&uuid) {
                    let handle = perspective.persisted.lock().await.clone();
                    if !crate::graphql::query_resolvers::can_access_perspective(
                        &user_email,
                        &handle,
                    ) {
                        return Box::pin(stream::once(async move {
                            Err(coasys_juniper::FieldError::new(
                                "Access denied: You don't have permission to subscribe to this perspective",
                                graphql_value!(null),
                            ))
                        }));
                    }
                } else {
                    return Box::pin(stream::once(async move {
                        Err(coasys_juniper::FieldError::new(
                            "Perspective not found",
                            graphql_value!(null),
                        ))
                    }));
                }

                let filter = match get_agent_did_filter(
                    context.auth_token.clone(),
                    "PERSPECTIVE_LINK_UPDATED",
                ) {
                    Ok(filter) => filter,
                    Err(e) => return Box::pin(stream::once(async move { Err(e) })),
                };

                let pubsub = get_global_pubsub().await;
                let topic = &PERSPECTIVE_LINK_UPDATED_TOPIC;

                subscribe_and_process::<PerspectiveLinkUpdatedWithOwner>(
                    pubsub,
                    topic.to_string(),
                    filter,
                )
                .await
            }
        }
    }

    async fn perspective_removed(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<String>> + Send>> {
        match check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let filter = match get_agent_did_filter(
                    context.auth_token.clone(),
                    "PERSPECTIVE_REMOVED",
                ) {
                    Ok(filter) => filter,
                    Err(e) => return Box::pin(stream::once(async move { Err(e) })),
                };

                let pubsub = get_global_pubsub().await;
                let topic = &PERSPECTIVE_REMOVED_TOPIC;
                log::info!(
                    "📬 PERSPECTIVE_REMOVED subscription: filter={:?}",
                    filter
                );

                subscribe_and_process::<PerspectiveRemovedWithOwner>(
                    pubsub,
                    topic.to_string(),
                    filter,
                )
                .await
            }
        }
    }

    async fn perspective_sync_state_change(
        &self,
        context: &RequestContext,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<String>> + Send>> {
        match check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                // Check if user has access to this perspective before subscribing
                let user_email = user_email_from_token(context.auth_token.clone());

                // Get the perspective and verify access
                if let Some(perspective) = crate::perspectives::get_perspective(&uuid) {
                    let handle = perspective.persisted.lock().await.clone();
                    if !crate::graphql::query_resolvers::can_access_perspective(
                        &user_email,
                        &handle,
                    ) {
                        return Box::pin(stream::once(async move {
                            Err(coasys_juniper::FieldError::new(
                                "Access denied: You don't have permission to subscribe to this perspective",
                                graphql_value!(null),
                            ))
                        }));
                    }
                } else {
                    return Box::pin(stream::once(async move {
                        Err(coasys_juniper::FieldError::new(
                            "Perspective not found",
                            graphql_value!(null),
                        ))
                    }));
                }

                let pubsub = get_global_pubsub().await;
                let topic = &PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC;
                subscribe_and_process::<PerspectiveStateFilter>(
                    pubsub,
                    topic.to_string(),
                    Some(uuid),
                )
                .await
            }
        }
    }

    async fn perspective_updated(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveHandle>> + Send>> {
        match check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let filter = match get_agent_did_filter(
                    context.auth_token.clone(),
                    "PERSPECTIVE_UPDATED",
                ) {
                    Ok(filter) => filter,
                    Err(e) => return Box::pin(stream::once(async move { Err(e) })),
                };

                let pubsub = get_global_pubsub().await;
                let topic = &PERSPECTIVE_UPDATED_TOPIC;
                log::info!(
                    "📬 PERSPECTIVE_UPDATED subscription: filter={:?}",
                    filter
                );

                subscribe_and_process::<PerspectiveWithOwner>(pubsub, topic.to_string(), filter)
                    .await
            }
        }
    }

    async fn runtime_message_received(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveExpression>> + Send>> {
        match check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let pubsub = get_global_pubsub().await;
                let topic = &RUNTIME_MESSAGED_RECEIVED_TOPIC;
                subscribe_and_process::<PerspectiveExpression>(pubsub, topic.to_string(), None)
                    .await
            }
        }
    }

    async fn runtime_notification_triggered(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<TriggeredNotification>> + Send>> {
        match check_capability(&context.capabilities, &AGENT_READ_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let pubsub = get_global_pubsub().await;
                let topic = &RUNTIME_NOTIFICATION_TRIGGERED_TOPIC;
                subscribe_and_process::<TriggeredNotification>(pubsub, topic.to_string(), None)
                    .await
            }
        }
    }

    async fn ai_transcription_text(
        &self,
        context: &RequestContext,
        stream_id: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<String>> + Send>> {
        match check_capability(&context.capabilities, &AI_TRANSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let pubsub = get_global_pubsub().await;
                let topic = &AI_TRANSCRIPTION_TEXT_TOPIC;
                subscribe_and_process::<TranscriptionTextFilter>(
                    pubsub,
                    topic.to_string(),
                    Some(stream_id),
                )
                .await
            }
        }
    }

    async fn ai_model_loading_status(
        &self,
        context: &RequestContext,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<AIModelLoadingStatus>> + Send>> {
        match check_capability(&context.capabilities, &AI_ALL_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let pubsub = get_global_pubsub().await;
                let topic = &AI_MODEL_LOADING_STATUS;
                subscribe_and_process::<AIModelLoadingStatus>(pubsub, topic.to_string(), None).await
            }
        }
    }

    async fn perspective_query_subscription(
        &self,
        context: &RequestContext,
        subscription_id: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<String>> + Send>> {
        match check_capability(&context.capabilities, &PERSPECTIVE_SUBSCRIBE_CAPABILITY) {
            Err(e) => Box::pin(stream::once(async move { Err(e.into()) })),
            Ok(_) => {
                let pubsub = get_global_pubsub().await;
                let topic = &PERSPECTIVE_QUERY_SUBSCRIPTION_TOPIC;
                subscribe_and_process::<PerspectiveQuerySubscriptionFilter>(
                    pubsub,
                    topic.to_string(),
                    Some(subscription_id),
                )
                .await
            }
        }
    }
}
