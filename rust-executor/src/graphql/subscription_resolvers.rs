#![allow(non_snake_case)]
use futures::stream::Stream;
use juniper::FieldResult;
use std::pin::Pin;

use crate::{
    js_core::JsCoreHandle,
    pubsub::{
        get_global_pubsub, subscribe_and_process, AGENT_STATUS_CHANGED_TOPIC, AGENT_UPDATED_TOPIC,
        EXCEPTION_OCCURRED_TOPIC, NEIGHBOURHOOD_SIGNAL_TOPIC, PERSPECTIVE_ADDED_TOPIC,
        PERSPECTIVE_LINK_ADDED_TOPIC, PERSPECTIVE_LINK_REMOVED_TOPIC,
        PERSPECTIVE_LINK_UPDATED_TOPIC, PERSPECTIVE_REMOVED_TOPIC,
        PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC, PERSPECTIVE_UPDATED_TOPIC,
        RUNTIME_MESSAGED_RECEIVED_TOPIC,
    },
};

use super::graphql_types::*;

pub struct Subscription;

#[juniper::graphql_subscription(context = JsCoreHandle)]
impl Subscription {
    async fn agent_status_changed(
        &self,
        context: &JsCoreHandle,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<AgentStatus>> + Send>> {
        println!("incoming sub");
        let pubsub = get_global_pubsub().await;
        let topic = &AGENT_STATUS_CHANGED_TOPIC;

        subscribe_and_process::<AgentStatus>(pubsub, topic.to_string()).await
    }

    async fn agent_updated(
        &self,
        context: &JsCoreHandle,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<Agent>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &AGENT_UPDATED_TOPIC;

        subscribe_and_process::<Agent>(pubsub, topic.to_string()).await
    }

    async fn exception_occurred(
        &self,
        context: &JsCoreHandle,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<ExceptionInfo>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &EXCEPTION_OCCURRED_TOPIC;

        subscribe_and_process::<ExceptionInfo>(pubsub, topic.to_string()).await
    }

    async fn neighbourhood_signal(
        &self,
        context: &JsCoreHandle,
        perspectiveUUID: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveExpression>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &NEIGHBOURHOOD_SIGNAL_TOPIC;

        subscribe_and_process::<PerspectiveExpression>(pubsub, topic.to_string()).await
    }

    async fn perspective_added(
        &self,
        context: &JsCoreHandle,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveHandle>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &PERSPECTIVE_ADDED_TOPIC;

        subscribe_and_process::<PerspectiveHandle>(pubsub, topic.to_string()).await
    }

    async fn perspective_link_added(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<LinkExpression>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &PERSPECTIVE_LINK_ADDED_TOPIC;

        subscribe_and_process::<LinkExpression>(pubsub, topic.to_string()).await
    }

    async fn perspective_link_removed(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<LinkExpression>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &PERSPECTIVE_LINK_REMOVED_TOPIC;

        subscribe_and_process::<LinkExpression>(pubsub, topic.to_string()).await
    }

    async fn perspective_link_updated(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<LinkExpressionUpdated>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &PERSPECTIVE_LINK_UPDATED_TOPIC;

        subscribe_and_process::<LinkExpressionUpdated>(pubsub, topic.to_string()).await
    }

    async fn perspective_removed(
        &self,
        context: &JsCoreHandle,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<String>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &PERSPECTIVE_REMOVED_TOPIC;

        subscribe_and_process::<String>(pubsub, topic.to_string()).await
    }

    async fn perspective_sync_state_change(
        &self,
        context: &JsCoreHandle,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<String>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &PERSPECTIVE_SYNC_STATE_CHANGE_TOPIC;

        subscribe_and_process::<String>(pubsub, topic.to_string()).await
    }

    async fn perspective_updated(
        &self,
        context: &JsCoreHandle,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveHandle>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &PERSPECTIVE_UPDATED_TOPIC;

        subscribe_and_process::<PerspectiveHandle>(pubsub, topic.to_string()).await
    }

    async fn runtime_message_received(
        &self,
        context: &JsCoreHandle,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveExpression>> + Send>> {
        let pubsub = get_global_pubsub().await;
        let topic = &RUNTIME_MESSAGED_RECEIVED_TOPIC;

        subscribe_and_process::<PerspectiveExpression>(pubsub, topic.to_string()).await
    }
}
