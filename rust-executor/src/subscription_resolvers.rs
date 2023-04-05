#![allow(non_snake_case)]
use futures::stream::Stream;
use juniper::{graphql_value, FieldError, FieldResult};
use std::pin::Pin;

use crate::graphql_types::*;

pub struct Subscription;

#[juniper::graphql_subscription()]
impl Subscription {
    async fn agent_status_changed(
        &self,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<AgentStatus>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn agent_updated(&self) -> Pin<Box<dyn Stream<Item = FieldResult<Agent>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn exception_occurred(
        &self,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<ExceptionInfo>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn neighbourhood_signal(
        &self,
        perspectiveUUID: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveExpression>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn perspective_added(
        &self,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveHandle>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn perspective_link_added(
        &self,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<LinkExpression>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn perspective_link_removed(
        &self,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<LinkExpression>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn perspective_link_updated(
        &self,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<LinkExpressionUpdated>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn perspective_removed(&self) -> Pin<Box<dyn Stream<Item = FieldResult<String>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn perspective_sync_state_change(
        &self,
        uuid: String,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<String>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn perspective_updated(
        &self,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveHandle>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }

    async fn runtime_message_received(
        &self,
    ) -> Pin<Box<dyn Stream<Item = FieldResult<PerspectiveExpression>> + Send>> {
        let err = FieldError::new("Unimplemented", graphql_value!({ "type": "UNIMPLEMENTED" }));
        let err_stream = futures::stream::once(async { Err(err) });
        Box::pin(err_stream)
    }
}
