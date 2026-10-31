pub mod core;
pub mod domain;

pub use core::*;

// Re-export all domain types except those that conflict with core
// (Perspective, Neighbourhood, LanguageRef, UserInfo)
// or with rest::types (PerspectiveInput, NotificationInput, EntanglementProofInput).
// For those, use crate::types::domain::X or crate::api::types::X explicitly.
pub use domain::{
    AIPromptExamplesInput, AITaskInput, Agent, AgentSignature, AgentStatus, Apps, AuthInfoInput,
    CapabilityInput, DateTime, DecoratedNeighbourhoodExpression, DecoratedPerspectiveDiff,
    EndpointInput, EntanglementProof, ExceptionInfo, ExceptionType, ExpressionProofInput,
    ExpressionRendered, HostingUserInfo, Icon, ImportResult, ImportStats, InteractionCall,
    InteractionMeta, InteractionParameter, JsResultType, Language, LanguageHandle,
    LanguageLanguageInput, LanguageMeta, LanguageMetaInput, LinkExpressionInput,
    LinkExpressionUpdated, LinkInput, LinkMutations, LinkQuery, LinkStatus, LinkUpdated,
    ModelInput, NeighbourhoodSignalFilter, NotificationInput, OnlineAgent, PaymentRequestResult,
    PerspectiveEvent, PerspectiveExpression, PerspectiveHandle, PerspectiveInput,
    PerspectiveLinkEvent, PerspectiveLinkUpdatedEvent, PerspectiveRemovedEvent, PerspectiveState,
    PerspectiveStateFilter, PerspectiveUnsignedInput, PromptOutput, QuerySubscription,
    QuerySubscriptionUpdate, QuerySubscriptionUpdateEvent, QueryUpdatesLagged, ReadinessStatus,
    RequestContext, Resource, ResourceInput, RuntimeInfo, SentMessage, TranscriptionTextFilter,
    UserCreationResult, UserStatistics, VerificationRequestResult,
};
