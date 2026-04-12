// Auto-generated REST API types from Rust via ts-rs
// These types are the source of truth for REST request/response bodies.
// Do NOT edit manually — regenerate with: pnpm run generate:rest-types
//
// Note: Domain types (Link, LinkInput, LinkExpression, etc.) are NOT re-exported
// here to avoid conflicts with the hand-written domain classes in the SDK.
// Only REST-specific request/response types are exported.

export type { AddAgentInfosRequest } from "./AddAgentInfosRequest";
export type { AddLinkExpressionRequest } from "./AddLinkExpressionRequest";
export type { AddLinkRequest } from "./AddLinkRequest";
export type { AddLinksBulkRequest } from "./AddLinksBulkRequest";
export type { AddSdnaRequest } from "./AddSdnaRequest";
export type { ApplyTemplateRequest } from "./ApplyTemplateRequest";
export type { BroadcastRequest } from "./BroadcastRequest";
export type { CommitBatchRequest } from "./CommitBatchRequest";
export type { CreateExpressionRequest } from "./CreateExpressionRequest";
export type { CreatePerspectiveRequest } from "./CreatePerspectiveRequest";
export type { CreateSubjectRequest } from "./CreateSubjectRequest";
export type { CreateUserRequest } from "./CreateUserRequest";
export type { DecoratedExpressionProof } from "./DecoratedExpressionProof";
export type { DecoratedLinkExpression } from "./DecoratedLinkExpression";
export type { DisposeQueryRequest } from "./DisposeQueryRequest";
export type { EmailTestRequest } from "./EmailTestRequest";
export type { EmbedRequest } from "./EmbedRequest";
export type { EntanglementProofPreflightRequest } from "./EntanglementProofPreflightRequest";
export type { ExecuteCommandsRequest } from "./ExecuteCommandsRequest";
export type { ExportRequest } from "./ExportRequest";
export type { ExpressionManyRequest } from "./ExpressionManyRequest";
export type { FriendSendMessageRequest } from "./FriendSendMessageRequest";
export type { FriendsListRequest } from "./FriendsListRequest";
export type { GenerateAgentRequest } from "./GenerateAgentRequest";
export type { GenerateJwtRequest } from "./GenerateJwtRequest";
export type { GetSubjectDataRequest } from "./GetSubjectDataRequest";
export type { HostingInfoResponse } from "./HostingInfoResponse";
export type { HostingWalletResponse } from "./HostingWalletResponse";
export type { ImportAgentRequest } from "./ImportAgentRequest";
export type { ImportRequest } from "./ImportRequest";
export type { JoinNeighbourhoodRequest } from "./JoinNeighbourhoodRequest";
export type { KeepAliveQueryRequest } from "./KeepAliveQueryRequest";
export type { LinkLanguageTemplatesRequest } from "./LinkLanguageTemplatesRequest";
export type { LinkMutationRequest } from "./LinkMutationRequest";
export type { LinkMutationResponse } from "./LinkMutationResponse";
export type { LinkMutationsRequest } from "./LinkMutationsRequest";
export type { LinkUpdateInput } from "./LinkUpdateInput";
export type { LockAgentRequest } from "./LockAgentRequest";
export type { LoginUserRequest } from "./LoginUserRequest";
export type { OpenLinkRequest } from "./OpenLinkRequest";
export type { PermitCapabilityRequest } from "./PermitCapabilityRequest";
export type { PromptRequest } from "./PromptRequest";
export type { PublicPerspectiveInput } from "./PublicPerspectiveInput";
export type { PublishLanguageRequest } from "./PublishLanguageRequest";
export type { PublishNeighbourhoodRequest } from "./PublishNeighbourhoodRequest";
export type { QueryRequest } from "./QueryRequest";
export type { RemoveLinkRequest } from "./RemoveLinkRequest";
export type { RemoveLinksBulkRequest } from "./RemoveLinksBulkRequest";
export type { RequestCapabilityRequest } from "./RequestCapabilityRequest";
export type { RequestPaymentRequest } from "./RequestPaymentRequest";
export type { RequestVerificationRequest } from "./RequestVerificationRequest";
export type { SetDefaultModelRequest } from "./SetDefaultModelRequest";
export type { SetFreeHostingEnabledRequest } from "./SetFreeHostingEnabledRequest";
export type { SetHotWalletAddressRequest } from "./SetHotWalletAddressRequest";
export type { SetMultiUserRequest } from "./SetMultiUserRequest";
export type { SetOnlineStatusRequest } from "./SetOnlineStatusRequest";
export type { SetStatusRequest } from "./SetStatusRequest";
export type { SignMessageRequest } from "./SignMessageRequest";
export type { SignalRequest } from "./SignalRequest";
export type { SubscribeQueryRequest } from "./SubscribeQueryRequest";
export type { SubscribeQueryResponse } from "./SubscribeQueryResponse";
export type { TrustedAgentsRequest } from "./TrustedAgentsRequest";
export type { UnlockAgentRequest } from "./UnlockAgentRequest";
export type { UpdateLinkRequest } from "./UpdateLinkRequest";
export type { UpdatePerspectiveRequest } from "./UpdatePerspectiveRequest";
export type { UpdateProfileRequest } from "./UpdateProfileRequest";
export type { VerifyEmailRequest } from "./VerifyEmailRequest";
export type { VerifySignatureRequest } from "./VerifySignatureRequest";
export type { NotificationGrantRequest } from "./NotificationGrantRequest";
export type { WriteSettingsRequest } from "./WriteSettingsRequest";
export type { RouteMap, RouteMethod, RoutePath, GetRoutes, PostRoutes, PutRoutes, PatchRoutes, DeleteRoutes, OpenTranscriptionRequest, FeedTranscriptionRequest, CloseTranscriptionRequest } from "./routes";
