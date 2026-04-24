// AUTO-GENERATED — do not edit manually
// Regenerate: cd rust-executor && cargo test generate_route_map

import type {
  AIModelLoadingStatus,
  AddAgentInfosRequest,
  AddLinkExpressionRequest,
  AddLinkRequest,
  AddLinksBulkRequest,
  AddSdnaRequest,
  ApplyTemplateRequest,
  BroadcastRequest,
  CommitBatchRequest,
  ComputeLogEntry,
  CreateExpressionRequest,
  CreatePerspectiveRequest,
  CreateSubjectRequest,
  CreateUserRequest,
  CreateUserResponse,
  DecoratedLinkExpression,
  DisposeQueryRequest,
  EmailTestRequest,
  EmbedRequest,
  EntanglementProofPreflightRequest,
  ExecuteCommandsRequest,
  ExportRequest,
  ExpressionManyRequest,
  ExpressionRendered,
  FriendSendMessageRequest,
  FriendsListRequest,
  GenerateAgentRequest,
  GenerateJwtRequest,
  GetSubjectDataRequest,
  HostingInfoResponse,
  HostingWalletResponse,
  ImportAgentRequest,
  ImportRequest,
  JoinNeighbourhoodRequest,
  KeepAliveQueryRequest,
  LinkLanguageTemplatesRequest,
  LinkMutationResponse,
  LinkMutationsRequest,
  LockAgentRequest,
  LoginUserRequest,
  NotificationGrantRequest,
  OpenLinkRequest,
  PermitCapabilityRequest,
  PromptRequest,
  PublishLanguageRequest,
  PublishNeighbourhoodRequest,
  QueryRequest,
  RemoveLinkRequest,
  RemoveLinksBulkRequest,
  RequestCapabilityRequest,
  RequestPaymentRequest,
  RequestVerificationRequest,
  SetDefaultModelRequest,
  SetHotWalletAddressRequest,
  SetMultiUserRequest,
  SetOnlineStatusRequest,
  SetStatusRequest,
  SetUserFreeAccessRequest,
  SignMessageRequest,
  SignalRequest,
  SubscribeQueryRequest,
  SubscribeQueryResponse,
  UnlockAgentRequest,
  UpdateLinkRequest,
  UpdatePerspectiveRequest,
  UpdateProfileRequest,
  UserStatistics,
  VerificationRequestResult,
  VerifyEmailRequest,
  VerifySignatureRequest,
} from './index';

import type {
  Agent,
  AgentSignature,
  AgentStatus,
  Apps,
  EntanglementProof,
  EntanglementProofInput,
  InteractionCall,
  InteractionMeta,
  LanguageHandle,
  LanguageMeta,
  LanguageRef,
  Notification,
  NotificationInput,
  OnlineAgent,
  Perspective,
  PerspectiveHandle,
  RuntimeInfo,
} from '../../index';

import type { AITask } from '../../ai/Tasks';
import type { Model } from '../../ai/AITypes';

// Transcription request types (not yet in ts-rs exports)
export interface OpenTranscriptionRequest {
  model_id: string;
  params?: {
    threshold?: number;
    min_speech_duration_ms?: number;
    min_silence_duration_ms?: number;
    speech_pad_ms?: number;
    max_speech_duration_s?: number;
  };
}

export interface FeedTranscriptionRequest {
  stream_id: string;
  audio_base64: string;
}

export interface CloseTranscriptionRequest {
  stream_id: string;
}

export interface RouteMap {
  'GET /agent': { request: never; response: Agent };
  'GET /agent/apps': { request: never; response: Apps[] };
  'DELETE /agent/apps/:id': { request: never; response: Apps[] };
  'POST /agent/auth/jwt': { request: GenerateJwtRequest; response: string };
  'POST /agent/auth/permit': { request: PermitCapabilityRequest; response: string };
  'POST /agent/auth/request': { request: RequestCapabilityRequest; response: string };
  'DELETE /agent/auth/token/:token': { request: never; response: Apps[] };
  'GET /agent/by-did/:did': { request: never; response: Agent | null };
  'POST /agent/entanglement-proof-preflight': { request: EntanglementProofPreflightRequest; response: EntanglementProof };
  'DELETE /agent/entanglement-proofs': { request: EntanglementProofInput[]; response: EntanglementProof[] };
  'GET /agent/entanglement-proofs': { request: never; response: EntanglementProof[] };
  'POST /agent/entanglement-proofs': { request: EntanglementProofInput[]; response: EntanglementProof[] };
  'POST /agent/generate': { request: GenerateAgentRequest; response: AgentStatus };
  'POST /agent/import': { request: ImportAgentRequest; response: AgentStatus };
  'GET /agent/is-locked': { request: never; response: boolean };
  'POST /agent/lock': { request: LockAgentRequest; response: AgentStatus };
  'PATCH /agent/profile': { request: UpdateProfileRequest; response: Agent };
  'POST /agent/sign': { request: SignMessageRequest; response: AgentSignature };
  'GET /agent/status': { request: never; response: AgentStatus };
  'DELETE /agent/trusted': { request: string[]; response: string[] };
  'GET /agent/trusted': { request: never; response: string[] };
  'PUT /agent/trusted': { request: string[]; response: string[] };
  'POST /agent/unlock': { request: UnlockAgentRequest; response: AgentStatus };
  'POST /ai/embed': { request: EmbedRequest; response: string };
  'GET /ai/model-loading-status': { request: never; response: AIModelLoadingStatus };
  'GET /ai/models': { request: never; response: Model[] };
  'POST /ai/models': { request: Record<string, unknown>; response: string };
  'DELETE /ai/models/:id': { request: never; response: boolean };
  'PUT /ai/models/:id': { request: Record<string, unknown>; response: boolean };
  'PUT /ai/models/:id/default': { request: SetDefaultModelRequest; response: boolean };
  'GET /ai/models/default': { request: never; response: Model | null };
  'POST /ai/prompt': { request: PromptRequest; response: string };
  'GET /ai/tasks': { request: never; response: AITask[] };
  'POST /ai/tasks': { request: Record<string, unknown>; response: AITask };
  'DELETE /ai/tasks/:id': { request: never; response: boolean };
  'PUT /ai/tasks/:id': { request: Record<string, unknown>; response: AITask };
  'POST /ai/transcription/close': { request: CloseTranscriptionRequest; response: string };
  'POST /ai/transcription/feed': { request: FeedTranscriptionRequest; response: string };
  'POST /ai/transcription/open': { request: OpenTranscriptionRequest; response: string };
  'POST /dev/email-test': { request: EmailTestRequest; response: unknown };
  'GET /events/agent': { request: never; response: void };
  'GET /events/ai': { request: never; response: void };
  'GET /events/neighbourhoods/:uuid/signals': { request: never; response: void };
  'GET /events/perspectives': { request: never; response: void };
  'GET /events/perspectives/:uuid/links': { request: never; response: void };
  'GET /events/query-subscription/:subscription_id': { request: never; response: void };
  'GET /events/runtime': { request: never; response: void };
  'GET /events/unified': { request: never; response: void };
  'POST /expressions': { request: CreateExpressionRequest; response: string };
  'GET /expressions/:url': { request: never; response: ExpressionRendered | string | null };
  'POST /expressions/:url/interact': { request: InteractionCall; response: string };
  'GET /expressions/:url/interactions': { request: never; response: InteractionMeta[] };
  'POST /expressions/many': { request: ExpressionManyRequest; response: Array<ExpressionRendered | null> };
  'GET /hosting': { request: never; response: HostingInfoResponse };
  'POST /hosting/request-payment': { request: RequestPaymentRequest; response: unknown };
  'GET /hosting/wallet': { request: never; response: HostingWalletResponse };
  'GET /hosting/wallet/history': { request: never; response: unknown };
  'PUT /hosting/wallet/hot-wallet-address': { request: SetHotWalletAddressRequest; response: boolean };
  'GET /languages': { request: never; response: LanguageHandle[] };
  'DELETE /languages/:address': { request: never; response: boolean };
  'GET /languages/:address': { request: never; response: LanguageHandle };
  'GET /languages/:address/meta': { request: never; response: LanguageMeta };
  'PUT /languages/:address/settings': { request: Record<string, unknown>; response: boolean };
  'GET /languages/:address/source': { request: never; response: string };
  'POST /languages/apply-template': { request: ApplyTemplateRequest; response: LanguageRef };
  'POST /languages/publish': { request: PublishLanguageRequest; response: LanguageMeta };
  'POST /neighbourhoods/:uuid/broadcast': { request: BroadcastRequest; response: boolean };
  'GET /neighbourhoods/:uuid/has-telepresence': { request: never; response: boolean };
  'GET /neighbourhoods/:uuid/online-agents': { request: never; response: OnlineAgent[] };
  'PUT /neighbourhoods/:uuid/online-status': { request: SetOnlineStatusRequest; response: boolean };
  'GET /neighbourhoods/:uuid/other-agents': { request: never; response: string[] };
  'POST /neighbourhoods/:uuid/signal': { request: SignalRequest; response: boolean };
  'POST /neighbourhoods/join': { request: JoinNeighbourhoodRequest; response: PerspectiveHandle };
  'POST /neighbourhoods/publish': { request: PublishNeighbourhoodRequest; response: string };
  'GET /perspectives': { request: never; response: PerspectiveHandle[] };
  'POST /perspectives': { request: CreatePerspectiveRequest; response: PerspectiveHandle };
  'DELETE /perspectives/:uuid': { request: never; response: boolean };
  'GET /perspectives/:uuid': { request: never; response: PerspectiveHandle };
  'PUT /perspectives/:uuid': { request: UpdatePerspectiveRequest; response: PerspectiveHandle };
  'POST /perspectives/:uuid/batch': { request: never; response: string };
  'POST /perspectives/:uuid/batch/commit': { request: CommitBatchRequest; response: LinkMutationResponse };
  'POST /perspectives/:uuid/commands': { request: ExecuteCommandsRequest; response: void };
  'POST /perspectives/:uuid/create-subject': { request: CreateSubjectRequest; response: boolean };
  'POST /perspectives/:uuid/dispose-query-subscription': { request: DisposeQueryRequest; response: boolean };
  'POST /perspectives/:uuid/dispose-surreal-query-subscription': { request: DisposeQueryRequest; response: boolean };
  'POST /perspectives/:uuid/get-subject-data': { request: GetSubjectDataRequest; response: string };
  'POST /perspectives/:uuid/keep-alive-query': { request: KeepAliveQueryRequest; response: boolean };
  'POST /perspectives/:uuid/keep-alive-surreal-query': { request: KeepAliveQueryRequest; response: boolean };
  'DELETE /perspectives/:uuid/links': { request: RemoveLinkRequest; response: boolean };
  'GET /perspectives/:uuid/links': { request: never; response: DecoratedLinkExpression[] };
  'POST /perspectives/:uuid/links': { request: AddLinkRequest; response: DecoratedLinkExpression };
  'PUT /perspectives/:uuid/links': { request: UpdateLinkRequest; response: DecoratedLinkExpression };
  'POST /perspectives/:uuid/links/bulk': { request: AddLinksBulkRequest; response: DecoratedLinkExpression[] };
  'POST /perspectives/:uuid/links/expression': { request: AddLinkExpressionRequest; response: DecoratedLinkExpression };
  'POST /perspectives/:uuid/links/mutations': { request: LinkMutationsRequest; response: LinkMutationResponse };
  'POST /perspectives/:uuid/links/remove-bulk': { request: RemoveLinksBulkRequest; response: DecoratedLinkExpression[] };
  'POST /perspectives/:uuid/publish-snapshot': { request: never; response: string };
  'POST /perspectives/:uuid/query': { request: QueryRequest; response: string };
  'POST /perspectives/:uuid/sdna': { request: AddSdnaRequest; response: boolean };
  'GET /perspectives/:uuid/snapshot': { request: never; response: Perspective };
  'POST /perspectives/:uuid/subscribe-query': { request: SubscribeQueryRequest; response: SubscribeQueryResponse };
  'POST /perspectives/:uuid/subscribe-surreal-query': { request: SubscribeQueryRequest; response: SubscribeQueryResponse };
  'GET /runtime/compute-log': { request: never; response: ComputeLogEntry[] };
  'POST /runtime/export': { request: ExportRequest; response: boolean };
  'GET /runtime/free-hosting-enabled': { request: never; response: boolean };
  'PUT /runtime/free-hosting-enabled': { request: Record<string, unknown>; response: boolean };
  'DELETE /runtime/friends': { request: FriendsListRequest; response: string[] };
  'GET /runtime/friends': { request: never; response: string[] };
  'PUT /runtime/friends': { request: FriendsListRequest; response: string[] };
  'GET /runtime/friends/:did': { request: never; response: unknown };
  'POST /runtime/friends/:did/message': { request: FriendSendMessageRequest; response: boolean };
  'GET /runtime/hc/agent-infos': { request: never; response: string[] };
  'POST /runtime/hc/agent-infos': { request: AddAgentInfosRequest; response: boolean };
  'POST /runtime/holochain/restart': { request: never; response: boolean };
  'POST /runtime/import': { request: ImportRequest; response: unknown };
  'GET /runtime/info': { request: never; response: RuntimeInfo };
  'DELETE /runtime/link-language-templates': { request: LinkLanguageTemplatesRequest; response: string[] };
  'GET /runtime/link-language-templates': { request: never; response: string[] };
  'PUT /runtime/link-language-templates': { request: LinkLanguageTemplatesRequest; response: string[] };
  'GET /runtime/messages/inbox': { request: never; response: unknown };
  'GET /runtime/messages/outbox': { request: never; response: unknown };
  'GET /runtime/network-metrics': { request: never; response: string };
  'GET /runtime/notifications': { request: never; response: Notification[] };
  'POST /runtime/notifications': { request: NotificationInput; response: string };
  'DELETE /runtime/notifications/:id': { request: never; response: boolean };
  'PATCH /runtime/notifications/:id': { request: NotificationInput; response: boolean };
  'PATCH /runtime/notifications/:id/grant': { request: NotificationGrantRequest; response: boolean };
  'POST /runtime/open-link': { request: OpenLinkRequest; response: boolean };
  'POST /runtime/quit': { request: never; response: boolean };
  'PUT /runtime/status': { request: SetStatusRequest; response: boolean };
  'GET /runtime/tls-domain': { request: never; response: string | null };
  'POST /runtime/verify-signature': { request: VerifySignatureRequest; response: boolean };
  'GET /users': { request: never; response: UserStatistics[] };
  'POST /users': { request: CreateUserRequest; response: CreateUserResponse };
  'GET /users/:email/wallet': { request: never; response: string };
  'POST /users/free-access': { request: SetUserFreeAccessRequest; response: boolean };
  'POST /users/login': { request: LoginUserRequest; response: string };
  'GET /users/multi-user-enabled': { request: never; response: boolean };
  'PUT /users/multi-user-enabled': { request: SetMultiUserRequest; response: boolean };
  'POST /users/request-verification': { request: RequestVerificationRequest; response: VerificationRequestResult };
  'POST /users/verify-email': { request: VerifyEmailRequest; response: string };
  'GET /ws/audio': { request: never; response: void };
}

/** Extract the HTTP method from a route key */
export type RouteMethod<K extends keyof RouteMap> = K extends `${infer M} ${string}` ? M : never;

/** Extract the path from a route key */
export type RoutePath<K extends keyof RouteMap> = K extends `${string} ${infer P}` ? P : never;

/** All GET route keys */
export type GetRoutes = { [K in keyof RouteMap]: K extends `GET ${string}` ? K : never }[keyof RouteMap];

/** All POST route keys */
export type PostRoutes = { [K in keyof RouteMap]: K extends `POST ${string}` ? K : never }[keyof RouteMap];

/** All PUT route keys */
export type PutRoutes = { [K in keyof RouteMap]: K extends `PUT ${string}` ? K : never }[keyof RouteMap];

/** All PATCH route keys */
export type PatchRoutes = { [K in keyof RouteMap]: K extends `PATCH ${string}` ? K : never }[keyof RouteMap];

/** All DELETE route keys */
export type DeleteRoutes = { [K in keyof RouteMap]: K extends `DELETE ${string}` ? K : never }[keyof RouteMap];
