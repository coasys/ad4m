//! Generates `core/src/generated/rest/routes.ts` from the route registry.

use crate::rest::route_registry::ROUTES;

#[test]
fn generate_route_map() {
    let mut ts = String::new();
    ts.push_str("// AUTO-GENERATED — do not edit manually\n");
    ts.push_str("// Regenerate: cd rust-executor && cargo test generate_route_map\n\n");

    // Import REST request/response types from generated index
    ts.push_str("import type {\n");
    ts.push_str("  AddAgentInfosRequest,\n");
    ts.push_str("  AddLinkExpressionRequest,\n");
    ts.push_str("  AddLinkRequest,\n");
    ts.push_str("  AddLinksBulkRequest,\n");
    ts.push_str("  AddSdnaRequest,\n");
    ts.push_str("  ApplyTemplateRequest,\n");
    ts.push_str("  BroadcastRequest,\n");
    ts.push_str("  CommitBatchRequest,\n");
    ts.push_str("  CreateExpressionRequest,\n");
    ts.push_str("  CreatePerspectiveRequest,\n");
    ts.push_str("  CreateSubjectRequest,\n");
    ts.push_str("  CreateUserRequest,\n");
    ts.push_str("  DecoratedLinkExpression,\n");
    ts.push_str("  DisposeQueryRequest,\n");
    ts.push_str("  EmailTestRequest,\n");
    ts.push_str("  EmbedRequest,\n");
    ts.push_str("  EntanglementProofPreflightRequest,\n");
    ts.push_str("  ExecuteCommandsRequest,\n");
    ts.push_str("  ExportRequest,\n");
    ts.push_str("  ExpressionManyRequest,\n");
    ts.push_str("  FriendSendMessageRequest,\n");
    ts.push_str("  FriendsListRequest,\n");
    ts.push_str("  GenerateAgentRequest,\n");
    ts.push_str("  GenerateJwtRequest,\n");
    ts.push_str("  GetSubjectDataRequest,\n");
    ts.push_str("  HostingInfoResponse,\n");
    ts.push_str("  HostingWalletResponse,\n");
    ts.push_str("  ImportAgentRequest,\n");
    ts.push_str("  ImportRequest,\n");
    ts.push_str("  JoinNeighbourhoodRequest,\n");
    ts.push_str("  KeepAliveQueryRequest,\n");
    ts.push_str("  LinkLanguageTemplatesRequest,\n");
    ts.push_str("  LinkMutationResponse,\n");
    ts.push_str("  LinkMutationsRequest,\n");
    ts.push_str("  LockAgentRequest,\n");
    ts.push_str("  LoginUserRequest,\n");
    ts.push_str("  OpenLinkRequest,\n");
    ts.push_str("  PermitCapabilityRequest,\n");
    ts.push_str("  PromptRequest,\n");
    ts.push_str("  PublishLanguageRequest,\n");
    ts.push_str("  PublishNeighbourhoodRequest,\n");
    ts.push_str("  QueryRequest,\n");
    ts.push_str("  RemoveLinkRequest,\n");
    ts.push_str("  RemoveLinksBulkRequest,\n");
    ts.push_str("  RequestCapabilityRequest,\n");
    ts.push_str("  RequestPaymentRequest,\n");
    ts.push_str("  RequestVerificationRequest,\n");
    ts.push_str("  SetDefaultModelRequest,\n");
    ts.push_str("  SetHotWalletAddressRequest,\n");
    ts.push_str("  SetMultiUserRequest,\n");
    ts.push_str("  SetOnlineStatusRequest,\n");
    ts.push_str("  SetStatusRequest,\n");
    ts.push_str("  SignMessageRequest,\n");
    ts.push_str("  SignalRequest,\n");
    ts.push_str("  SubscribeQueryRequest,\n");
    ts.push_str("  SubscribeQueryResponse,\n");
    ts.push_str("  UnlockAgentRequest,\n");
    ts.push_str("  UpdateLinkRequest,\n");
    ts.push_str("  UpdatePerspectiveRequest,\n");
    ts.push_str("  UpdateProfileRequest,\n");
    ts.push_str("  VerifyEmailRequest,\n");
    ts.push_str("  VerifySignatureRequest,\n");
    ts.push_str("} from './index';\n\n");

    // Import domain types from the SDK (via wildcard re-exports in core/src/index.ts)
    ts.push_str("import type {\n");
    ts.push_str("  Agent,\n");
    ts.push_str("  AgentSignature,\n");
    ts.push_str("  AgentStatus,\n");
    ts.push_str("  Apps,\n");
    ts.push_str("  EntanglementProof,\n");
    ts.push_str("  EntanglementProofInput,\n");
    ts.push_str("  InteractionCall,\n");
    ts.push_str("  InteractionMeta,\n");
    ts.push_str("  LanguageHandle,\n");
    ts.push_str("  LanguageMeta,\n");
    ts.push_str("  LanguageRef,\n");
    ts.push_str("  Notification,\n");
    ts.push_str("  NotificationInput,\n");
    ts.push_str("  OnlineAgent,\n");
    ts.push_str("  Perspective,\n");
    ts.push_str("  PerspectiveHandle,\n");
    ts.push_str("  RuntimeInfo,\n");
    ts.push_str("} from '../../index';\n\n");

    // AI types not re-exported from core index
    ts.push_str("import type { AITask } from '../../ai/Tasks';\n");
    ts.push_str("import type { Model } from '../../ai/AITypes';\n\n");

    // Transcription types (not yet exported via ts-rs)
    ts.push_str("// Transcription request types (not yet in ts-rs exports)\n");
    ts.push_str("export interface OpenTranscriptionRequest {\n");
    ts.push_str("  model_id: string;\n");
    ts.push_str("  params?: {\n");
    ts.push_str("    threshold?: number;\n");
    ts.push_str("    min_speech_duration_ms?: number;\n");
    ts.push_str("    min_silence_duration_ms?: number;\n");
    ts.push_str("    speech_pad_ms?: number;\n");
    ts.push_str("    max_speech_duration_s?: number;\n");
    ts.push_str("  };\n");
    ts.push_str("}\n\n");
    ts.push_str("export interface FeedTranscriptionRequest {\n");
    ts.push_str("  stream_id: string;\n");
    ts.push_str("  audio_base64: string;\n");
    ts.push_str("}\n\n");
    ts.push_str("export interface CloseTranscriptionRequest {\n");
    ts.push_str("  stream_id: string;\n");
    ts.push_str("}\n\n");

    // Generate the RouteMap
    ts.push_str("export interface RouteMap {\n");
    for route in ROUTES {
        ts.push_str(&format!(
            "  '{} {}': {{ request: {}; response: {} }};\n",
            route.method, route.path, route.request_type, route.response_type
        ));
    }
    ts.push_str("}\n\n");

    // Helper types
    ts.push_str("/** Extract the HTTP method from a route key */\n");
    ts.push_str("export type RouteMethod<K extends keyof RouteMap> = K extends `${infer M} ${string}` ? M : never;\n\n");
    ts.push_str("/** Extract the path from a route key */\n");
    ts.push_str("export type RoutePath<K extends keyof RouteMap> = K extends `${string} ${infer P}` ? P : never;\n\n");
    ts.push_str("/** All GET route keys */\n");
    ts.push_str("export type GetRoutes = { [K in keyof RouteMap]: K extends `GET ${string}` ? K : never }[keyof RouteMap];\n\n");
    ts.push_str("/** All POST route keys */\n");
    ts.push_str("export type PostRoutes = { [K in keyof RouteMap]: K extends `POST ${string}` ? K : never }[keyof RouteMap];\n\n");
    ts.push_str("/** All PUT route keys */\n");
    ts.push_str("export type PutRoutes = { [K in keyof RouteMap]: K extends `PUT ${string}` ? K : never }[keyof RouteMap];\n\n");
    ts.push_str("/** All PATCH route keys */\n");
    ts.push_str("export type PatchRoutes = { [K in keyof RouteMap]: K extends `PATCH ${string}` ? K : never }[keyof RouteMap];\n\n");
    ts.push_str("/** All DELETE route keys */\n");
    ts.push_str("export type DeleteRoutes = { [K in keyof RouteMap]: K extends `DELETE ${string}` ? K : never }[keyof RouteMap];\n");

    let out_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../core/src/generated/rest/routes.ts");
    std::fs::write(&out_path, &ts).unwrap_or_else(|e| {
        panic!("Failed to write {}: {}", out_path.display(), e);
    });
    println!("Generated {}", out_path.display());
}
