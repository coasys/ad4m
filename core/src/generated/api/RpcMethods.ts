// Auto-generated from the executor's WS-RPC HandlerMap (rust-executor/src/api/ws_handler.rs).
// Do NOT edit manually — regenerate with: pnpm run generate:api-types

import type { AIModelLoadingStatus } from "./AIModelLoadingStatus";
import type { AITask } from "./AITask";
import type { AddAgentInfosRequest } from "./AddAgentInfosRequest";
import type { Agent } from "./Agent";
import type { AgentAddEntanglementProofsParams } from "./AgentAddEntanglementProofsParams";
import type { AgentByDidParams } from "./AgentByDidParams";
import type { AgentRemoveAppParams } from "./AgentRemoveAppParams";
import type { AgentRevokeTokenParams } from "./AgentRevokeTokenParams";
import type { AgentSignature } from "./AgentSignature";
import type { AgentStatus } from "./AgentStatus";
import type { AiAddModelParams } from "./AiAddModelParams";
import type { AiAddTaskParams } from "./AiAddTaskParams";
import type { AiDiscoverModelsParams } from "./AiDiscoverModelsParams";
import type { AiGetDefaultModelParams } from "./AiGetDefaultModelParams";
import type { AiIdParams } from "./AiIdParams";
import type { AiModelLoadingStatusParams } from "./AiModelLoadingStatusParams";
import type { AiSetDefaultModelParams } from "./AiSetDefaultModelParams";
import type { AiTranscriptionCloseParams } from "./AiTranscriptionCloseParams";
import type { AiTranscriptionOpenParams } from "./AiTranscriptionOpenParams";
import type { AiUpdateModelParams } from "./AiUpdateModelParams";
import type { AiUpdateTaskParams } from "./AiUpdateTaskParams";
import type { ApplyTemplateRequest } from "./ApplyTemplateRequest";
import type { Apps } from "./Apps";
import type { ComputeLogEntry } from "./ComputeLogEntry";
import type { CreateExpressionRequest } from "./CreateExpressionRequest";
import type { CreatePerspectiveRequest } from "./CreatePerspectiveRequest";
import type { CreateUserRequest } from "./CreateUserRequest";
import type { DecoratedLinkExpression } from "./DecoratedLinkExpression";
import type { DecoratedPerspective } from "./DecoratedPerspective";
import type { EmbedRequest } from "./EmbedRequest";
import type { EntanglementProof } from "./EntanglementProof";
import type { EntanglementProofPreflightRequest } from "./EntanglementProofPreflightRequest";
import type { EntanglementProofsWrapper } from "./EntanglementProofsWrapper";
import type { EventName } from "./Events";
import type { ExportRequest } from "./ExportRequest";
import type { ExpressionInteractRequest } from "./ExpressionInteractRequest";
import type { ExpressionManyRequest } from "./ExpressionManyRequest";
import type { ExpressionRendered } from "./ExpressionRendered";
import type { ExpressionUrlRequest } from "./ExpressionUrlRequest";
import type { FireOutcome } from "./FireOutcome";
import type { FriendsListRequest } from "./FriendsListRequest";
import type { GenerateAgentRequest } from "./GenerateAgentRequest";
import type { GenerateJwtRequest } from "./GenerateJwtRequest";
import type { HostRate } from "./HostRate";
import type { HostingInfoResult } from "./HostingInfoResult";
import type { HostingRequestPaymentResult } from "./HostingRequestPaymentResult";
import type { HostingWalletResult } from "./HostingWalletResult";
import type { ImportAgentRequest } from "./ImportAgentRequest";
import type { ImportRequest } from "./ImportRequest";
import type { InteractionMeta } from "./InteractionMeta";
import type { JoinNeighbourhoodRequest } from "./JoinNeighbourhoodRequest";
import type { JsonValue } from "./serde_json/JsonValue";
import type { LanguageAddressParams } from "./LanguageAddressParams";
import type { LanguageHandle } from "./LanguageHandle";
import type { LanguageListParams } from "./LanguageListParams";
import type { LanguageMeta } from "./LanguageMeta";
import type { LanguageRef } from "./LanguageRef";
import type { LanguageWriteSettingsParams } from "./LanguageWriteSettingsParams";
import type { LinkLanguageTemplatesRequest } from "./LinkLanguageTemplatesRequest";
import type { LockAgentRequest } from "./LockAgentRequest";
import type { Model } from "./Model";
import type { NeighbourhoodBroadcastParams } from "./NeighbourhoodBroadcastParams";
import type { NeighbourhoodOnlineStatusParams } from "./NeighbourhoodOnlineStatusParams";
import type { NeighbourhoodSignalParams } from "./NeighbourhoodSignalParams";
import type { NeighbourhoodUuidParams } from "./NeighbourhoodUuidParams";
import type { Notification } from "./Notification";
import type { NotificationInput } from "./NotificationInput";
import type { OnlineAgent } from "./OnlineAgent";
import type { OpenLinkRequest } from "./OpenLinkRequest";
import type { OverlayView } from "./OverlayView";
import type { PermitCapabilityRequest } from "./PermitCapabilityRequest";
import type { PerspectiveAddAutoProcessorParams } from "./PerspectiveAddAutoProcessorParams";
import type { PerspectiveAddLinkExpressionParams } from "./PerspectiveAddLinkExpressionParams";
import type { PerspectiveAddLinkParams } from "./PerspectiveAddLinkParams";
import type { PerspectiveAddLinksParams } from "./PerspectiveAddLinksParams";
import type { PerspectiveAddSdnaParams } from "./PerspectiveAddSdnaParams";
import type { PerspectiveAddSdnaResult } from "./PerspectiveAddSdnaResult";
import type { PerspectiveCommitBatchParams } from "./PerspectiveCommitBatchParams";
import type { PerspectiveCreateSubjectParams } from "./PerspectiveCreateSubjectParams";
import type { PerspectiveEvaluateGettersParams } from "./PerspectiveEvaluateGettersParams";
import type { PerspectiveExecuteCommandsParams } from "./PerspectiveExecuteCommandsParams";
import type { PerspectiveExpression } from "./PerspectiveExpression";
import type { PerspectiveFlowProposalParams } from "./PerspectiveFlowProposalParams";
import type { PerspectiveFlowReceiptVerdict } from "./PerspectiveFlowReceiptVerdict";
import type { PerspectiveFlowValidOutputsParams } from "./PerspectiveFlowValidOutputsParams";
import type { PerspectiveGetSubjectDataParams } from "./PerspectiveGetSubjectDataParams";
import type { PerspectiveHandle } from "./PerspectiveHandle";
import type { PerspectiveLinkDiff } from "./PerspectiveLinkDiff";
import type { PerspectiveLinkMutationsParams } from "./PerspectiveLinkMutationsParams";
import type { PerspectiveMintFlowReceiptParams } from "./PerspectiveMintFlowReceiptParams";
import type { PerspectiveMintFlowReceiptResult } from "./PerspectiveMintFlowReceiptResult";
import type { PerspectiveModelQueryParams } from "./PerspectiveModelQueryParams";
import type { PerspectiveNamedShacl } from "./PerspectiveNamedShacl";
import type { PerspectiveProposeFlowTransitionParams } from "./PerspectiveProposeFlowTransitionParams";
import type { PerspectiveQueryLinksParams } from "./PerspectiveQueryLinksParams";
import type { PerspectiveQueryParams } from "./PerspectiveQueryParams";
import type { PerspectiveRejectFlowProposalResult } from "./PerspectiveRejectFlowProposalResult";
import type { PerspectiveRemoveAutoProcessorParams } from "./PerspectiveRemoveAutoProcessorParams";
import type { PerspectiveRemoveLinkParams } from "./PerspectiveRemoveLinkParams";
import type { PerspectiveRemoveLinksParams } from "./PerspectiveRemoveLinksParams";
import type { PerspectiveResolveInterpretationParams } from "./PerspectiveResolveInterpretationParams";
import type { PerspectiveResynced } from "./PerspectiveResynced";
import type { PerspectiveShacl } from "./PerspectiveShacl";
import type { PerspectiveShaclNameParams } from "./PerspectiveShaclNameParams";
import type { PerspectiveSparqlParams } from "./PerspectiveSparqlParams";
import type { PerspectiveSubjectClassesOfParams } from "./PerspectiveSubjectClassesOfParams";
import type { PerspectiveSubscribed } from "./PerspectiveSubscribed";
import type { PerspectiveSubscriptionParams } from "./PerspectiveSubscriptionParams";
import type { PerspectiveUpdateLinkParams } from "./PerspectiveUpdateLinkParams";
import type { PerspectiveUpdateParams } from "./PerspectiveUpdateParams";
import type { PerspectiveUuidParams } from "./PerspectiveUuidParams";
import type { PerspectiveVerifyFlowReceiptParams } from "./PerspectiveVerifyFlowReceiptParams";
import type { PromptRequest } from "./PromptRequest";
import type { ProposeOutcome } from "./ProposeOutcome";
import type { PublishLanguageRequest } from "./PublishLanguageRequest";
import type { PublishNeighbourhoodRequest } from "./PublishNeighbourhoodRequest";
import type { RequestCapabilityRequest } from "./RequestCapabilityRequest";
import type { RequestPaymentRequest } from "./RequestPaymentRequest";
import type { RunInterpretationRequest } from "./RunInterpretationRequest";
import type { RunInterpretationWithHarnessRequest } from "./RunInterpretationWithHarnessRequest";
import type { RuntimeComputeLogParams } from "./RuntimeComputeLogParams";
import type { RuntimeFriendStatusParams } from "./RuntimeFriendStatusParams";
import type { RuntimeGrantNotificationParams } from "./RuntimeGrantNotificationParams";
import type { RuntimeImportResult } from "./RuntimeImportResult";
import type { RuntimeInfo } from "./RuntimeInfo";
import type { RuntimeNotificationIdParams } from "./RuntimeNotificationIdParams";
import type { RuntimeSendFriendMessageParams } from "./RuntimeSendFriendMessageParams";
import type { RuntimeSetFreeHostingEnabledParams } from "./RuntimeSetFreeHostingEnabledParams";
import type { RuntimeUnytSendHotParams } from "./RuntimeUnytSendHotParams";
import type { RuntimeUnytWalletHistoryParams } from "./RuntimeUnytWalletHistoryParams";
import type { RuntimeUpdateNotificationParams } from "./RuntimeUpdateNotificationParams";
import type { SentMessage } from "./SentMessage";
import type { SetHostRatesRequest } from "./SetHostRatesRequest";
import type { SetHotWalletAddressRequest } from "./SetHotWalletAddressRequest";
import type { SetMultiUserRequest } from "./SetMultiUserRequest";
import type { SetStatusRequest } from "./SetStatusRequest";
import type { SetUnytMembraneProofRequest } from "./SetUnytMembraneProofRequest";
import type { SetUserFreeAccessRequest } from "./SetUserFreeAccessRequest";
import type { SignMessageRequest } from "./SignMessageRequest";
import type { TrustedAgentsWrapper } from "./TrustedAgentsWrapper";
import type { UnlockAgentRequest } from "./UnlockAgentRequest";
import type { UnytVersionInfo } from "./UnytVersionInfo";
import type { UpdateProfileRequest } from "./UpdateProfileRequest";
import type { UserCreationResult } from "./UserCreationResult";
import type { UserStatistics } from "./UserStatistics";
import type { UsersEmailParams } from "./UsersEmailParams";
import type { UsersEmailTestParams } from "./UsersEmailTestParams";
import type { UsersEmailTestResult } from "./UsersEmailTestResult";
import type { UsersLoginParams } from "./UsersLoginParams";
import type { UsersRequestVerificationParams } from "./UsersRequestVerificationParams";
import type { UsersSetCreditsParams } from "./UsersSetCreditsParams";
import type { UsersVerifyEmailParams } from "./UsersVerifyEmailParams";
import type { ValidOutput } from "./ValidOutput";
import type { VerificationRequestResult } from "./VerificationRequestResult";
import type { VerifySignatureRequest } from "./VerifySignatureRequest";

/** Every executor RPC method: its params and result. */
export interface RpcMethods {
  "agent.addEntanglementProofs": { params: AgentAddEntanglementProofsParams; result: Array<EntanglementProof> };
  "agent.addTrustedAgents": { params: TrustedAgentsWrapper; result: Array<string> };
  "agent.byDid": { params: AgentByDidParams; result: Agent | null };
  "agent.deleteEntanglementProofs": { params: EntanglementProofsWrapper; result: Array<EntanglementProof> };
  "agent.deleteTrustedAgents": { params: TrustedAgentsWrapper; result: Array<string> };
  "agent.entanglementProofPreflight": { params: EntanglementProofPreflightRequest; result: EntanglementProof };
  "agent.generate": { params: GenerateAgentRequest; result: AgentStatus };
  "agent.generateJwt": { params: GenerateJwtRequest; result: string };
  "agent.get": { params: Record<string, never>; result: Agent };
  "agent.getApps": { params: Record<string, never>; result: Array<Apps> };
  "agent.getEntanglementProofs": { params: Record<string, never>; result: Array<EntanglementProof> };
  "agent.getTrustedAgents": { params: Record<string, never>; result: Array<string> };
  "agent.import": { params: ImportAgentRequest; result: AgentStatus };
  "agent.isLocked": { params: Record<string, never>; result: boolean };
  "agent.lock": { params: LockAgentRequest; result: AgentStatus };
  "agent.permitCapability": { params: PermitCapabilityRequest; result: string };
  "agent.removeApp": { params: AgentRemoveAppParams; result: Array<Apps> };
  "agent.requestCapability": { params: RequestCapabilityRequest; result: string };
  "agent.revokeToken": { params: AgentRevokeTokenParams; result: Array<Apps> };
  "agent.sign": { params: SignMessageRequest; result: AgentSignature };
  "agent.status": { params: Record<string, never>; result: AgentStatus };
  "agent.unlock": { params: UnlockAgentRequest; result: AgentStatus };
  "agent.updateProfile": { params: UpdateProfileRequest; result: Agent };
  "ai.addModel": { params: AiAddModelParams; result: string };
  "ai.addTask": { params: AiAddTaskParams; result: AITask };
  "ai.discoverModels": { params: AiDiscoverModelsParams; result: Array<string> };
  "ai.embed": { params: EmbedRequest; result: string };
  "ai.getDefaultModel": { params: AiGetDefaultModelParams; result: Model | null };
  "ai.modelLoadingStatus": { params: AiModelLoadingStatusParams; result: AIModelLoadingStatus };
  "ai.models": { params: Record<string, never>; result: Array<Model> };
  "ai.prompt": { params: PromptRequest; result: string };
  "ai.removeModel": { params: AiIdParams; result: boolean };
  "ai.removeTask": { params: AiIdParams; result: boolean };
  "ai.setDefaultModel": { params: AiSetDefaultModelParams; result: boolean };
  "ai.tasks": { params: Record<string, never>; result: Array<AITask> };
  "ai.transcriptionClose": { params: AiTranscriptionCloseParams; result: string };
  "ai.transcriptionOpen": { params: AiTranscriptionOpenParams; result: string };
  "ai.updateModel": { params: AiUpdateModelParams; result: boolean };
  "ai.updateTask": { params: AiUpdateTaskParams; result: AITask };
  "events.unwatch": { params: Record<string, never>; result: boolean };
  "events.watch": { params: Partial<Record<EventName, Array<string> | null>>; result: boolean };
  "expression.create": { params: CreateExpressionRequest; result: string };
  "expression.get": { params: ExpressionUrlRequest; result: ExpressionRendered | null };
  "expression.getMany": { params: ExpressionManyRequest; result: Array<ExpressionRendered | null> };
  "expression.getRaw": { params: ExpressionUrlRequest; result: string | null };
  "expression.interact": { params: ExpressionInteractRequest; result: string };
  "expression.interactions": { params: ExpressionUrlRequest; result: Array<InteractionMeta> };
  "hosting.info": { params: Record<string, never>; result: HostingInfoResult };
  "hosting.requestPayment": { params: RequestPaymentRequest; result: HostingRequestPaymentResult };
  "hosting.setHotWallet": { params: SetHotWalletAddressRequest; result: boolean };
  "hosting.wallet": { params: Record<string, never>; result: HostingWalletResult };
  "hosting.walletHistory": { params: Record<string, never>; result: JsonValue };
  "language.all": { params: LanguageListParams; result: Array<LanguageHandle> };
  "language.applyTemplate": { params: ApplyTemplateRequest; result: LanguageRef };
  "language.get": { params: LanguageAddressParams; result: LanguageHandle };
  "language.meta": { params: LanguageAddressParams; result: LanguageMeta };
  "language.publish": { params: PublishLanguageRequest; result: LanguageMeta };
  "language.remove": { params: LanguageAddressParams; result: boolean };
  "language.source": { params: LanguageAddressParams; result: string };
  "language.writeSettings": { params: LanguageWriteSettingsParams; result: boolean };
  "neighbourhood.hasTelepresence": { params: NeighbourhoodUuidParams; result: boolean };
  "neighbourhood.join": { params: JoinNeighbourhoodRequest; result: PerspectiveHandle };
  "neighbourhood.onlineAgents": { params: NeighbourhoodUuidParams; result: Array<OnlineAgent> };
  "neighbourhood.otherAgents": { params: NeighbourhoodUuidParams; result: Array<string> };
  "neighbourhood.publish": { params: PublishNeighbourhoodRequest; result: string };
  "neighbourhood.sendBroadcast": { params: NeighbourhoodBroadcastParams; result: boolean };
  "neighbourhood.sendSignal": { params: NeighbourhoodSignalParams; result: boolean };
  "neighbourhood.setOnlineStatus": { params: NeighbourhoodOnlineStatusParams; result: boolean };
  "perspective.acceptFlowProposal": { params: PerspectiveFlowProposalParams; result: Array<FireOutcome> };
  "perspective.acceptInterpretation": { params: PerspectiveResolveInterpretationParams; result: boolean };
  "perspective.addAutoProcessor": { params: PerspectiveAddAutoProcessorParams; result: string };
  "perspective.addLink": { params: PerspectiveAddLinkParams; result: DecoratedLinkExpression };
  "perspective.addLinkExpression": { params: PerspectiveAddLinkExpressionParams; result: DecoratedLinkExpression };
  "perspective.addLinks": { params: PerspectiveAddLinksParams; result: Array<DecoratedLinkExpression> };
  "perspective.addSdna": { params: PerspectiveAddSdnaParams; result: PerspectiveAddSdnaResult };
  "perspective.all": { params: Record<string, never>; result: Array<PerspectiveHandle> };
  "perspective.commitBatch": { params: PerspectiveCommitBatchParams; result: PerspectiveLinkDiff };
  "perspective.create": { params: CreatePerspectiveRequest; result: PerspectiveHandle };
  "perspective.createBatch": { params: PerspectiveUuidParams; result: string };
  "perspective.createSubject": { params: PerspectiveCreateSubjectParams; result: boolean };
  "perspective.disposeQuery": { params: PerspectiveSubscriptionParams; result: boolean };
  "perspective.disposeSparql": { params: PerspectiveSubscriptionParams; result: boolean };
  "perspective.evaluateGetters": { params: PerspectiveEvaluateGettersParams; result: string };
  "perspective.executeCommands": { params: PerspectiveExecuteCommandsParams; result: null };
  "perspective.flowValidOutputs": { params: PerspectiveFlowValidOutputsParams; result: Array<ValidOutput> };
  "perspective.get": { params: PerspectiveUuidParams; result: PerspectiveHandle | null };
  "perspective.getAllShacl": { params: PerspectiveUuidParams; result: Array<PerspectiveNamedShacl> };
  "perspective.getShacl": { params: PerspectiveShaclNameParams; result: PerspectiveShacl | null };
  "perspective.getShaclNames": { params: PerspectiveUuidParams; result: Array<string> };
  "perspective.getShaclTargetClass": { params: PerspectiveShaclNameParams; result: string | null };
  "perspective.getSubjectData": { params: PerspectiveGetSubjectDataParams; result: string };
  "perspective.interpretationOverlays": { params: PerspectiveUuidParams; result: Array<OverlayView> };
  "perspective.linkMutations": { params: PerspectiveLinkMutationsParams; result: PerspectiveLinkDiff };
  "perspective.mintFlowReceipt": { params: PerspectiveMintFlowReceiptParams; result: PerspectiveMintFlowReceiptResult };
  "perspective.modelQuery": { params: PerspectiveModelQueryParams; result: string };
  "perspective.modelSubscribe": { params: PerspectiveModelQueryParams; result: PerspectiveSubscribed };
  "perspective.proposeFlowTransition": { params: PerspectiveProposeFlowTransitionParams; result: ProposeOutcome };
  "perspective.publishSnapshot": { params: PerspectiveUuidParams; result: string };
  "perspective.queryLinks": { params: PerspectiveQueryLinksParams; result: Array<DecoratedLinkExpression> };
  "perspective.queryProlog": { params: PerspectiveQueryParams; result: string };
  "perspective.querySparql": { params: PerspectiveSparqlParams; result: string };
  "perspective.rejectFlowProposal": { params: PerspectiveFlowProposalParams; result: PerspectiveRejectFlowProposalResult };
  "perspective.rejectInterpretation": { params: PerspectiveResolveInterpretationParams; result: boolean };
  "perspective.remove": { params: PerspectiveUuidParams; result: boolean };
  "perspective.removeAutoProcessor": { params: PerspectiveRemoveAutoProcessorParams; result: boolean };
  "perspective.removeLink": { params: PerspectiveRemoveLinkParams; result: boolean };
  "perspective.removeLinks": { params: PerspectiveRemoveLinksParams; result: Array<DecoratedLinkExpression> };
  "perspective.resyncSubscription": { params: PerspectiveSubscriptionParams; result: PerspectiveResynced };
  "perspective.runInterpretation": { params: RunInterpretationRequest; result: Array<string> };
  "perspective.runInterpretationWithHarness": { params: RunInterpretationWithHarnessRequest; result: Array<string> };
  "perspective.snapshot": { params: PerspectiveUuidParams; result: DecoratedPerspective | null };
  "perspective.subjectClassesOf": { params: PerspectiveSubjectClassesOfParams; result: { [key in string]: Array<string> } };
  "perspective.subscribeQuery": { params: PerspectiveQueryParams; result: PerspectiveSubscribed };
  "perspective.subscribeSparql": { params: PerspectiveQueryParams; result: PerspectiveSubscribed };
  "perspective.update": { params: PerspectiveUpdateParams; result: PerspectiveHandle };
  "perspective.updateLink": { params: PerspectiveUpdateLinkParams; result: DecoratedLinkExpression };
  "perspective.verifyFlowReceipt": { params: PerspectiveVerifyFlowReceiptParams; result: PerspectiveFlowReceiptVerdict };
  "runtime.addFriends": { params: FriendsListRequest; result: Array<string> };
  "runtime.addHcAgentInfos": { params: AddAgentInfosRequest; result: boolean };
  "runtime.addLinkLanguageTemplates": { params: LinkLanguageTemplatesRequest; result: Array<string> };
  "runtime.computeLog": { params: RuntimeComputeLogParams; result: Array<ComputeLogEntry> };
  "runtime.createNotification": { params: NotificationInput; result: string };
  "runtime.deleteNotification": { params: RuntimeNotificationIdParams; result: boolean };
  "runtime.exportData": { params: ExportRequest; result: boolean };
  "runtime.freeHostingEnabled": { params: Record<string, never>; result: boolean };
  "runtime.friendStatus": { params: RuntimeFriendStatusParams; result: PerspectiveExpression | null };
  "runtime.friends": { params: Record<string, never>; result: Array<string> };
  "runtime.grantNotification": { params: RuntimeGrantNotificationParams; result: boolean };
  "runtime.hcAgentInfos": { params: Record<string, never>; result: Array<string> };
  "runtime.hostRates": { params: Record<string, never>; result: Array<HostRate> };
  "runtime.importData": { params: ImportRequest; result: RuntimeImportResult };
  "runtime.inbox": { params: Record<string, never>; result: Array<PerspectiveExpression> };
  "runtime.info": { params: Record<string, never>; result: RuntimeInfo };
  "runtime.linkLanguageTemplates": { params: Record<string, never>; result: Array<string> };
  "runtime.networkMetrics": { params: Record<string, never>; result: string };
  "runtime.notifications": { params: Record<string, never>; result: Array<Notification> };
  "runtime.openLink": { params: OpenLinkRequest; result: boolean };
  "runtime.outbox": { params: Record<string, never>; result: Array<SentMessage> };
  "runtime.quit": { params: Record<string, never>; result: boolean };
  "runtime.removeFriends": { params: FriendsListRequest; result: Array<string> };
  "runtime.removeLinkLanguageTemplates": { params: LinkLanguageTemplatesRequest; result: Array<string> };
  "runtime.restartHolochain": { params: Record<string, never>; result: boolean };
  "runtime.sendFriendMessage": { params: RuntimeSendFriendMessageParams; result: boolean };
  "runtime.setFreeHostingEnabled": { params: RuntimeSetFreeHostingEnabledParams; result: boolean };
  "runtime.setHostRates": { params: SetHostRatesRequest; result: boolean };
  "runtime.setStatus": { params: SetStatusRequest; result: boolean };
  "runtime.setUnytMembraneProof": { params: SetUnytMembraneProofRequest; result: boolean };
  "runtime.tlsDomain": { params: Record<string, never>; result: string | null };
  "runtime.unytAgentKey": { params: Record<string, never>; result: null };
  "runtime.unytHotAgentPubkey": { params: Record<string, never>; result: null };
  "runtime.unytReinstallDna": { params: Record<string, never>; result: null };
  "runtime.unytSendHot": { params: RuntimeUnytSendHotParams; result: null };
  "runtime.unytVersionInfo": { params: Record<string, never>; result: UnytVersionInfo };
  "runtime.unytWalletBalance": { params: Record<string, never>; result: null };
  "runtime.unytWalletHistory": { params: RuntimeUnytWalletHistoryParams; result: null };
  "runtime.updateNotification": { params: RuntimeUpdateNotificationParams; result: boolean };
  "runtime.verifySignature": { params: VerifySignatureRequest; result: boolean };
  "user.create": { params: CreateUserRequest; result: UserCreationResult };
  "user.credits": { params: UsersSetCreditsParams; result: boolean };
  "user.emailTest": { params: UsersEmailTestParams; result: UsersEmailTestResult };
  "user.freeAccess": { params: SetUserFreeAccessRequest; result: boolean };
  "user.list": { params: Record<string, never>; result: Array<UserStatistics> };
  "user.login": { params: UsersLoginParams; result: string };
  "user.multiUserEnabled": { params: Record<string, never>; result: boolean };
  "user.requestVerification": { params: UsersRequestVerificationParams; result: VerificationRequestResult };
  "user.setMultiUserEnabled": { params: SetMultiUserRequest; result: boolean };
  "user.verifyEmail": { params: UsersVerifyEmailParams; result: string };
  "user.wallet": { params: UsersEmailParams; result: string };
}

export type RpcMethod = keyof RpcMethods;

/** Idempotent reads: the client resends one once after a reconnect. */
export const READ_METHODS: ReadonlySet<RpcMethod> = new Set<RpcMethod>([
  "agent.byDid",
  "agent.entanglementProofPreflight",
  "agent.get",
  "agent.getApps",
  "agent.getEntanglementProofs",
  "agent.getTrustedAgents",
  "agent.isLocked",
  "agent.status",
  "ai.discoverModels",
  "ai.getDefaultModel",
  "ai.modelLoadingStatus",
  "ai.models",
  "ai.tasks",
  "expression.get",
  "expression.getMany",
  "expression.getRaw",
  "expression.interactions",
  "hosting.info",
  "hosting.wallet",
  "hosting.walletHistory",
  "language.all",
  "language.get",
  "language.meta",
  "language.source",
  "neighbourhood.hasTelepresence",
  "neighbourhood.onlineAgents",
  "neighbourhood.otherAgents",
  "perspective.all",
  "perspective.evaluateGetters",
  "perspective.flowValidOutputs",
  "perspective.get",
  "perspective.getAllShacl",
  "perspective.getShacl",
  "perspective.getShaclNames",
  "perspective.getShaclTargetClass",
  "perspective.getSubjectData",
  "perspective.interpretationOverlays",
  "perspective.modelQuery",
  "perspective.queryLinks",
  "perspective.queryProlog",
  "perspective.querySparql",
  "perspective.snapshot",
  "perspective.subjectClassesOf",
  "perspective.verifyFlowReceipt",
  "runtime.computeLog",
  "runtime.freeHostingEnabled",
  "runtime.friendStatus",
  "runtime.friends",
  "runtime.hcAgentInfos",
  "runtime.hostRates",
  "runtime.inbox",
  "runtime.info",
  "runtime.linkLanguageTemplates",
  "runtime.networkMetrics",
  "runtime.notifications",
  "runtime.outbox",
  "runtime.tlsDomain",
  "runtime.unytAgentKey",
  "runtime.unytHotAgentPubkey",
  "runtime.unytVersionInfo",
  "runtime.unytWalletBalance",
  "runtime.unytWalletHistory",
  "runtime.verifySignature",
  "user.list",
  "user.multiUserEnabled",
  "user.wallet",
]);

/** Calls that can run for minutes: the client's default timeout is its long one. */
export const LONG_METHODS: ReadonlySet<RpcMethod> = new Set<RpcMethod>([
  "agent.generate",
  "agent.unlock",
  "ai.addModel",
  "ai.embed",
  "ai.prompt",
  "language.applyTemplate",
  "language.publish",
  "neighbourhood.join",
  "neighbourhood.publish",
  "perspective.runInterpretation",
  "perspective.runInterpretationWithHarness",
  "runtime.restartHolochain",
]);
