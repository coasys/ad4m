import { AiInference_1_0_0 } from '../generated/services/ai.inference'
import type { AiInference_1_0_0_Events } from '../generated/services/ai.inference'
import { AiModels_1_0_0 } from '../generated/services/ai.models'
import type { AiModels_1_0_0_Events } from '../generated/services/ai.models'
import { BillingLedger_1_0_0 } from '../generated/services/billing.ledger'
import type { BillingLedger_1_0_0_Events } from '../generated/services/billing.ledger'

/**
 * Events `ad4m.on` keeps its names for, though built-in services emit them:
 * the name → the service event's payload.
 */
export interface ServiceEventAliasMap {
    'transcription-text': AiInference_1_0_0_Events['transcription-text']
    'model-loading-status': AiModels_1_0_0_Events['model-loading-status']
    'hosting-user-info-changed': BillingLedger_1_0_0_Events['account-changed']
}

/** Each alias → the service event type on the wire, and its scope field. */
export const SERVICE_EVENT_ALIASES: Record<keyof ServiceEventAliasMap, { type: string; scopeField?: string }> = {
    'transcription-text': { type: `${AiInference_1_0_0.hash}.transcription-text`, scopeField: AiInference_1_0_0.scopes['transcription-text'] },
    'model-loading-status': { type: `${AiModels_1_0_0.hash}.model-loading-status` },
    'hosting-user-info-changed': { type: `${BillingLedger_1_0_0.hash}.account-changed` },
}
