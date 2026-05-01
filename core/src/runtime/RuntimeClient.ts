import { RestClient } from "../restClient"
import { Perspective, PerspectiveExpression } from "../perspectives/Perspective"
import { RuntimeInfo, ExceptionInfo, SentMessage, NotificationInput, Notification, TriggeredNotification, ImportResult, UserStatistics } from "./RuntimeTypes"
import type {
    OpenLinkRequest,
    TrustedAgentsRequest,
    LinkLanguageTemplatesRequest,
    FriendsListRequest,
    VerifySignatureRequest,
    SetStatusRequest,
    FriendSendMessageRequest,
    ExportRequest,
    ImportRequest,
    SetMultiUserRequest,
    SetFreeHostingEnabledRequest,
} from "../generated/rest"

export type MessageCallback = (message: PerspectiveExpression) => null
export type ExceptionCallback = (info: ExceptionInfo) => null
export type NotificationTriggeredCallback = (notification: TriggeredNotification) => null
export type NotificationRequestedCallback = (notification: Notification) => null

function normalizeExceptionType(type: ExceptionInfo['type'] | string): ExceptionInfo['type'] {
    if (typeof type !== 'string' || type === type.toUpperCase()) {
        return type as ExceptionInfo['type']
    }

    return type
        .replace(/([a-z0-9])([A-Z])/g, '$1_$2')
        .replace(/([A-Z])([A-Z][a-z])/g, '$1_$2')
        .toUpperCase() as ExceptionInfo['type']
}

export class RuntimeClient {
    #restClient: RestClient
    #messageReceivedCallbacks: MessageCallback[]
    #exceptionOccurredCallbacks: ExceptionCallback[]
    #notificationTriggeredCallbacks: NotificationTriggeredCallback[]
    #notificationRequestedCallbacks: NotificationRequestedCallback[]
    #unsubscribers: (() => void)[]

    constructor(baseUrl: string, token?: string, subscribe: boolean = true, sharedRestClient?: RestClient) {
        this.#restClient = sharedRestClient || new RestClient(baseUrl, token)
        this.#messageReceivedCallbacks = []
        this.#exceptionOccurredCallbacks = []
        this.#notificationTriggeredCallbacks = []
        this.#notificationRequestedCallbacks = []
        this.#unsubscribers = []

        if(subscribe) {
            this.subscribeMessageReceived()
            this.subscribeExceptionOccurred()
            this.subscribeNotificationTriggered()
        }
    }

    async info(): Promise<RuntimeInfo> {
        return this.#restClient.call<RuntimeInfo>('runtime.info')
    }

    async tlsDomain(): Promise<string | null> {
        return this.#restClient.call<string | null>('runtime.tlsDomain')
    }

    async quit(): Promise<Boolean> {
        return this.#restClient.call<Boolean>('runtime.quit')
    }

    async openLink(url: string): Promise<Boolean> {
        return this.#restClient.call<Boolean>('runtime.openLink', { url })
    }

    async addTrustedAgents(agents: string[]): Promise<string[]> {
        return this.#restClient.call<string[]>('agent.addTrustedAgents', { agents })
    }

    async deleteTrustedAgents(agents: string[]): Promise<string[]> {
        return this.#restClient.call<string[]>('agent.deleteTrustedAgents', { agents })
    }

    async getTrustedAgents(): Promise<string[]> {
        return this.#restClient.call<string[]>('agent.getTrustedAgents')
    }

    async addKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        return this.#restClient.call<string[]>('runtime.addLinkLanguageTemplates', { addresses })
    }

    async removeKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        return this.#restClient.call<string[]>('runtime.removeLinkLanguageTemplates', { addresses })
    }

    async knownLinkLanguageTemplates(): Promise<string[]> {
        return this.#restClient.call<string[]>('runtime.linkLanguageTemplates')
    }

    async addFriends(dids: string[]): Promise<string[]> {
        return this.#restClient.call<string[]>('runtime.addFriends', { dids })
    }

    async removeFriends(dids: string[]): Promise<string[]> {
        return this.#restClient.call<string[]>('runtime.removeFriends', { dids })
    }

    async friends(): Promise<string[]> {
        return this.#restClient.call<string[]>('runtime.friends')
    }

    async hcAgentInfos(): Promise<string[]> {
        return this.#restClient.call<string[]>('runtime.hcAgentInfos')
    }

    async getNetworkMetrics(): Promise<string> {
        return this.#restClient.call<string>('runtime.networkMetrics')
    }

    async restartHolochain(): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.restartHolochain')
    }

    async hcAddAgentInfos(agentInfos: string[]): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.addHcAgentInfos', { agentInfos })
    }

    async verifyStringSignedByDid(did: string, didSigningKeyId: string, data: string, signedData: string): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.verifySignature', { did, didSigningKeyId, data, signedData })
    }

    async setStatus(perspective: Perspective): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.setStatus', { status: perspective })
    }

    async friendStatus(did: string): Promise<PerspectiveExpression> {
        return this.#restClient.call<PerspectiveExpression>('runtime.friendStatus', { did })
    }

    async friendSendMessage(did: string, message: Perspective): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.sendFriendMessage', { did, message })
    }

    async messageInbox(filter?: string): Promise<PerspectiveExpression[]> {
        return this.#restClient.call<PerspectiveExpression[]>('runtime.inbox', { filter })
    }

    async messageOutbox(filter?: string): Promise<SentMessage[]> {
        return this.#restClient.call<SentMessage[]>('runtime.outbox', { filter })
    }

    async requestInstallNotification(notification: NotificationInput) {
        return this.#restClient.call('runtime.createNotification', { ...notification })
    }

    async grantNotification(id: string): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.grantNotification', { id, granted: true })
    }

    async exportDb(filePath: string): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.exportData', { type: "db", filePath })
    }

    async importDb(filePath: string): Promise<ImportResult> {
        return this.#restClient.call<ImportResult>('runtime.importData', { type: "db", filePath })
    }

    async notifications(): Promise<Notification[]> {
        return this.#restClient.call<Notification[]>('runtime.notifications')
    }

    async updateNotification(id: string, notification: NotificationInput): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.updateNotification', { ...notification, id })
    }

    async removeNotification(id: string): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.deleteNotification', { id })
    }

    async exportPerspective(uuid: string, filePath: string): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.exportData', { type: "perspective", perspectiveUuid: uuid, filePath })
    }

    async importPerspective(filePath: string): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.importData', { type: "perspective", filePath })
    }

    async multiUserEnabled(): Promise<boolean> {
        return this.#restClient.call<boolean>('user.multiUserEnabled')
    }

    async setMultiUserEnabled(enabled: boolean): Promise<boolean> {
        return this.#restClient.call<boolean>('user.setMultiUserEnabled', { enabled })
    }

    async freeHostingEnabled(): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.freeHostingEnabled')
    }

    async setFreeHostingEnabled(enabled: boolean): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.setFreeHostingEnabled', { enabled })
    }

    async listUsers(): Promise<UserStatistics[]> {
        return this.#restClient.call<UserStatistics[]>('user.list')
    }

    async userWalletAddress(email: string): Promise<string | null> {
        return this.#restClient.call<string | null>('user.wallet', { email })
    }

    async emailTestModeEnable(): Promise<boolean> {
        return this.#restClient.call<boolean>('user.emailTest', { action: 'enable' })
    }

    async emailTestModeDisable(): Promise<boolean> {
        return this.#restClient.call<boolean>('user.emailTest', { action: 'disable' })
    }

    async emailTestGetCode(email: string): Promise<string | null> {
        return this.#restClient.call<string | null>('user.emailTest', { action: 'get-code', email })
    }

    async emailTestClearCodes(): Promise<boolean> {
        return this.#restClient.call<boolean>('user.emailTest', { action: 'clear-codes' })
    }

    async emailTestSetExpiry(email: string, verificationType: string, expiresAt: number): Promise<boolean> {
        return this.#restClient.call<boolean>('user.emailTest', { action: 'set-expiry', email, verificationType, expiresAt })
    }

    // ---- Unyt / mHOT methods ----

    async unytAgentKey(): Promise<string> {
        return this.#restClient.call<string>('runtime.unytAgentKey')
    }

    async unytHotAgentPubkey(): Promise<string> {
        return this.#restClient.call<string>('runtime.unytHotAgentPubkey')
    }

    async unytWalletBalance(): Promise<string> {
        return this.#restClient.call<string>('runtime.unytWalletBalance')
    }

    async unytWalletHistory(page?: number, perPage?: number): Promise<string> {
        return this.#restClient.call<string>('runtime.unytWalletHistory', { page, perPage })
    }

    async unytVersionInfo(): Promise<string> {
        return this.#restClient.call<string>('runtime.unytVersionInfo')
    }

    async unytSetMembraneProof(proof: string): Promise<{ success: boolean; message: string }> {
        return this.#restClient.call<{ success: boolean; message: string }>('runtime.unytSetMembraneProof', { proof })
    }

    async unytReinstallDna(): Promise<{ success: boolean; message: string }> {
        return this.#restClient.call<{ success: boolean; message: string }>('runtime.unytReinstallDna')
    }

    async unytSendHot(recipient: string, amount: string): Promise<{ success: boolean; message: string }> {
        return this.#restClient.call<{ success: boolean; message: string }>('runtime.unytSendHot', { recipient, amount })
    }

    async setUserCredits(email: string, amount: number): Promise<boolean> {
        return this.#restClient.call<boolean>('user.credits', { email, amount })
    }

    async setUserFreeAccess(email: string, enabled: boolean): Promise<boolean> {
        return this.#restClient.call<boolean>('user.freeAccess', { email, enabled })
    }

    async setHostRates(ratesJson: string): Promise<boolean> {
        return this.#restClient.call<boolean>('runtime.setHostRates', { ratesJson })
    }

    async getHostRates(): Promise<{ description: string; priceInHOT: number }[]> {
        const result = await this.#restClient.call<string>('runtime.getHostRates')
        try {
            return JSON.parse(result)
        } catch {
            return []
        }
    }

    addNotificationTriggeredCallback(cb: NotificationTriggeredCallback) {
        this.#notificationTriggeredCallbacks.push(cb)
    }

    subscribeNotificationTriggered() {
        const unsub = this.#restClient.subscribe('/api/v1/events', (data) => {
            if (data.type === 'notification-triggered') {
                this.#notificationTriggeredCallbacks.forEach(cb => cb(data.notification as TriggeredNotification))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addMessageCallback(cb: MessageCallback) {
        this.#messageReceivedCallbacks.push(cb)
    }

    subscribeMessageReceived() {
        const unsub = this.#restClient.subscribe('/api/v1/events', (data) => {
            if (data.type === 'message-received') {
                this.#messageReceivedCallbacks.forEach(cb => cb(data.message as PerspectiveExpression))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addExceptionCallback(cb: ExceptionCallback) {
        this.#exceptionOccurredCallbacks.push(cb)
    }

    subscribeExceptionOccurred() {
        const unsub = this.#restClient.subscribe('/api/v1/events', (data) => {
            if (data.type === 'exception-occurred' && data.exception) {
                const exception = data.exception as ExceptionInfo
                const normalizedException = {
                    ...exception,
                    type: normalizeExceptionType(exception.type),
                }
                this.#exceptionOccurredCallbacks.forEach(cb => cb(normalizedException))
            }
        })
        this.#unsubscribers.push(unsub)
    }
}
