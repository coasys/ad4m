import { ApiClient } from "../apiClient"
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
} from "../generated/api"

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
    #apiClient: ApiClient
    #messageReceivedCallbacks: MessageCallback[]
    #exceptionOccurredCallbacks: ExceptionCallback[]
    #notificationTriggeredCallbacks: NotificationTriggeredCallback[]
    #notificationRequestedCallbacks: NotificationRequestedCallback[]
    #unsubscribers: (() => void)[]

    constructor(baseUrl: string, token?: string, subscribe: boolean = true, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
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
        return this.#apiClient.call<RuntimeInfo>('runtime.info')
    }

    async tlsDomain(): Promise<string | null> {
        return this.#apiClient.call<string | null>('runtime.tlsDomain')
    }

    async quit(): Promise<Boolean> {
        return this.#apiClient.call<Boolean>('runtime.quit')
    }

    async openLink(url: string): Promise<Boolean> {
        return this.#apiClient.call<Boolean>('runtime.openLink', { url })
    }

    async addTrustedAgents(agents: string[]): Promise<string[]> {
        return this.#apiClient.call<string[]>('agent.addTrustedAgents', { agents })
    }

    async deleteTrustedAgents(agents: string[]): Promise<string[]> {
        return this.#apiClient.call<string[]>('agent.deleteTrustedAgents', { agents })
    }

    async getTrustedAgents(): Promise<string[]> {
        return this.#apiClient.call<string[]>('agent.getTrustedAgents')
    }

    async addKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        return this.#apiClient.call<string[]>('runtime.addLinkLanguageTemplates', { addresses })
    }

    async removeKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        return this.#apiClient.call<string[]>('runtime.removeLinkLanguageTemplates', { addresses })
    }

    async knownLinkLanguageTemplates(): Promise<string[]> {
        return this.#apiClient.call<string[]>('runtime.linkLanguageTemplates')
    }

    async addFriends(dids: string[]): Promise<string[]> {
        return this.#apiClient.call<string[]>('runtime.addFriends', { dids })
    }

    async removeFriends(dids: string[]): Promise<string[]> {
        return this.#apiClient.call<string[]>('runtime.removeFriends', { dids })
    }

    async friends(): Promise<string[]> {
        return this.#apiClient.call<string[]>('runtime.friends')
    }

    async hcAgentInfos(): Promise<string[]> {
        return this.#apiClient.call<string[]>('runtime.hcAgentInfos')
    }

    async getNetworkMetrics(): Promise<string> {
        return this.#apiClient.call<string>('runtime.networkMetrics')
    }

    async restartHolochain(): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.restartHolochain')
    }

    async hcAddAgentInfos(agentInfos: string[]): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.addHcAgentInfos', { agentInfos })
    }

    async verifyStringSignedByDid(did: string, didSigningKeyId: string, data: string, signedData: string): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.verifySignature', { did, didSigningKeyId, data, signedData })
    }

    async setStatus(perspective: Perspective): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.setStatus', { status: perspective })
    }

    async friendStatus(did: string): Promise<PerspectiveExpression> {
        return this.#apiClient.call<PerspectiveExpression>('runtime.friendStatus', { did })
    }

    async friendSendMessage(did: string, message: Perspective): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.sendFriendMessage', { did, message })
    }

    async messageInbox(filter?: string): Promise<PerspectiveExpression[]> {
        return this.#apiClient.call<PerspectiveExpression[]>('runtime.inbox', { filter })
    }

    async messageOutbox(filter?: string): Promise<SentMessage[]> {
        return this.#apiClient.call<SentMessage[]>('runtime.outbox', { filter })
    }

    async requestInstallNotification(notification: NotificationInput) {
        return this.#apiClient.call('runtime.createNotification', { ...notification })
    }

    async grantNotification(id: string): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.grantNotification', { id, granted: true })
    }

    async exportDb(filePath: string): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.exportData', { type: "db", filePath })
    }

    async importDb(filePath: string): Promise<ImportResult> {
        return this.#apiClient.call<ImportResult>('runtime.importData', { type: "db", filePath })
    }

    async notifications(): Promise<Notification[]> {
        return this.#apiClient.call<Notification[]>('runtime.notifications')
    }

    async updateNotification(id: string, notification: NotificationInput): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.updateNotification', { ...notification, id })
    }

    async removeNotification(id: string): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.deleteNotification', { id })
    }

    async exportPerspective(uuid: string, filePath: string): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.exportData', { type: "perspective", perspectiveUuid: uuid, filePath })
    }

    async importPerspective(filePath: string): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.importData', { type: "perspective", filePath })
    }

    async multiUserEnabled(): Promise<boolean> {
        return this.#apiClient.call<boolean>('user.multiUserEnabled')
    }

    async setMultiUserEnabled(enabled: boolean): Promise<boolean> {
        return this.#apiClient.call<boolean>('user.setMultiUserEnabled', { enabled })
    }

    async freeHostingEnabled(): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.freeHostingEnabled')
    }

    async setFreeHostingEnabled(enabled: boolean): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.setFreeHostingEnabled', { enabled })
    }

    async listUsers(): Promise<UserStatistics[]> {
        return this.#apiClient.call<UserStatistics[]>('user.list')
    }

    async userWalletAddress(email: string): Promise<string | null> {
        return this.#apiClient.call<string | null>('user.wallet', { email })
    }

    async emailTestModeEnable(): Promise<boolean> {
        return this.#apiClient.call<boolean>('user.emailTest', { action: 'enable' })
    }

    async emailTestModeDisable(): Promise<boolean> {
        return this.#apiClient.call<boolean>('user.emailTest', { action: 'disable' })
    }

    async emailTestGetCode(email: string): Promise<string | null> {
        return this.#apiClient.call<string | null>('user.emailTest', { action: 'get-code', email })
    }

    async emailTestClearCodes(): Promise<boolean> {
        return this.#apiClient.call<boolean>('user.emailTest', { action: 'clear-codes' })
    }

    async emailTestSetExpiry(email: string, verificationType: string, expiresAt: number): Promise<boolean> {
        return this.#apiClient.call<boolean>('user.emailTest', { action: 'set-expiry', email, verificationType, expiresAt })
    }

    // ---- Unyt / mHOT methods ----

    async unytAgentKey(): Promise<string> {
        return this.#apiClient.call<string>('runtime.unytAgentKey')
    }

    async unytHotAgentPubkey(): Promise<string> {
        return this.#apiClient.call<string>('runtime.unytHotAgentPubkey')
    }

    async unytWalletBalance(): Promise<string> {
        return this.#apiClient.call<string>('runtime.unytWalletBalance')
    }

    async unytWalletHistory(page?: number, perPage?: number): Promise<string> {
        return this.#apiClient.call<string>('runtime.unytWalletHistory', { page, perPage })
    }

    async unytVersionInfo(): Promise<string> {
        return this.#apiClient.call<string>('runtime.unytVersionInfo')
    }

    async unytSetMembraneProof(proof: string): Promise<{ success: boolean; message: string }> {
        return this.#apiClient.call<{ success: boolean; message: string }>('runtime.unytSetMembraneProof', { proof })
    }

    async unytReinstallDna(): Promise<{ success: boolean; message: string }> {
        return this.#apiClient.call<{ success: boolean; message: string }>('runtime.unytReinstallDna')
    }

    async unytSendHot(recipient: string, amount: string): Promise<{ success: boolean; message: string }> {
        return this.#apiClient.call<{ success: boolean; message: string }>('runtime.unytSendHot', { recipient, amount })
    }

    async setUserCredits(email: string, amount: number): Promise<boolean> {
        return this.#apiClient.call<boolean>('user.credits', { email, amount })
    }

    async setUserFreeAccess(email: string, enabled: boolean): Promise<boolean> {
        return this.#apiClient.call<boolean>('user.freeAccess', { email, enabled })
    }

    async setHostRates(ratesJson: string): Promise<boolean> {
        return this.#apiClient.call<boolean>('runtime.setHostRates', { ratesJson })
    }

    async getHostRates(): Promise<{ description: string; priceInHOT: number }[]> {
        const result = await this.#apiClient.call<string>('runtime.getHostRates')
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
        const unsub = this.#apiClient.subscribe((data) => {
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
        const unsub = this.#apiClient.subscribe((data) => {
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
        const unsub = this.#apiClient.subscribe((data) => {
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
