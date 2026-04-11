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

export class RuntimeClient {
    #restClient: RestClient
    #messageReceivedCallbacks: MessageCallback[]
    #exceptionOccurredCallbacks: ExceptionCallback[]
    #notificationTriggeredCallbacks: NotificationTriggeredCallback[]
    #notificationRequestedCallbacks: NotificationRequestedCallback[]
    #unsubscribers: (() => void)[]

    constructor(baseUrl: string, token?: string, subscribe: boolean = true) {
        this.#restClient = new RestClient(baseUrl, token)
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
        return this.#restClient.get<RuntimeInfo>('/api/v1/runtime/info')
    }

    async tlsDomain(): Promise<string | null> {
        return this.#restClient.get<string | null>('/api/v1/runtime/tls-domain')
    }

    async quit(): Promise<Boolean> {
        return this.#restClient.post<Boolean>('/api/v1/runtime/quit')
    }

    async openLink(url: string): Promise<Boolean> {
        return this.#restClient.post<Boolean>('/api/v1/runtime/open-link', { url })
    }

    async addTrustedAgents(agents: string[]): Promise<string[]> {
        return this.#restClient.put<string[]>('/api/v1/agent/trusted', { agents })
    }

    async deleteTrustedAgents(agents: string[]): Promise<string[]> {
        return this.#restClient.delete<string[]>('/api/v1/agent/trusted', { agents })
    }

    async getTrustedAgents(): Promise<string[]> {
        return this.#restClient.get<string[]>('/api/v1/agent/trusted')
    }

    async addKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        return this.#restClient.put<string[]>('/api/v1/runtime/link-language-templates', { addresses })
    }

    async removeKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        return this.#restClient.delete<string[]>('/api/v1/runtime/link-language-templates', { addresses })
    }

    async knownLinkLanguageTemplates(): Promise<string[]> {
        return this.#restClient.get<string[]>('/api/v1/runtime/link-language-templates')
    }

    async addFriends(dids: string[]): Promise<string[]> {
        return this.#restClient.put<string[]>('/api/v1/runtime/friends', { dids })
    }

    async removeFriends(dids: string[]): Promise<string[]> {
        return this.#restClient.delete<string[]>('/api/v1/runtime/friends', { dids })
    }

    async friends(): Promise<string[]> {
        return this.#restClient.get<string[]>('/api/v1/runtime/friends')
    }

    async hcAgentInfos(): Promise<string> {
        return this.#restClient.get<string>('/api/v1/runtime/hc/agent-infos')
    }

    async getNetworkMetrics(): Promise<string> {
        return this.#restClient.get<string>('/api/v1/runtime/network-metrics')
    }

    async restartHolochain(): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/runtime/holochain/restart')
    }

    async hcAddAgentInfos(agentInfos: String): Promise<void> {
        return this.#restClient.post<void>('/api/v1/runtime/hc/agent-infos', { agentInfos })
    }

    async verifyStringSignedByDid(did: string, didSigningKeyId: string, data: string, signedData: string): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/runtime/verify-signature', { did, didSigningKeyId, data, signedData })
    }

    async setStatus(perspective: Perspective): Promise<boolean> {
        return this.#restClient.put<boolean>('/api/v1/runtime/status', { status: perspective })
    }

    async friendStatus(did: string): Promise<PerspectiveExpression> {
        return this.#restClient.get<PerspectiveExpression>(`/api/v1/runtime/friends/${encodeURIComponent(did)}`)
    }

    async friendSendMessage(did: string, message: Perspective): Promise<boolean> {
        return this.#restClient.post<boolean>(`/api/v1/runtime/friends/${encodeURIComponent(did)}/message`, { message })
    }

    async messageInbox(filter?: string): Promise<PerspectiveExpression[]> {
        const params = filter ? `?filter=${encodeURIComponent(filter)}` : ''
        return this.#restClient.get<PerspectiveExpression[]>(`/api/v1/runtime/messages/inbox${params}`)
    }

    async messageOutbox(filter?: string): Promise<SentMessage[]> {
        const params = filter ? `?filter=${encodeURIComponent(filter)}` : ''
        return this.#restClient.get<SentMessage[]>(`/api/v1/runtime/messages/outbox${params}`)
    }

    async requestInstallNotification(notification: NotificationInput) {
        return this.#restClient.post('/api/v1/runtime/notifications', { notification })
    }

    async grantNotification(id: string): Promise<boolean> {
        return this.#restClient.patch<boolean>(`/api/v1/runtime/notifications/${encodeURIComponent(id)}`, { granted: true })
    }

    async exportDb(filePath: string): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/runtime/export', { type: "db", filePath })
    }

    async importDb(filePath: string): Promise<ImportResult> {
        return this.#restClient.post<ImportResult>('/api/v1/runtime/import', { type: "db", filePath })
    }

    async notifications(): Promise<Notification[]> {
        return this.#restClient.get<Notification[]>('/api/v1/runtime/notifications')
    }

    async updateNotification(id: string, notification: NotificationInput): Promise<boolean> {
        return this.#restClient.patch<boolean>(`/api/v1/runtime/notifications/${encodeURIComponent(id)}`, { notification })
    }

    async removeNotification(id: string): Promise<boolean> {
        return this.#restClient.delete<boolean>(`/api/v1/runtime/notifications/${encodeURIComponent(id)}`)
    }

    async exportPerspective(uuid: string, filePath: string): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/runtime/export', { type: "perspective", perspectiveUuid: uuid, filePath })
    }

    async importPerspective(filePath: string): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/runtime/import', { type: "perspective", filePath })
    }

    async multiUserEnabled(): Promise<boolean> {
        return this.#restClient.get<boolean>('/api/v1/users/multi-user-enabled')
    }

    async setMultiUserEnabled(enabled: boolean): Promise<boolean> {
        return this.#restClient.put<boolean>('/api/v1/users/multi-user-enabled', { enabled })
    }

    async freeHostingEnabled(): Promise<boolean> {
        return this.#restClient.get<boolean>('/api/v1/runtime/free-hosting-enabled')
    }

    async setFreeHostingEnabled(enabled: boolean): Promise<boolean> {
        return this.#restClient.put<boolean>('/api/v1/runtime/free-hosting-enabled', { enabled })
    }

    async listUsers(): Promise<UserStatistics[]> {
        return this.#restClient.get<UserStatistics[]>('/api/v1/users')
    }

    async userWalletAddress(email: string): Promise<string | null> {
        return this.#restClient.get<string | null>(`/api/v1/users/${encodeURIComponent(email)}/wallet`)
    }

    async emailTestModeEnable(): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/dev/email-test', { action: 'enable' })
    }

    async emailTestModeDisable(): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/dev/email-test', { action: 'disable' })
    }

    async emailTestGetCode(email: string): Promise<string | null> {
        return this.#restClient.post<string | null>('/api/v1/dev/email-test', { action: 'get-code', email })
    }

    async emailTestClearCodes(): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/dev/email-test', { action: 'clear-codes' })
    }

    async emailTestSetExpiry(email: string, verificationType: string, expiresAt: number): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/dev/email-test', { action: 'set-expiry', email, verificationType, expiresAt })
    }

    // ---- Unyt / mHOT methods ----

    async unytAgentKey(): Promise<string> {
        return this.#restClient.get<string>('/api/v1/runtime/unyt/agent-key')
    }

    async unytHotAgentPubkey(): Promise<string> {
        return this.#restClient.get<string>('/api/v1/runtime/unyt/hot-agent-pubkey')
    }

    async unytWalletBalance(): Promise<string> {
        return this.#restClient.get<string>('/api/v1/runtime/unyt/wallet-balance')
    }

    async unytWalletHistory(page?: number, perPage?: number): Promise<string> {
        const params = new URLSearchParams()
        if (page !== undefined) params.set('page', String(page))
        if (perPage !== undefined) params.set('perPage', String(perPage))
        const qs = params.toString()
        return this.#restClient.get<string>(`/api/v1/runtime/unyt/wallet-history${qs ? '?' + qs : ''}`)
    }

    async unytVersionInfo(): Promise<string> {
        return this.#restClient.get<string>('/api/v1/runtime/unyt/version-info')
    }

    async unytSetMembraneProof(proof: string): Promise<{ success: boolean; message: string }> {
        return this.#restClient.post<{ success: boolean; message: string }>('/api/v1/runtime/unyt/membrane-proof', { proof })
    }

    async unytReinstallDna(): Promise<{ success: boolean; message: string }> {
        return this.#restClient.post<{ success: boolean; message: string }>('/api/v1/runtime/unyt/reinstall-dna')
    }

    async unytSendHot(recipient: string, amount: string): Promise<{ success: boolean; message: string }> {
        return this.#restClient.post<{ success: boolean; message: string }>('/api/v1/runtime/unyt/send-hot', { recipient, amount })
    }

    async setUserCredits(email: string, amount: number): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/users/credits', { email, amount })
    }

    async setUserFreeAccess(email: string, enabled: boolean): Promise<boolean> {
        return this.#restClient.post<boolean>('/api/v1/users/free-access', { email, enabled })
    }

    async setHostRates(ratesJson: string): Promise<boolean> {
        return this.#restClient.put<boolean>('/api/v1/runtime/host-rates', { ratesJson })
    }

    async getHostRates(): Promise<{ description: string; priceInHOT: number }[]> {
        const result = await this.#restClient.get<string>('/api/v1/runtime/host-rates')
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
        const unsub = this.#restClient.subscribe('/api/v1/events/runtime', (data) => {
            if (data.type === 'notification-triggered') {
                this.#notificationTriggeredCallbacks.forEach(cb => cb(data.notification))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addMessageCallback(cb: MessageCallback) {
        this.#messageReceivedCallbacks.push(cb)
    }

    subscribeMessageReceived() {
        const unsub = this.#restClient.subscribe('/api/v1/events/runtime', (data) => {
            if (data.type === 'message-received') {
                this.#messageReceivedCallbacks.forEach(cb => cb(data.message))
            }
        })
        this.#unsubscribers.push(unsub)
    }

    addExceptionCallback(cb: ExceptionCallback) {
        this.#exceptionOccurredCallbacks.push(cb)
    }

    subscribeExceptionOccurred() {
        const unsub = this.#restClient.subscribe('/api/v1/events/runtime', (data) => {
            if (data.type === 'exception-occurred') {
                this.#exceptionOccurredCallbacks.forEach(cb => cb(data.exception))
            }
        })
        this.#unsubscribers.push(unsub)
    }
}
