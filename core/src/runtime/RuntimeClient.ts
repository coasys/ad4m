import {ApiClient, CallOptions } from '../apiClient'
import { Perspective, PerspectiveExpression } from "../perspectives/Perspective"
import { RuntimeInfo, SentMessage, NotificationInput, Notification, ImportResult, UserStatistics } from "./RuntimeTypes"
import type { HostRate, UnytVersionInfo } from "../generated/api"

export class RuntimeClient {
    #apiClient: ApiClient

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
    }

    async info(): Promise<RuntimeInfo> {
        return this.#apiClient.call('runtime.info', {})
    }

    async tlsDomain(): Promise<string | null> {
        return this.#apiClient.call('runtime.tlsDomain', {})
    }

    async quit(): Promise<Boolean> {
        return this.#apiClient.call('runtime.quit', {})
    }

    async openLink(url: string): Promise<Boolean> {
        return this.#apiClient.call('runtime.openLink', { url })
    }

    async addTrustedAgents(agents: string[]): Promise<string[]> {
        return this.#apiClient.call('agent.addTrustedAgents', { agents })
    }

    async deleteTrustedAgents(agents: string[]): Promise<string[]> {
        return this.#apiClient.call('agent.deleteTrustedAgents', { agents })
    }

    async getTrustedAgents(): Promise<string[]> {
        return this.#apiClient.call('agent.getTrustedAgents', {})
    }

    async addKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        return this.#apiClient.call('runtime.addLinkLanguageTemplates', { addresses })
    }

    async removeKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        return this.#apiClient.call('runtime.removeLinkLanguageTemplates', { addresses })
    }

    async knownLinkLanguageTemplates(): Promise<string[]> {
        return this.#apiClient.call('runtime.linkLanguageTemplates', {})
    }

    async addFriends(dids: string[]): Promise<string[]> {
        return this.#apiClient.call('runtime.addFriends', { dids })
    }

    async removeFriends(dids: string[]): Promise<string[]> {
        return this.#apiClient.call('runtime.removeFriends', { dids })
    }

    async friends(): Promise<string[]> {
        return this.#apiClient.call('runtime.friends', {})
    }

    async hcAgentInfos(): Promise<string[]> {
        return this.#apiClient.call('runtime.hcAgentInfos', {})
    }

    async getNetworkMetrics(): Promise<string> {
        return this.#apiClient.call('runtime.networkMetrics', {})
    }

    async restartHolochain(options?: CallOptions): Promise<boolean> {
        return this.#apiClient.call('runtime.restartHolochain', {}, options)
    }

    async hcAddAgentInfos(agentInfos: string[]): Promise<boolean> {
        return this.#apiClient.call('runtime.addHcAgentInfos', { agentInfos })
    }

    async verifyStringSignedByDid(did: string, didSigningKeyId: string, data: string, signedData: string): Promise<boolean> {
        // The executor resolves the key from the DID document; `didSigningKeyId` goes unused.
        return this.#apiClient.call('runtime.verifySignature', { did, data, signedData })
    }

    async setStatus(perspective: Perspective): Promise<boolean> {
        return this.#apiClient.call('runtime.setStatus', { status: Perspective.toWire(perspective) })
    }

    async friendStatus(did: string): Promise<PerspectiveExpression> {
        const status = await this.#apiClient.call('runtime.friendStatus', { did })
        return status ? PerspectiveExpression.fromWire(status) : null
    }

    async friendSendMessage(did: string, message: Perspective): Promise<boolean> {
        return this.#apiClient.call('runtime.sendFriendMessage', { did, message: Perspective.toWire(message) })
    }

    async messageInbox(): Promise<PerspectiveExpression[]> {
        return (await this.#apiClient.call('runtime.inbox', {})).map(PerspectiveExpression.fromWire)
    }

    async messageOutbox(): Promise<SentMessage[]> {
        const sent = await this.#apiClient.call('runtime.outbox', {})
        return sent.map(({ recipient, message }) => ({ recipient, message: PerspectiveExpression.fromWire(message) }))
    }

    async requestInstallNotification(notification: NotificationInput) {
        return this.#apiClient.call('runtime.createNotification', { ...notification })
    }

    async grantNotification(id: string): Promise<boolean> {
        return this.#apiClient.call('runtime.grantNotification', { id, granted: true })
    }

    async exportDb(filePath: string): Promise<boolean> {
        return this.#apiClient.call('runtime.exportData', { type: "db", filePath })
    }

    async importDb(filePath: string): Promise<ImportResult> {
        const result = await this.#apiClient.call('runtime.importData', { type: "db", filePath })
        if ('success' in result) throw new Error('runtime.importData answered a perspective import for type "db"')
        return result
    }

    async notifications(): Promise<Notification[]> {
        return this.#apiClient.call('runtime.notifications', {})
    }

    async updateNotification(id: string, notification: NotificationInput): Promise<boolean> {
        return this.#apiClient.call('runtime.updateNotification', { ...notification, id })
    }

    async removeNotification(id: string): Promise<boolean> {
        return this.#apiClient.call('runtime.deleteNotification', { id })
    }

    async exportPerspective(uuid: string, filePath: string): Promise<boolean> {
        return this.#apiClient.call('runtime.exportData', { type: "perspective", perspectiveUuid: uuid, filePath })
    }

    async importPerspective(filePath: string): Promise<boolean> {
        const result = await this.#apiClient.call('runtime.importData', { type: "perspective", filePath })
        return 'success' in result && result.success
    }

    async multiUserEnabled(): Promise<boolean> {
        return this.#apiClient.call('user.multiUserEnabled', {})
    }

    async setMultiUserEnabled(enabled: boolean): Promise<boolean> {
        return this.#apiClient.call('user.setMultiUserEnabled', { enabled })
    }

    async freeHostingEnabled(): Promise<boolean> {
        return this.#apiClient.call('runtime.freeHostingEnabled', {})
    }

    async setFreeHostingEnabled(enabled: boolean): Promise<boolean> {
        return this.#apiClient.call('runtime.setFreeHostingEnabled', { enabled })
    }

    async listUsers(): Promise<UserStatistics[]> {
        return this.#apiClient.call('user.list', {})
    }

    async userWalletAddress(email: string): Promise<string | null> {
        return this.#apiClient.call('user.wallet', { email })
    }

    async emailTestModeEnable(): Promise<boolean> {
        return (await this.#apiClient.call('user.emailTest', { action: 'enable' })) === true
    }

    async emailTestModeDisable(): Promise<boolean> {
        return (await this.#apiClient.call('user.emailTest', { action: 'disable' })) === true
    }

    async emailTestGetCode(email: string): Promise<string | null> {
        const code = await this.#apiClient.call('user.emailTest', { action: 'get-code', email })
        return typeof code === 'string' ? code : null
    }

    async emailTestClearCodes(): Promise<boolean> {
        return (await this.#apiClient.call('user.emailTest', { action: 'clear-codes' })) === true
    }

    async emailTestSetExpiry(email: string, verificationType: string, expiresAt: number): Promise<boolean> {
        return (await this.#apiClient.call('user.emailTest', { action: 'set-expiry', email, verificationType, expiresAt })) === true
    }

    // ---- Unyt / mHOT methods ----

    async unytAgentKey(): Promise<string> {
        return this.#apiClient.call('runtime.unytAgentKey', {})
    }

    async unytHotAgentPubkey(): Promise<string> {
        return this.#apiClient.call('runtime.unytHotAgentPubkey', {})
    }

    async unytWalletBalance(): Promise<string> {
        return this.#apiClient.call('runtime.unytWalletBalance', {})
    }

    async unytWalletHistory(page?: number, perPage?: number): Promise<string> {
        return this.#apiClient.call('runtime.unytWalletHistory', { page, perPage })
    }

    /** Installed and bundled DNA versions, and why the last install failed (`installError`). */
    async unytVersionInfo(): Promise<UnytVersionInfo> {
        return this.#apiClient.call('runtime.unytVersionInfo', {})
    }

    /** Stores the membrane proof (base64); the executor then installs the Unyt DNA in the
     *  background. Poll {@link unytVersionInfo} for the outcome. */
    async setUnytMembraneProof(proof: string): Promise<boolean> {
        return this.#apiClient.call('runtime.setUnytMembraneProof', { proof })
    }

    async unytReinstallDna(): Promise<{ success: boolean; message: string }> {
        return this.#apiClient.call('runtime.unytReinstallDna', {})
    }

    async unytSendHot(recipient: string, amount: string): Promise<{ success: boolean; message: string }> {
        return this.#apiClient.call('runtime.unytSendHot', { recipient, amount })
    }

    async setUserCredits(email: string, amount: number): Promise<boolean> {
        return this.#apiClient.call('user.credits', { email, amount })
    }

    async setUserFreeAccess(email: string, enabled: boolean): Promise<boolean> {
        return this.#apiClient.call('user.freeAccess', { email, enabled })
    }

    async setHostRates(rates: HostRate[]): Promise<boolean> {
        return this.#apiClient.call('runtime.setHostRates', { rates })
    }

    async hostRates(): Promise<HostRate[]> {
        return this.#apiClient.call('runtime.hostRates', {})
    }

}
