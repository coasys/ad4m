import {ApiClient, CallOptions } from '../apiClient'
import { Perspective, PerspectiveExpression } from "../perspectives/Perspective"
import { RuntimeInfo, SentMessage, NotificationInput, Notification, ImportResult, UserStatistics } from "./RuntimeTypes"
import type { HostRate, UnytVersionInfo } from "../generated/api"
import { ServiceClient } from '../services/ServiceClient'
import { BillingLedger_1_0_0 } from '../generated/services/billing.ledger'
import type { BillingLedger_1_0_0_Events, BillingLedger_1_0_0_Methods } from '../generated/services/billing.ledger'
import { HolochainConductor_1_0_0 } from '../generated/services/holochain.conductor'
import type { HolochainConductor_1_0_0_Events, HolochainConductor_1_0_0_Methods } from '../generated/services/holochain.conductor'
import { UnytWallet_1_0_0 } from '../generated/services/unyt.wallet'
import type { UnytWallet_1_0_0_Events, UnytWallet_1_0_0_Methods } from '../generated/services/unyt.wallet'

export class RuntimeClient {
    #apiClient: ApiClient
    #ledger: ServiceClient<BillingLedger_1_0_0_Methods, BillingLedger_1_0_0_Events>
    #holochain: ServiceClient<HolochainConductor_1_0_0_Methods, HolochainConductor_1_0_0_Events>
    #unyt: ServiceClient<UnytWallet_1_0_0_Methods, UnytWallet_1_0_0_Events>

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
        this.#ledger = new ServiceClient(this.#apiClient, BillingLedger_1_0_0)
        this.#holochain = new ServiceClient(this.#apiClient, HolochainConductor_1_0_0)
        this.#unyt = new ServiceClient(this.#apiClient, UnytWallet_1_0_0)
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
        return this.#holochain.call('agentInfos', {})
    }

    async getNetworkMetrics(): Promise<string> {
        return this.#holochain.call('networkMetrics', {})
    }

    async restartHolochain(options?: CallOptions): Promise<boolean> {
        return this.#holochain.call('restart', {}, options)
    }

    async hcAddAgentInfos(agentInfos: string[]): Promise<boolean> {
        return this.#holochain.call('addAgentInfos', { agentInfos })
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
        return this.#ledger.call('freeHostingEnabled', {})
    }

    async setFreeHostingEnabled(enabled: boolean): Promise<boolean> {
        return this.#ledger.call('setFreeHostingEnabled', { enabled })
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
        return this.#unyt.call('agentKey', {})
    }

    async unytHotAgentPubkey(): Promise<string> {
        return this.#unyt.call('hotAgentPubkey', {})
    }

    /** The wallet's ledger, as JSON text. */
    async unytWalletBalance(): Promise<string> {
        return JSON.stringify(await this.#unyt.call('balance', {}))
    }

    /** One page of wallet transactions, as JSON text. */
    async unytWalletHistory(page?: number, perPage?: number): Promise<string> {
        return JSON.stringify(await this.#unyt.call('history', { page, perPage }))
    }

    /** Installed and bundled DNA versions, and why the last install failed (`installError`). */
    async unytVersionInfo(): Promise<UnytVersionInfo> {
        return this.#unyt.call('versionInfo', {}) as Promise<UnytVersionInfo>
    }

    /** Stores the membrane proof (base64); the executor then installs the Unyt DNA in the
     *  background. Poll {@link unytVersionInfo} for the outcome. */
    async setUnytMembraneProof(proof: string): Promise<boolean> {
        return this.#unyt.call('setMembraneProof', { proof })
    }

    async unytReinstallDna(): Promise<{ success: boolean; message: string }> {
        return this.#unyt.call('reinstallDna', {})
    }

    async unytSendHot(recipient: string, amount: string): Promise<{ success: boolean; message: string }> {
        return this.#unyt.call('sendHot', { recipient, amount })
    }

    async setUserCredits(email: string, amount: number): Promise<boolean> {
        return this.#apiClient.call('user.credits', { email, amount })
    }

    async setUserFreeAccess(email: string, enabled: boolean): Promise<boolean> {
        return this.#apiClient.call('user.freeAccess', { email, enabled })
    }

    async setHostRates(rates: HostRate[]): Promise<boolean> {
        return this.#ledger.call('setRates', { rates })
    }

    async hostRates(): Promise<HostRate[]> {
        return this.#ledger.call('rates', {})
    }

}
