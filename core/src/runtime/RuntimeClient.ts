import { ApolloClient, gql } from "@apollo/client/core"
import { Perspective, PerspectiveExpression } from "../perspectives/Perspective"
import unwrapApolloResult from "../unwrapApolloResult"
import { RuntimeInfo, ExceptionInfo, SentMessage, NotificationInput, Notification, TriggeredNotification } from "./RuntimeResolver"

const PERSPECTIVE_EXPRESSION_FIELDS = `
author
timestamp
data { 
    links {
        author
        timestamp
        data { source, predicate, target }
        proof { valid, invalid, signature, key }
    }  
}
proof { valid, invalid, signature, key }
`

const NOTIFICATION_DEFINITION_FIELDS = `
description
appName
appUrl
appIconPath
trigger
perspectiveIds
webhookUrl
webhookAuth
`

const NOTIFICATION_FIELDS = `
id
granted
${NOTIFICATION_DEFINITION_FIELDS}
`

const TRIGGERED_NOTIFICATION_FIELDS = `
notification { ${NOTIFICATION_FIELDS} }
perspectiveId
triggerMatch
`

export type MessageCallback = (message: PerspectiveExpression) => null
export type ExceptionCallback = (info: ExceptionInfo) => null
export type NotificationTriggeredCallback = (notification: TriggeredNotification) => null
export type NotificationRequestedCallback = (notification: Notification) => null

export class RuntimeClient {
    #apolloClient: ApolloClient<any>
    #messageReceivedCallbacks: MessageCallback[]
    #exceptionOccurredCallbacks: ExceptionCallback[]
    #notificationTriggeredCallbacks: NotificationTriggeredCallback[]
    #notificationRequestedCallbacks: NotificationRequestedCallback[]

    constructor(client: ApolloClient<any>, subscribe: boolean = true) {
        this.#apolloClient = client
        this.#messageReceivedCallbacks = []
        this.#exceptionOccurredCallbacks = []
        this.#notificationTriggeredCallbacks = []

        if(subscribe) {
            this.subscribeMessageReceived()
            this.subscribeExceptionOccurred()
            this.subscribeNotificationTriggered()
        }
    }

    async info(): Promise<RuntimeInfo> {
        const { runtimeInfo } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query runtimeInfo {
                runtimeInfo {
                    ad4mExecutorVersion,
                    isInitialized,
                    isUnlocked
                }
            }`,
        }));
        return runtimeInfo
    }

    async quit(): Promise<Boolean> {
        const result = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeQuit { runtimeQuit }`
        }))

        return result.runtimeQuit
    }

    async openLink(url: string): Promise<Boolean> {
        const { runtimeOpenLink } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeOpenLink($url: String!) {
                runtimeOpenLink(url: $url)
            }`,
            variables: { url }
        }))
        return runtimeOpenLink
    }

    async addTrustedAgents(agents: string[]): Promise<string[]> {
        const { addTrustedAgents } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation addTrustedAgents($agents: [String!]!) {
                addTrustedAgents(agents: $agents)
            }`,
            variables: { agents }
        }))
        return addTrustedAgents 
    }

    async deleteTrustedAgents(agents: string[]): Promise<string[]> {
        const { deleteTrustedAgents } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation deleteTrustedAgents($agents: [String!]!) {
                deleteTrustedAgents(agents: $agents)
            }`,
            variables: { agents }
        }))
        return deleteTrustedAgents 
    }

    async getTrustedAgents(): Promise<string[]> {
        const { getTrustedAgents } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query getTrustedAgents {
                getTrustedAgents
            }`,
        }))
        return getTrustedAgents
    }

    async addKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        const { runtimeAddKnownLinkLanguageTemplates } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeAddKnownLinkLanguageTemplates($addresses: [String!]!) {
                runtimeAddKnownLinkLanguageTemplates(addresses: $addresses)
            }`,
            variables: { addresses }
        }))
        return runtimeAddKnownLinkLanguageTemplates 
    }

    async removeKnownLinkLanguageTemplates(addresses: string[]): Promise<string[]> {
        const { runtimeRemoveKnownLinkLanguageTemplates } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeRemoveKnownLinkLanguageTemplates($addresses: [String!]!) {
                runtimeRemoveKnownLinkLanguageTemplates(addresses: $addresses)
            }`,
            variables: { addresses }
        }))
        return runtimeRemoveKnownLinkLanguageTemplates 
    }

    async knownLinkLanguageTemplates(): Promise<string[]> {
        const { runtimeKnownLinkLanguageTemplates } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query runtimeKnownLinkLanguageTemplates {
                runtimeKnownLinkLanguageTemplates
            }`,
        }))
        return runtimeKnownLinkLanguageTemplates
    }

    async addFriends(dids: string[]): Promise<string[]> {
        const { runtimeAddFriends } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeAddFriends($dids: [String!]!) {
                runtimeAddFriends(dids: $dids)
            }`,
            variables: { dids }
        }))
        return runtimeAddFriends 
    }

    async removeFriends(dids: string[]): Promise<string[]> {
        const { runtimeRemoveFriends } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeRemoveFriends($dids: [String!]!) {
                runtimeRemoveFriends(dids: $dids)
            }`,
            variables: { dids }
        }))
        return runtimeRemoveFriends 
    }

    async friends(): Promise<string[]> {
        const { runtimeFriends } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query runtimeFriends {
                runtimeFriends
            }`,
        }))
        return runtimeFriends
    }

    async hcAgentInfos(): Promise<String> {
        const { runtimeHcAgentInfos } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query runtimeHcAgentInfos {
                runtimeHcAgentInfos
            }`,
        }))
        return runtimeHcAgentInfos
    }

    async hcAddAgentInfos(agentInfos: String): Promise<void> {
        const { runtimeHcAddAgentInfos } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeHcAddAgentInfos($agentInfos: String!) {
                runtimeHcAddAgentInfos(agentInfos: $agentInfos)
            }`,
            variables: { agentInfos }
        }))
        return runtimeHcAddAgentInfos
    }

    async verifyStringSignedByDid(did: string, didSigningKeyId: string, data: string, signedData: string): Promise<boolean> {
        const { runtimeVerifyStringSignedByDid } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`query runtimeVerifyStringSignedByDid($did: String!, $didSigningKeyId: String!, $data: String!, $signedData: String!) {
                runtimeVerifyStringSignedByDid(did: $did, didSigningKeyId: $didSigningKeyId, data: $data, signedData: $signedData)
            }`,
            variables: { did, didSigningKeyId, data, signedData }
        }))
        return runtimeVerifyStringSignedByDid
    }
    
    async setStatus(perspective: Perspective): Promise<boolean> {
        const { runtimeSetStatus } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeSetStatus($status: PerspectiveInput!) {
                runtimeSetStatus(status: $status)
            }`,
            variables: { status: perspective }
        }))
        return runtimeSetStatus
    }

    async friendStatus(did: string): Promise<PerspectiveExpression> {
        const { runtimeFriendStatus } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query runtimeFriendStatus($did: String!) {
                runtimeFriendStatus(did: $did) { ${PERSPECTIVE_EXPRESSION_FIELDS} }
            }`,
            variables: { did }
        }))
        return runtimeFriendStatus
    }

    async friendSendMessage(did: string, message: Perspective): Promise<boolean> {
        const { runtimeFriendSendMessage } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeFriendSendMessage($did: String!, $message: PerspectiveInput!) {
                runtimeFriendSendMessage(did: $did, message: $message)
            }`,
            variables: { did,  message }
        }))
        return runtimeFriendSendMessage
    }

    async messageInbox(filter?: string): Promise<PerspectiveExpression[]> {
        const { runtimeMessageInbox } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query runtimeMessageInbox($filter: String) {
                runtimeMessageInbox(filter: $filter) { ${PERSPECTIVE_EXPRESSION_FIELDS} }
            }`,
            variables: { filter }
        }))
        return runtimeMessageInbox
    }

    async messageOutbox(filter?: string): Promise<SentMessage[]> {
        const { runtimeMessageOutbox } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query runtimeMessageOutbox($filter: String) {
                runtimeMessageOutbox(filter: $filter) { 
                    recipient,
                    message {
                        ${PERSPECTIVE_EXPRESSION_FIELDS} 
                    }
                }
            }`,
            variables: { filter }
        }))
        return runtimeMessageOutbox
    }

    async requestInstallNotification(notification: NotificationInput) {
        const { runtimeRequestInstallNotification } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeRequestInstallNotification($notification: NotificationInput!) {
                runtimeRequestInstallNotification(notification: $notification)
            }`,
            variables: { notification }
        }))
        return runtimeRequestInstallNotification
    }

    async grantNotification(id: string): Promise<boolean> {
        const { runtimeGrantNotification } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeGrantNotification($id: String!) {
                runtimeGrantNotification(id: $id)
            }`,
            variables: { id }
        }))
        return runtimeGrantNotification
    }

    async exportDb(filePath: string): Promise<boolean> {
        const { runtimeExportDb } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeExportDb($filePath: String!) {
                runtimeExportDb(filePath: $filePath)
            }`,
            variables: { filePath }
        }))
        return runtimeExportDb
    }

    async importDb(filePath: string): Promise<boolean> {
        const { runtimeImportDb } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeImportDb($filePath: String!) {
                runtimeImportDb(filePath: $filePath)
            }`,
            variables: { filePath }
        }))
        return runtimeImportDb
    }

    async notifications(): Promise<Notification[]> {
        const { runtimeNotifications } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query runtimeNotifications {
                runtimeNotifications { ${NOTIFICATION_FIELDS} }
            }`,
        }))
        return runtimeNotifications
    }

    async updateNotification(id: string, notification: NotificationInput): Promise<boolean> {
        const { runtimeUpdateNotification } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeUpdateNotification($id: String!, $notification: NotificationInput!) {
                runtimeUpdateNotification(id: $id, notification: $notification)
            }`,
            variables: { id, notification }
        }))
        return runtimeUpdateNotification
    }

    async removeNotification(id: string): Promise<boolean> {
        const { runtimeRemoveNotification } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeRemoveNotification($id: String!) {
                runtimeRemoveNotification(id: $id)
            }`,
            variables: { id }
        }))
        return runtimeRemoveNotification
    }

    async exportPerspective(uuid: string, filePath: string): Promise<boolean> {
        const { runtimeExportPerspective } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeExportPerspective($perspectiveUuid: String!, $filePath: String!) {
                runtimeExportPerspective(perspectiveUuid: $perspectiveUuid, filePath: $filePath)
            }`,
            variables: { perspectiveUuid: uuid, filePath }
        }))
        return runtimeExportPerspective
    }

    async importPerspective(filePath: string): Promise<boolean> {
        const { runtimeImportPerspective } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation runtimeImportPerspective($filePath: String!) {
                runtimeImportPerspective(filePath: $filePath)
            }`,
            variables: { filePath }
        }))
        return runtimeImportPerspective
    }

    addNotificationTriggeredCallback(cb: NotificationTriggeredCallback) {
        this.#notificationTriggeredCallbacks.push(cb)
    }

    subscribeNotificationTriggered() {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                runtimeNotificationTriggered { ${TRIGGERED_NOTIFICATION_FIELDS} }
            }   
        `}).subscribe({
            next: result => {
                this.#notificationTriggeredCallbacks.forEach(cb => {
                    cb(result.data.runtimeNotificationTriggered)
                })
            },
            error: (e) => console.error(e)
        })
    }

    addMessageCallback(cb: MessageCallback) {
        this.#messageReceivedCallbacks.push(cb)
    }

    subscribeMessageReceived() {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                runtimeMessageReceived { ${PERSPECTIVE_EXPRESSION_FIELDS} }
            }   
        `}).subscribe({
            next: result => {
                this.#messageReceivedCallbacks.forEach(cb => {
                    cb(result.data.runtimeMessageReceived)
                })
            },
            error: (e) => console.error(e)
        })
    }

    addExceptionCallback(cb: ExceptionCallback) {
        this.#exceptionOccurredCallbacks.push(cb)
    }

    subscribeExceptionOccurred() {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                exceptionOccurred {
                    title
                    message
                    type
                    addon
                }
            }`
        }).subscribe({
            next: result => {
                this.#exceptionOccurredCallbacks.forEach(cb => {
                    cb(result.data.exceptionOccurred)
                })
            },
            error: (e) => console.error(e)
        })
    }
}