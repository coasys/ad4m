import { ApolloClient, gql } from "@apollo/client/core";
import { PerspectiveInput } from "../perspectives/Perspective";
import unwrapApolloResult from "../unwrapApolloResult";
import { Agent, EntanglementProof, EntanglementProofInput } from "./Agent";
import { AgentStatus } from "./AgentStatus"
import { LinkMutations } from "../links/Links";
import { PerspectiveClient } from "../perspectives/PerspectiveClient";

const AGENT_SUBITEMS = `
    did
    directMessageLanguage
    perspective { 
        links {
            author, timestamp, 
            proof {
                signature, key, valid, invalid
            }
            data {
                source, predicate, target
            }
        }
    }
`

const AGENT_STATUS_FIELDS =`
    isInitialized
    isUnlocked
    did
    didDocument
    error
`

const ENTANGLEMENT_PROOF_FIELDS = `
    did
    didSigningKeyId
    deviceKeyType
    deviceKey
    deviceKeySignedByDid
    didSignedByDeviceKey
`

export interface InitializeArgs {
    did: string,
    didDocument: string,
    keystore: string,
    passphrase: string
}

export type AgentUpdatedCallback = (agent: Agent) => null
export type AgentStatusChangedCallback = (agent: Agent) => null
/**
 * Provides access to all functions regarding the local agent,
 * such as generating, locking, unlocking, importing the DID keystore,
 * as well as updating the publicly shared Agent expression.
 */
export class AgentClient {
    #apolloClient: ApolloClient<any>
    #updatedCallbacks: AgentUpdatedCallback[]
    #agentStatusChangedCallbacks: AgentStatusChangedCallback[]
    
    constructor(client: ApolloClient<any>, subscribe: boolean = true) {
        this.#apolloClient = client
        this.#updatedCallbacks = []
        this.#agentStatusChangedCallbacks = []

        if(subscribe) {
            this.subscribeAgentUpdated()
            this.subscribeAgentStatusChanged()
        }
    }

    /**
     * Returns the Agent expression of the local agent as it is shared
     * publicly via the AgentLanguage.
     * 
     * I.e. this is the users profile.
     */
    async me(): Promise<Agent> {
        const { agent } = unwrapApolloResult(await this.#apolloClient.query({ 
            query: gql`query agent { agent { ${AGENT_SUBITEMS} } }` 
        }))
        let agentObject = new Agent(agent.did, agent.perspective)
        agentObject.directMessageLanguage = agent.directMessageLanguage
        return agentObject
    }

    async status(): Promise<AgentStatus> {
        const { agentStatus } = unwrapApolloResult(await this.#apolloClient.query({ 
            query: gql`query agentStatus {
                agentStatus {
                    ${AGENT_STATUS_FIELDS}
                }
            }` 
        }))
        return new AgentStatus(agentStatus)
    }

    async generate(passphrase: string): Promise<AgentStatus> {
        const { agentGenerate } = unwrapApolloResult(await this.#apolloClient.mutate({ 
            mutation: gql`mutation agentGenerate(
                $passphrase: String!
            ) {
                agentGenerate(passphrase: $passphrase) {
                    ${AGENT_STATUS_FIELDS}
                }
            }`,
            variables: { passphrase} 
        }))
        return new AgentStatus(agentGenerate)
    }

    async import(args: InitializeArgs): Promise<AgentStatus> {
        let { did, didDocument, keystore, passphrase } = args
        const { agentImport } = unwrapApolloResult(await this.#apolloClient.mutate({ 
            mutation: gql`mutation agentImport(
                $did: String!,
                $didDocument: String!,
                $keystore: String!,
                $passphrase: String!
            ) {
                agentImport(did: $did, didDocument: $didDocument, keystore: $keystore, passphrase: $passphrase) {
                    ${AGENT_STATUS_FIELDS}
                }
            }`,
            variables: { did, didDocument, keystore, passphrase} 
        }))
        return new AgentStatus(agentImport)
    }

    async lock(passphrase: string): Promise<AgentStatus> {
        const { agentLock } = unwrapApolloResult(await this.#apolloClient.mutate({ 
            mutation: gql`mutation agentLock($passphrase: String!) {
                agentLock(passphrase: $passphrase) {
                    ${AGENT_STATUS_FIELDS}
                }
            }`,
            variables: { passphrase }
        }))
        return new AgentStatus(agentLock)
    }

    async unlock(passphrase: string): Promise<AgentStatus> {
        const { agentUnlock } = unwrapApolloResult(await this.#apolloClient.mutate({ 
            mutation: gql`mutation agentUnlock($passphrase: String!) {
                agentUnlock(passphrase: $passphrase) {
                    ${AGENT_STATUS_FIELDS}
                }
            }`,
            variables: { passphrase }
        }))
        return new AgentStatus(agentUnlock)
    }


    async byDID(did: string): Promise<Agent> {
        const { agentByDID } = unwrapApolloResult(await this.#apolloClient.query({ 
            query: gql`query agentByDID($did: String!) {
                agentByDID(did: $did) {
                    ${AGENT_SUBITEMS}
                }
            }`,
            variables: { did } 
        }))
        return agentByDID as Agent
    }

    async updatePublicPerspective(perspective: PerspectiveInput): Promise<Agent> {
        const cleanedPerspective = JSON.parse(JSON.stringify(perspective));
        delete cleanedPerspective.__typename;
        cleanedPerspective.links.forEach(link => {
           delete link.__typename;
           delete link.data.__typename;
           delete link.proof.__typename;
        });

        const { agentUpdatePublicPerspective } = unwrapApolloResult(await this.#apolloClient.mutate({ 
            mutation: gql`mutation agentUpdatePublicPerspective($perspective: PerspectiveInput!) {
                agentUpdatePublicPerspective(perspective: $perspective) {
                    ${AGENT_SUBITEMS}
                }
            }`,
            variables: { perspective: cleanedPerspective }
        }))
        const a = agentUpdatePublicPerspective
        const agent = new Agent(a.did, a.perspective)
        agent.directMessageLanguage = a.directMessageLanguage
        return agent
    }

    async mutatePublicPerspective(mutations: LinkMutations): Promise<Agent> {
        const perspectiveClient = new PerspectiveClient(this.#apolloClient);
        const agentClient = new AgentClient(this.#apolloClient);
        
        //Create the proxy perspective and load existing links
        const proxyPerspective = await perspectiveClient.add("Agent Perspective Proxy");
        const agentMe = await agentClient.me();

        if (agentMe.perspective) {
            await proxyPerspective.loadSnapshot(agentMe.perspective);
        }

        //Make the mutations on the proxy perspective
        for (const addition of mutations.additions) {
            await proxyPerspective.add(addition);
        }
        for (const removal of mutations.removals) {
            await proxyPerspective.remove(removal);
        }

        //Get the snapshot of the proxy perspective
        const snapshot = await proxyPerspective.snapshot();
        //Update the users public perspective
        const agent = await this.updatePublicPerspective(snapshot);
        //Cleanup and return
        await perspectiveClient.remove(proxyPerspective.uuid);
        return agent
    }

    async updateDirectMessageLanguage(directMessageLanguage: string): Promise<Agent> {
        const { agentUpdateDirectMessageLanguage } = unwrapApolloResult(await this.#apolloClient.mutate({ 
            mutation: gql`mutation agentUpdateDirectMessageLanguage($directMessageLanguage: String!) {
                agentUpdateDirectMessageLanguage(directMessageLanguage: $directMessageLanguage) {
                    ${AGENT_SUBITEMS}
                }
            }`,
            variables: { directMessageLanguage }
        }))
        const a = agentUpdateDirectMessageLanguage
        const agent = new Agent(a.did, a.perspective)
        agent.directMessageLanguage = a.directMessageLanguage
        return agent
    }

    async addEntanglementProofs(proofs: EntanglementProofInput[]): Promise<EntanglementProof[]> {
        const { agentAddEntanglementProofs } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation agentAddEntanglementProofs($proofs: [EntanglementProofInput!]!) {
                agentAddEntanglementProofs(proofs: $proofs) {
                    ${ENTANGLEMENT_PROOF_FIELDS}
                }
            }`,
            variables: { proofs }
        }))
        return agentAddEntanglementProofs 
    }

    async deleteEntanglementProofs(proofs: EntanglementProofInput[]): Promise<EntanglementProof[]> {
        const { agentDeleteEntanglementProofs } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation agentDeleteEntanglementProofs($proofs: [EntanglementProofInput!]!) {
                agentDeleteEntanglementProofs(proofs: $proofs) {
                    ${ENTANGLEMENT_PROOF_FIELDS}
                }
            }`,
            variables: { proofs }
        }))
        return agentDeleteEntanglementProofs 
    }

    async getEntanglementProofs(): Promise<string[]> {
        const { agentGetEntanglementProofs } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`query agentGetEntanglementProofs {
                agentGetEntanglementProofs {
                    ${ENTANGLEMENT_PROOF_FIELDS}
                }
            }`,
        }))
        return agentGetEntanglementProofs
    }

    async entanglementProofPreFlight(deviceKey: string, deviceKeyType: string): Promise<EntanglementProof> {
        const { agentEntanglementProofPreFlight } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation agentEntanglementProofPreFlight($deviceKey: String!, $deviceKeyType: String!) {
                agentEntanglementProofPreFlight(deviceKey: $deviceKey, deviceKeyType: $deviceKeyType) {
                    ${ENTANGLEMENT_PROOF_FIELDS}
                }
            }`,
            variables: { deviceKey, deviceKeyType }
        }))
        return agentEntanglementProofPreFlight
    }

    addUpdatedListener(listener) {
        this.#updatedCallbacks.push(listener)
    }

    subscribeAgentUpdated() {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                agentUpdated { ${AGENT_SUBITEMS} }
            }   
        `}).subscribe({
            next: result => {
                const agent = result.data.agentUpdated
                this.#updatedCallbacks.forEach(cb => {
                    cb(agent)
                })
            },
            error: (e) => console.error(e)
        })
    }

    addAgentStatusChangedListener(listener) {
        this.#agentStatusChangedCallbacks.push(listener)
    }

    subscribeAgentStatusChanged() {
        this.#apolloClient.subscribe({
            query: gql` subscription {
                agentStatusChanged { ${AGENT_STATUS_FIELDS} }
            }   
        `}).subscribe({
            next: result => {
                const agent = result.data.agentStatusChanged
                this.#agentStatusChangedCallbacks.forEach(cb => {
                    cb(agent)
                })
            },
            error: (e) => console.error(e)
        })
    }

    async requestCapability(appName: string, appDesc: string, appUrl: string, capabilities: string): Promise<string> {
        const { agentRequestCapability } = unwrapApolloResult(await this.#apolloClient.mutate({ 
            mutation: gql`mutation agentRequestCapability($appName: String!, $appDesc: String!, $appUrl: String!, $capabilities: String!) {
                agentRequestCapability(appName: $appName, appDesc: $appDesc, appUrl: $appUrl, capabilities: $capabilities)
            }`,
            variables: { appName, appDesc, appUrl, capabilities }
        }))
        return agentRequestCapability
    }

    async permitCapability(auth: string): Promise<string> {
        const { agentPermitCapability } = unwrapApolloResult(await this.#apolloClient.mutate({ 
            mutation: gql`mutation agentPermitCapability($auth: String!) {
                agentPermitCapability(auth: $auth)
            }`,
            variables: { auth }
        }))
        return agentPermitCapability
    }

    async generateJwt(requestId: string, rand: string): Promise<string> {
        const { agentGenerateJwt } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`mutation agentGenerateJwt($requestId: String!, $rand: String!) {
                agentGenerateJwt(requestId: $requestId, rand: $rand)
            }`,
            variables: { requestId, rand }
        }))
        return agentGenerateJwt
    }

    async isLocked(): Promise<boolean> {
        const { agentIsLocked } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`query agentIsLocked {
                agentIsLocked
            }`,
        }))
        return agentIsLocked
    }
}
