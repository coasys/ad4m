import { DID } from "../DID";
import { OnlineAgent } from "../language/Language";
import { Perspective, PerspectiveExpression, PerspectiveUnsignedInput } from "../perspectives/Perspective";
import { NeighbourhoodClient } from "./NeighbourhoodClient";

export class NeighbourhoodProxy {
    #client: NeighbourhoodClient
    #pID: string

    constructor(client: NeighbourhoodClient, pID: string) {
        this.#client = client
        this.#pID = pID
    }

    async otherAgents(): Promise<DID[]> {
        return await this.#client.otherAgents(this.#pID)
    }

    async hasTelepresenceAdapter(): Promise<boolean> {
        return await this.#client.hasTelepresenceAdapter(this.#pID)
    }

    async onlineAgents(): Promise<OnlineAgent[]> {
        return await this.#client.onlineAgents(this.#pID)
    }

    async setOnlineStatus(status: Perspective): Promise<boolean> {
        return await this.#client.setOnlineStatus(this.#pID, status)
    }

    async setOnlineStatusU(status: PerspectiveUnsignedInput): Promise<boolean> {
        return await this.#client.setOnlineStatusU(this.#pID, status)
    }

    async sendSignal(remoteAgentDid: string, payload: Perspective): Promise<boolean> {
        return await this.#client.sendSignal(this.#pID, remoteAgentDid, payload)
    }

    async sendSignalU(remoteAgentDid: string, payload: PerspectiveUnsignedInput): Promise<boolean> {
        return await this.#client.sendSignalU(this.#pID, remoteAgentDid, payload)
    }

    async sendBroadcast(payload: Perspective, loopback: boolean = false): Promise<boolean> {
        return await this.#client.sendBroadcast(this.#pID, payload, loopback)
    }

    async sendBroadcastU(payload: PerspectiveUnsignedInput, loopback: boolean = false): Promise<boolean> {
        return await this.#client.sendBroadcastU(this.#pID, payload, loopback)
    }

    async addSignalHandler(handler: (payload: PerspectiveExpression) => void): Promise<void> {
        await this.#client.addSignalHandler(this.#pID, handler)
    }

    removeSignalHandler(handler: (payload: PerspectiveExpression) => void) {
        this.#client.removeSignalHandler(this.#pID, handler)
    }
}
