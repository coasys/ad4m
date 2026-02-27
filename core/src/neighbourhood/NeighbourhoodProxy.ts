import { DID } from "../DID";
import { OnlineAgent } from "../language/Language";
import {
  Perspective,
  PerspectiveExpression,
  PerspectiveUnsignedInput,
} from "../perspectives/Perspective";
import { NeighbourhoodClient } from "./NeighbourhoodClient";

export class NeighbourhoodProxy {
  private _client: NeighbourhoodClient;
  private _pID: string;

  constructor(client: NeighbourhoodClient, pID: string) {
    this._client = client;
    this._pID = pID;
  }

  async otherAgents(): Promise<DID[]> {
    return await this._client.otherAgents(this._pID);
  }

  async hasTelepresenceAdapter(): Promise<boolean> {
    return await this._client.hasTelepresenceAdapter(this._pID);
  }

  async onlineAgents(): Promise<OnlineAgent[]> {
    return await this._client.onlineAgents(this._pID);
  }

  async setOnlineStatus(status: Perspective): Promise<boolean> {
    return await this._client.setOnlineStatus(this._pID, status);
  }

  async setOnlineStatusU(status: PerspectiveUnsignedInput): Promise<boolean> {
    return await this._client.setOnlineStatusU(this._pID, status);
  }

  async sendSignal(
    remoteAgentDid: string,
    payload: Perspective,
  ): Promise<boolean> {
    return await this._client.sendSignal(this._pID, remoteAgentDid, payload);
  }

  async sendSignalU(
    remoteAgentDid: string,
    payload: PerspectiveUnsignedInput,
  ): Promise<boolean> {
    return await this._client.sendSignalU(this._pID, remoteAgentDid, payload);
  }

  async sendBroadcast(
    payload: Perspective,
    loopback: boolean = false,
  ): Promise<boolean> {
    return await this._client.sendBroadcast(this._pID, payload, loopback);
  }

  async sendBroadcastU(
    payload: PerspectiveUnsignedInput,
    loopback: boolean = false,
  ): Promise<boolean> {
    return await this._client.sendBroadcastU(this._pID, payload, loopback);
  }

  async addSignalHandler(
    handler: (payload: PerspectiveExpression) => void,
  ): Promise<void> {
    await this._client.addSignalHandler(this._pID, handler);
  }

  removeSignalHandler(handler: (payload: PerspectiveExpression) => void) {
    this._client.removeSignalHandler(this._pID, handler);
  }
}
