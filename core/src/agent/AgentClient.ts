import { RestClient } from "../restClient";
import { PerspectiveInput } from "../perspectives/Perspective";
import {
  Agent,
  Apps,
  AuthInfo,
  AuthInfoInput,
  EntanglementProof,
  EntanglementProofInput,
  UserCreationResult,
} from "./Agent";
import { HostingUserInfo, PaymentRequestResult } from "../runtime/RuntimeResolver";
import { AgentStatus } from "./AgentStatus";
import { LinkMutations } from "../links/Links";
import { PerspectiveClient } from "../perspectives/PerspectiveClient";
import { VerificationRequestResult } from "../runtime/RuntimeResolver";

export interface InitializeArgs {
  did: string;
  didDocument: string;
  keystore: string;
  passphrase: string;
}

export type AgentUpdatedCallback = (agent: Agent) => null;
export type AgentStatusChangedCallback = (agent: Agent) => null;
export type AgentAppsUpdatedCallback = () => null;
export type HostingUserInfoChangedCallback = (info: HostingUserInfo) => void;
export type ComputeLogUpdatedCallback = (entry: any) => void;

/**
 * Provides access to all functions regarding the local agent,
 * such as generating, locking, unlocking, importing the DID keystore,
 * as well as updating the publicly shared Agent expression.
 */
export class AgentClient {
  #restClient: RestClient;
  #baseUrl: string;
  #token?: string;
  #appsChangedCallback: AgentAppsUpdatedCallback[];
  #updatedCallbacks: AgentUpdatedCallback[];
  #agentStatusChangedCallbacks: AgentStatusChangedCallback[];
  #hostingUserInfoChangedCallbacks: HostingUserInfoChangedCallback[];
  #computeLogUpdatedCallbacks: ComputeLogUpdatedCallback[];
  #unsubscribers: (() => void)[];

  constructor(baseUrl: string, token?: string, subscribe: boolean = true) {
    this.#baseUrl = baseUrl;
    this.#token = token;
    this.#restClient = new RestClient(baseUrl, token);
    this.#updatedCallbacks = [];
    this.#agentStatusChangedCallbacks = [];
    this.#appsChangedCallback = [];
    this.#hostingUserInfoChangedCallbacks = [];
    this.#computeLogUpdatedCallbacks = [];
    this.#unsubscribers = [];

    if (subscribe) {
      this.subscribeAgentUpdated();
      this.subscribeAgentStatusChanged();
      this.subscribeAppsChanged();
    }
  }

  async me(): Promise<Agent> {
    const agent = await this.#restClient.get<any>('/api/v1/agent');
    let agentObject = new Agent(agent.did, agent.perspective);
    agentObject.directMessageLanguage = agent.directMessageLanguage;
    return agentObject;
  }

  async status(): Promise<AgentStatus> {
    const agentStatus = await this.#restClient.get<any>('/api/v1/agent/status');
    return new AgentStatus(agentStatus);
  }

  async generate(passphrase: string): Promise<AgentStatus> {
    const result = await this.#restClient.post<any>('/api/v1/agent', { passphrase });
    return new AgentStatus(result);
  }

  async import(args: InitializeArgs): Promise<AgentStatus> {
    const result = await this.#restClient.post<any>('/api/v1/agent/import', args);
    return new AgentStatus(result);
  }

  async lock(passphrase: string): Promise<AgentStatus> {
    const result = await this.#restClient.post<any>('/api/v1/agent/lock', { passphrase });
    return new AgentStatus(result);
  }

  async unlock(passphrase: string, holochain = true): Promise<AgentStatus> {
    const result = await this.#restClient.post<any>('/api/v1/agent/unlock', { passphrase, holochain });
    return new AgentStatus(result);
  }

  async byDID(did: string): Promise<Agent> {
    return this.#restClient.get<Agent>(`/api/v1/agent/byDID/${encodeURIComponent(did)}`);
  }

  async updatePublicPerspective(perspective: PerspectiveInput): Promise<Agent> {
    const cleanedPerspective = JSON.parse(JSON.stringify(perspective));
    delete cleanedPerspective.__typename;
    cleanedPerspective.links.forEach((link: any) => {
      delete link.__typename;
      delete link.data.__typename;
      delete link.proof.__typename;
      delete link.status;
    });

    const a = await this.#restClient.put<any>('/api/v1/agent/perspective', { perspective: cleanedPerspective });
    const agent = new Agent(a.did, a.perspective);
    agent.directMessageLanguage = a.directMessageLanguage;
    return agent;
  }

  async mutatePublicPerspective(mutations: LinkMutations): Promise<Agent> {
    const perspectiveClient = new PerspectiveClient(this.#baseUrl, this.#token);
    const agentClient = new AgentClient(this.#baseUrl, this.#token);

    const proxyPerspective = await perspectiveClient.add("Agent Perspective Proxy");
    const agentMe = await agentClient.me();

    if (agentMe.perspective) {
      await proxyPerspective.loadSnapshot(agentMe.perspective);
    }

    for (const addition of mutations.additions) {
      await proxyPerspective.add(addition);
    }
    for (const removal of mutations.removals) {
      await proxyPerspective.remove(removal);
    }

    const snapshot = await proxyPerspective.snapshot();
    const agent = await this.updatePublicPerspective(snapshot);
    await perspectiveClient.remove(proxyPerspective.uuid);
    return agent;
  }

  async updateDirectMessageLanguage(directMessageLanguage: string): Promise<Agent> {
    const a = await this.#restClient.put<any>('/api/v1/agent/directMessageLanguage', { directMessageLanguage });
    const agent = new Agent(a.did, a.perspective);
    agent.directMessageLanguage = a.directMessageLanguage;
    return agent;
  }

  async addEntanglementProofs(proofs: EntanglementProofInput[]): Promise<EntanglementProof[]> {
    return this.#restClient.post<EntanglementProof[]>('/api/v1/agent/entanglement-proofs', { proofs });
  }

  async deleteEntanglementProofs(proofs: EntanglementProofInput[]): Promise<EntanglementProof[]> {
    return this.#restClient.delete<EntanglementProof[]>('/api/v1/agent/entanglement-proofs', { proofs });
  }

  async getEntanglementProofs(): Promise<string[]> {
    return this.#restClient.get<string[]>('/api/v1/agent/entanglement-proofs');
  }

  async entanglementProofPreFlight(deviceKey: string, deviceKeyType: string): Promise<EntanglementProof> {
    return this.#restClient.post<EntanglementProof>('/api/v1/agent/entanglement-proof-preflight', { deviceKey, deviceKeyType });
  }

  addUpdatedListener(listener: AgentUpdatedCallback) {
    this.#updatedCallbacks.push(listener);
  }

  addAppChangedListener(listener: AgentAppsUpdatedCallback) {
    this.#appsChangedCallback.push(listener);
  }

  subscribeAgentUpdated() {
    const unsub = this.#restClient.subscribe('/api/v1/events/agent', (data) => {
      if (data.type === 'agent-updated') {
        this.#updatedCallbacks.forEach((cb) => cb(data.agent));
      }
    });
    this.#unsubscribers.push(unsub);
  }

  subscribeAppsChanged() {
    const unsub = this.#restClient.subscribe('/api/v1/events/agent', (data) => {
      if (data.type === 'apps-changed') {
        this.#appsChangedCallback.forEach((cb) => cb());
      }
    });
    this.#unsubscribers.push(unsub);
  }

  addAgentStatusChangedListener(listener: AgentStatusChangedCallback) {
    this.#agentStatusChangedCallbacks.push(listener);
  }

  subscribeAgentStatusChanged() {
    const unsub = this.#restClient.subscribe('/api/v1/events/agent', (data) => {
      if (data.type === 'agent-status-changed') {
        this.#agentStatusChangedCallbacks.forEach((cb) => cb(data.agent));
      }
    });
    this.#unsubscribers.push(unsub);
  }

  addHostingUserInfoChangedListener(listener: HostingUserInfoChangedCallback) {
    this.#hostingUserInfoChangedCallbacks.push(listener);
  }

  subscribeHostingUserInfoChanged() {
    const unsub = this.#restClient.subscribe('/api/v1/events/agent', (data) => {
      if (data.type === 'hosting-user-info-changed') {
        this.#hostingUserInfoChangedCallbacks.forEach((cb) => cb(data.info));
      }
    });
    this.#unsubscribers.push(unsub);
  }

  addComputeLogUpdatedListener(listener: ComputeLogUpdatedCallback) {
    this.#computeLogUpdatedCallbacks.push(listener);
  }

  subscribeComputeLogUpdated() {
    const unsub = this.#restClient.subscribe('/api/v1/events/agent', (data) => {
      if (data.type === 'compute-log-updated') {
        this.#computeLogUpdatedCallbacks.forEach((cb) => cb(data.entry));
      }
    });
    this.#unsubscribers.push(unsub);
  }

  async computeLog(since?: string, limit?: number, userEmail?: string): Promise<any[]> {
    const params = new URLSearchParams();
    if (since) params.set('since', since);
    if (limit !== undefined) params.set('limit', String(limit));
    if (userEmail) params.set('userEmail', userEmail);
    const qs = params.toString();
    return this.#restClient.get<any[]>(`/api/v1/runtime/compute-log${qs ? '?' + qs : ''}`);
  }

  async requestCapability(authInfo: AuthInfoInput): Promise<string> {
    return this.#restClient.post<string>('/api/v1/agent/capability/request', { authInfo });
  }

  async permitCapability(auth: string): Promise<string> {
    return this.#restClient.post<string>('/api/v1/agent/capability/permit', { auth });
  }

  async generateJwt(requestId: string, rand: string): Promise<string> {
    return this.#restClient.post<string>('/api/v1/agent/jwt', { requestId, rand });
  }

  async getApps(): Promise<Apps[]> {
    return this.#restClient.get<Apps[]>('/api/v1/agent/apps');
  }

  async removeApp(requestId: string): Promise<Apps[]> {
    return this.#restClient.delete<Apps[]>(`/api/v1/agent/apps/${encodeURIComponent(requestId)}`);
  }

  async revokeToken(requestId: string): Promise<Apps[]> {
    return this.#restClient.post<Apps[]>(`/api/v1/agent/apps/${encodeURIComponent(requestId)}/revoke`);
  }

  async isLocked(): Promise<boolean> {
    return this.#restClient.get<boolean>('/api/v1/agent/isLocked');
  }

  async signMessage(message: string): Promise<string> {
    return this.#restClient.post<string>('/api/v1/agent/sign', { message });
  }

  // Multi-user methods
  async createUser(email: string, password: string, appInfo?: AuthInfoInput): Promise<UserCreationResult> {
    return this.#restClient.post<UserCreationResult>('/api/v1/runtime/users', { email, password, appInfo });
  }

  async loginUser(email: string, password: string): Promise<string> {
    return this.#restClient.post<string>('/api/v1/runtime/users/login', { email, password });
  }

  async requestLoginVerification(email: string, appInfo?: AuthInfoInput): Promise<VerificationRequestResult> {
    return this.#restClient.post<VerificationRequestResult>('/api/v1/runtime/users/request-verification', { email, appInfo });
  }

  async verifyEmailCode(email: string, code: string, verificationType: string): Promise<string> {
    return this.#restClient.post<string>('/api/v1/runtime/users/verify-email', { email, code, verificationType });
  }

  // Hosting methods
  async hostingUserInfo(): Promise<HostingUserInfo> {
    return this.#restClient.get<HostingUserInfo>('/api/v1/runtime/hosting/user-info');
  }

  async setHotWalletAddress(address: string): Promise<boolean> {
    return this.#restClient.put<boolean>('/api/v1/runtime/hosting/hot-wallet-address', { address });
  }

  async requestPayment(amountHOT: string): Promise<PaymentRequestResult> {
    return this.#restClient.post<PaymentRequestResult>('/api/v1/runtime/hosting/request-payment', { amountHOT });
  }
}
