import { ApiClient } from "../apiClient";
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
import { HostingUserInfo, PaymentRequestResult, ComputeLogEntry } from "../runtime/RuntimeTypes";
import { AgentStatus } from "./AgentStatus";
import { LinkMutations, LinkExpression } from "../links/Links";
import { PerspectiveClient } from "../perspectives/PerspectiveClient";
import { VerificationRequestResult } from "../runtime/RuntimeTypes";
import type {
  GenerateAgentRequest,
  ImportAgentRequest,
  LockAgentRequest,
  UnlockAgentRequest,
  SignMessageRequest,
  PermitCapabilityRequest,
  GenerateJwtRequest,
} from "../generated/api";

export interface InitializeArgs {
  did: string;
  didDocument: string;
  keystore: string;
  passphrase: string;
}

export type AgentUpdatedCallback = (agent: Agent) => void;
export type AgentStatusChangedCallback = (agent: Agent) => void;
export type AgentAppsUpdatedCallback = () => void;
export type HostingUserInfoChangedCallback = (info: HostingUserInfo) => void;
export type ComputeLogUpdatedCallback = (entry: ComputeLogEntry) => void;

export class AgentClient {
  #apiClient: ApiClient;
  #baseUrl: string;
  #token?: string;
  #appsChangedCallback: AgentAppsUpdatedCallback[];
  #updatedCallbacks: AgentUpdatedCallback[];
  #agentStatusChangedCallbacks: AgentStatusChangedCallback[];
  #hostingUserInfoChangedCallbacks: HostingUserInfoChangedCallback[];
  #computeLogUpdatedCallbacks: ComputeLogUpdatedCallback[];
  #unsubscribers: (() => void)[];

  constructor(baseUrl: string, token?: string, subscribe: boolean = true, sharedApiClient?: ApiClient) {
    this.#baseUrl = baseUrl;
    this.#token = token;
    this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
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
    const agent = await this.#apiClient.call<Agent>('agent.get');
    let agentObject = new Agent(agent.did, agent.perspective);
    agentObject.directMessageLanguage = agent.directMessageLanguage;
    return agentObject;
  }

  async status(): Promise<AgentStatus> {
    const agentStatus = await this.#apiClient.call<AgentStatus>('agent.status');
    return new AgentStatus(agentStatus);
  }

  async generate(passphrase: string): Promise<AgentStatus> {
    const result = await this.#apiClient.call<AgentStatus>('agent.generate', { passphrase });
    return new AgentStatus(result);
  }

  async import(args: InitializeArgs): Promise<AgentStatus> {
    const result = await this.#apiClient.call<AgentStatus>('agent.import', { ...args });
    return new AgentStatus(result);
  }

  async lock(passphrase: string): Promise<AgentStatus> {
    const result = await this.#apiClient.call<AgentStatus>('agent.lock', { passphrase });
    return new AgentStatus(result);
  }

  async unlock(passphrase: string, holochain = true): Promise<AgentStatus> {
    const result = await this.#apiClient.call<AgentStatus>('agent.unlock', { passphrase, holochain });
    return new AgentStatus(result);
  }

  async byDID(did: string): Promise<Agent> {
    return this.#apiClient.call<Agent>('agent.byDid', { did });
  }

  async updatePublicPerspective(perspective: PerspectiveInput): Promise<Agent> {
    const cleanedPerspective = JSON.parse(JSON.stringify(perspective));
    delete cleanedPerspective.__typename;
    cleanedPerspective.links.forEach((link: LinkExpression) => {
      delete link.__typename;
      delete link.data.__typename;
      delete link.proof.__typename;
      delete link.status;
    });

    const a = await this.#apiClient.call<Agent>('agent.updateProfile', { publicPerspective: cleanedPerspective });
    const agent = new Agent(a.did, a.perspective);
    agent.directMessageLanguage = a.directMessageLanguage;
    return agent;
  }

  async mutatePublicPerspective(mutations: LinkMutations): Promise<Agent> {
    const perspectiveClient = new PerspectiveClient(this.#baseUrl, this.#token);

    const proxyPerspective = await perspectiveClient.add("Agent Perspective Proxy");
    const agentMe = await this.me();

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
    const a = await this.#apiClient.call<Agent>('agent.updateProfile', { dmLanguage: directMessageLanguage });
    const agent = new Agent(a.did, a.perspective);
    agent.directMessageLanguage = a.directMessageLanguage;
    return agent;
  }

  async addEntanglementProofs(proofs: EntanglementProofInput[]): Promise<EntanglementProof[]> {
    return this.#apiClient.call<EntanglementProof[]>('agent.addEntanglementProofs', { proofs });
  }

  async deleteEntanglementProofs(proofs: EntanglementProofInput[]): Promise<EntanglementProof[]> {
    return this.#apiClient.call<EntanglementProof[]>('agent.deleteEntanglementProofs', { proofs });
  }

  async getEntanglementProofs(): Promise<string[]> {
    return this.#apiClient.call<string[]>('agent.getEntanglementProofs');
  }

  async entanglementProofPreFlight(deviceKey: string, deviceKeyType: string): Promise<EntanglementProof> {
    return this.#apiClient.call<EntanglementProof>('agent.entanglementProofPreflight', { deviceKey, deviceKeyType });
  }

  addUpdatedListener(listener: AgentUpdatedCallback) {
    this.#updatedCallbacks.push(listener);
  }

  addAppChangedListener(listener: AgentAppsUpdatedCallback) {
    this.#appsChangedCallback.push(listener);
  }

  subscribeAgentUpdated() {
    const unsub = this.#apiClient.subscribe((data) => {
      if (data.type === 'agent-updated') {
        this.#updatedCallbacks.forEach((cb) => cb((data.agent || data) as Agent));
      }
    });
    this.#unsubscribers.push(unsub);
  }

  subscribeAppsChanged() {
    const unsub = this.#apiClient.subscribe((data) => {
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
    const unsub = this.#apiClient.subscribe((data) => {
      if (data.type === 'agent-status-changed') {
        this.#agentStatusChangedCallbacks.forEach((cb) => cb((data.agent || data) as Agent));
      }
    });
    this.#unsubscribers.push(unsub);
  }

  addHostingUserInfoChangedListener(listener: HostingUserInfoChangedCallback) {
    this.#hostingUserInfoChangedCallbacks.push(listener);
  }

  subscribeHostingUserInfoChanged() {
    const unsub = this.#apiClient.subscribe((data) => {
      if (data.type === 'hosting-user-info-changed') {
        this.#hostingUserInfoChangedCallbacks.forEach((cb) => cb((data.info || data) as HostingUserInfo));
      }
    });
    this.#unsubscribers.push(unsub);
  }

  addComputeLogUpdatedListener(listener: ComputeLogUpdatedCallback) {
    this.#computeLogUpdatedCallbacks.push(listener);
  }

  subscribeComputeLogUpdated() {
    const unsub = this.#apiClient.subscribe((data) => {
      if (data.type === 'compute-log-updated') {
        this.#computeLogUpdatedCallbacks.forEach((cb) => cb((data.entry || data) as ComputeLogEntry));
      }
    });
    this.#unsubscribers.push(unsub);
  }

  async requestCapability(authInfo: AuthInfoInput): Promise<string> {
    return this.#apiClient.call<string>('agent.requestCapability', { authInfo });
  }

  async permitCapability(auth: string): Promise<string> {
    return this.#apiClient.call<string>('agent.permitCapability', { auth });
  }

  async generateJwt(requestId: string, rand: string): Promise<string> {
    return this.#apiClient.call<string>('agent.generateJwt', { requestId, rand });
  }

  async getApps(): Promise<Apps[]> {
    return this.#apiClient.call<Apps[]>('agent.getApps');
  }

  async removeApp(requestId: string): Promise<Apps[]> {
    return this.#apiClient.call<Apps[]>('agent.removeApp', { id: requestId });
  }

  async revokeToken(requestId: string): Promise<Apps[]> {
    return this.#apiClient.call<Apps[]>('agent.revokeToken', { token: requestId });
  }

  async isLocked(): Promise<boolean> {
    return this.#apiClient.call<boolean>('agent.isLocked');
  }

  async signMessage(message: string): Promise<string> {
    return this.#apiClient.call<string>('agent.sign', { message });
  }

  // Multi-user methods
  async createUser(email: string, password: string, appInfo?: AuthInfoInput): Promise<UserCreationResult> {
    return this.#apiClient.call<UserCreationResult>('user.create', { email, password, appInfo });
  }

  async loginUser(email: string, password: string): Promise<string> {
    return this.#apiClient.call<string>('user.login', { email, password });
  }

  async requestLoginVerification(email: string, appInfo?: AuthInfoInput): Promise<VerificationRequestResult> {
    return this.#apiClient.call<VerificationRequestResult>('user.requestVerification', { email, appInfo });
  }

  async verifyEmailCode(email: string, code: string, verificationType: string): Promise<string> {
    return this.#apiClient.call<string>('user.verifyEmail', { email, code, verificationType });
  }

  // Hosting methods
  async hostingUserInfo(): Promise<HostingUserInfo> {
    const resp = await this.#apiClient.call<any>('hosting.info');
    const info = resp?.userInfo || resp;
    return new HostingUserInfo(
      info.email || '',
      info.freeAccess ? 'unlimited' : String(info.credits ?? info.remainingCredits ?? '0'),
      info.hotWalletAddress || undefined,
      !!info.freeAccess,
    );
  }

  async computeLog(since?: string, limit?: number, userEmail?: string): Promise<ComputeLogEntry[]> {
    return this.#apiClient.call<ComputeLogEntry[]>('runtime.computeLog', { since, limit, userEmail });
  }

  async setHotWalletAddress(address: string): Promise<boolean> {
    return this.#apiClient.call<boolean>('hosting.setHotWallet', { address });
  }

  async requestPayment(amountHOT: string): Promise<PaymentRequestResult> {
    return this.#apiClient.call<PaymentRequestResult>('hosting.requestPayment', { amountHOT });
  }
}
