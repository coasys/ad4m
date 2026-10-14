import {ApiClient, CallOptions, RpcError } from '../apiClient';
import { PerspectiveInput } from "../perspectives/Perspective";
import {
  Agent,
  Apps,
  AuthInfoInput,
  EntanglementProof,
  EntanglementProofInput,
  UserCreationResult,
} from "./Agent";
import { HostingUserInfo, PaymentRequestResult, ComputeLogEntry } from "../runtime/RuntimeTypes";
import { AgentStatus } from "./AgentStatus";
import { LinkMutations, LinkExpression, LinkInput, linkEqual } from "../links/Links";
import { VerificationRequestResult } from "../runtime/RuntimeTypes";
import { createPersistentCache } from "../cache/PersistentCache";
import { KeyedCache, StoredEntry } from "../cache/KeyedCache";
import type { Agent as AgentData } from "../generated/api/Agent";
import type { AgentSignature } from "../generated/api/AgentSignature";

export interface InitializeArgs {
  did: string;
  didDocument: string;
  keystore: string;
  passphrase: string;
}

function toAgent(data: AgentData | null): Agent | null {
  return data ? Agent.fromWire(data) : null;
}

/** Most DIDs the executor takes in one `agent.byDIDs` call. */
const MAX_DIDS_PER_CALL = 200;

export class AgentClient {
  #apiClient: ApiClient;

  // byDID cache: concurrent misses go out in one `agent.byDIDs` call.
  #agents: KeyedCache<Agent>;
  // The own profile is kept fresh by `agent-updated` events, so it has no TTL.
  #selfDid: string | null = null;
  /** TTL of other agents' profiles, in memory and in the persistent cache (ms). */
  static REMOTE_AGENT_TTL_MS = 5 * 60_000; // 5 minutes

  constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
    this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    this.#agents = new KeyedCache<Agent>({
      loadMany: (dids) => this.#loadAgents(dids),
      maxBatch: MAX_DIDS_PER_CALL,
      persistent: createPersistentCache<StoredEntry<Agent>>('ad4m-agent-cache', 'agents'),
      maxAgeMs: (did) => did === this.#selfDid ? Infinity : AgentClient.REMOTE_AGENT_TTL_MS,
      persistentMaxAgeMs: () => AgentClient.REMOTE_AGENT_TTL_MS,
    });
  }

  async me(): Promise<Agent> {
    const agentObject = toAgent(await this.#apiClient.call('agent.get', {}));

    // Auto-set selfDid so event-driven cache invalidation works
    // without requiring apps to call setSelfDid() manually
    if (agentObject.did && !this.#selfDid) {
      this.#selfDid = agentObject.did;
    }

    return agentObject;
  }

  async status(): Promise<AgentStatus> {
    const agentStatus = await this.#apiClient.call('agent.status', {});
    return new AgentStatus(agentStatus);
  }

  async generate(passphrase: string, options?: CallOptions): Promise<AgentStatus> {
    const result = await this.#apiClient.call('agent.generate', { passphrase }, options);
    return new AgentStatus(result);
  }

  async import(args: InitializeArgs): Promise<AgentStatus> {
    const result = await this.#apiClient.call('agent.import', { ...args });
    return new AgentStatus(result);
  }

  async lock(passphrase: string): Promise<AgentStatus> {
    const result = await this.#apiClient.call('agent.lock', { passphrase });
    return new AgentStatus(result);
  }

  async unlock(passphrase: string, holochain = true, options?: CallOptions): Promise<AgentStatus> {
    const result = await this.#apiClient.call('agent.unlock', { passphrase, holochain }, options);
    return new AgentStatus(result);
  }

  /**
   * The agent with this DID, or `null` when it is unknown. Concurrent calls
   * for any DIDs share one `agent.byDIDs` request.
   */
  async byDID(did: string): Promise<Agent> {
    // agent-updated events keep cached entries fresh (the self-DID entry has no TTL).
    this.#listen();
    return this.#agents.get(did);
  }

  /**
   * One `agent.byDIDs` call for the misses of one tick. The executor fails
   * that call when one DID's lookup fails (500), so the DIDs of a failed
   * batch are then read alone: each caller gets what a lone `byDID` gives.
   */
  async #loadAgents(dids: string[]): Promise<Array<Agent | null | Error>> {
    try {
      return (await this.#apiClient.call('agent.byDIDs', { dids })).map(toAgent);
    } catch (error) {
      if (dids.length === 1 || !(error instanceof RpcError) || error.status !== 500) throw error;
      return Promise.all(dids.map(did =>
        this.#apiClient.call('agent.byDid', { did }).then(toAgent, (failure) =>
          failure instanceof Error ? failure : new Error(String(failure)))));
    }
  }

  #cacheAgent(agent: Agent): void {
    if (!agent.did) return;
    this.#agents.set(agent.did, agent);
    this.#listen();
  }

  /**
   * Set the current agent's own DID.
   * The self-DID receives event-driven invalidation and is never TTL-expired.
   */
  setSelfDid(did: string): void {
    this.#selfDid = did;
  }

  /**
   * Invalidate a specific DID's cache entry in both L1 and L2.
   */
  invalidateByDid(did: string): void {
    this.#agents.delete(did);
  }

  /**
   * Clear the entire L1 (memory) byDID cache.
   * L2 (IndexedDB) is preserved — agent data remains valid across sessions.
   */
  clearByDidCache(): void {
    this.#agents.clear();
  }

  async updatePublicPerspective(perspective: PerspectiveInput): Promise<Agent> {
    // Send only the signed fields: the public perspective carries no link status.
    const publicPerspective = {
      links: perspective.links.map(({ author, timestamp, data, proof }) => ({
        author,
        timestamp,
        data: { source: data.source, target: data.target, predicate: data.predicate },
        proof: { key: proof.key, signature: proof.signature, valid: proof.valid, invalid: proof.invalid },
      })),
    };
    const agent = toAgent(await this.#apiClient.call('agent.updateProfile', { publicPerspective }));

    // Immediately update byDID cache so subsequent byDID() calls
    // return fresh data without waiting for the agent-updated event
    this.#cacheAgent(agent);

    return agent;
  }

  async mutatePublicPerspective({ additions, removals }: LinkMutations): Promise<Agent> {
    const added = additions.length > 0 ? await this.#signLinks(additions) : [];
    const { perspective } = await this.me();
    const kept = (perspective?.links ?? []).filter(link => !removals.some(r => linkEqual(link, r as LinkExpression)));
    return this.updatePublicPerspective({ links: [...kept, ...added] } as PerspectiveInput);
  }

  /** Has the executor sign `links` as this agent, in a throwaway perspective. */
  async #signLinks(links: LinkInput[]): Promise<LinkExpression[]> {
    const { uuid } = await this.#apiClient.call('perspective.create', { name: 'Agent Perspective Proxy' });
    try {
      return (await this.#apiClient.call('perspective.addLinks', { uuid, links, status: 'SHARED' })).map(LinkExpression.fromWire);
    } finally {
      await this.#apiClient.call('perspective.remove', { uuid });
    }
  }

  async updateDirectMessageLanguage(directMessageLanguage: string): Promise<Agent> {
    const agent = toAgent(await this.#apiClient.call('agent.updateProfile', { dmLanguage: directMessageLanguage }));

    // Immediately update byDID cache so subsequent byDID() calls
    // return fresh data without waiting for the agent-updated event
    this.#cacheAgent(agent);

    return agent;
  }

  async addEntanglementProofs(proofs: EntanglementProofInput[]): Promise<EntanglementProof[]> {
    return this.#apiClient.call('agent.addEntanglementProofs', { proofs });
  }

  async deleteEntanglementProofs(proofs: EntanglementProofInput[]): Promise<EntanglementProof[]> {
    return this.#apiClient.call('agent.deleteEntanglementProofs', { proofs });
  }

  async getEntanglementProofs(): Promise<EntanglementProof[]> {
    return this.#apiClient.call('agent.getEntanglementProofs', {});
  }

  async entanglementProofPreFlight(deviceKey: string, deviceKeyType: string): Promise<EntanglementProof> {
    return this.#apiClient.call('agent.entanglementProofPreflight', { deviceKey, deviceKeyType });
  }

  /** Keeps cached agents fresh; registering again changes nothing. */
  #listen(): void {
    this.#apiClient.on('agent-updated', this.#onAgentUpdated);
  }

  #onAgentUpdated = (event: { agent: AgentData }): void => {
    this.#cacheAgent(Agent.fromWire(event.agent));
  };

  async requestCapability(authInfo: AuthInfoInput): Promise<string> {
    return this.#apiClient.call('agent.requestCapability', { authInfo });
  }

  async permitCapability(auth: string): Promise<string> {
    return this.#apiClient.call('agent.permitCapability', { auth });
  }

  async generateJwt(requestId: string, rand: string): Promise<string> {
    return this.#apiClient.call('agent.generateJwt', { requestId, rand });
  }

  async getApps(): Promise<Apps[]> {
    return this.#apiClient.call('agent.getApps', {});
  }

  async removeApp(requestId: string): Promise<Apps[]> {
    return this.#apiClient.call('agent.removeApp', { id: requestId });
  }

  async revokeToken(requestId: string): Promise<Apps[]> {
    return this.#apiClient.call('agent.revokeToken', { token: requestId });
  }

  async isLocked(): Promise<boolean> {
    return this.#apiClient.call('agent.isLocked', {});
  }

  async signMessage(message: string): Promise<AgentSignature> {
    return this.#apiClient.call('agent.sign', { message });
  }

  // Multi-user methods
  async createUser(email: string, password: string): Promise<UserCreationResult> {
    return this.#apiClient.call('user.create', { email, password });
  }

  async loginUser(email: string, password: string): Promise<string> {
    return this.#apiClient.call('user.login', { email, password });
  }

  async requestLoginVerification(email: string, appInfo?: AuthInfoInput): Promise<VerificationRequestResult> {
    return this.#apiClient.call('user.requestVerification', { email, appInfo });
  }

  async verifyEmailCode(email: string, code: string, verificationType: string): Promise<string> {
    return this.#apiClient.call('user.verifyEmail', { email, code, verificationType });
  }

  // Hosting methods
  async hostingUserInfo(): Promise<HostingUserInfo> {
    const info = (await this.#apiClient.call('hosting.info', {})).userInfo ?? {
      email: '', credits: null, hotWalletAddress: null, freeAccess: false,
    };
    return new HostingUserInfo(
      info.email,
      info.freeAccess ? 'unlimited' : String(info.credits ?? 0),
      info.hotWalletAddress || undefined,
      !!info.freeAccess,
    );
  }

  async computeLog(since?: string, limit?: number, userEmail?: string): Promise<ComputeLogEntry[]> {
    return this.#apiClient.call('runtime.computeLog', { since, limit, userEmail });
  }

  async setHotWalletAddress(address: string): Promise<boolean> {
    return this.#apiClient.call('hosting.setHotWallet', { address });
  }

  async requestPayment(amountHOT: string): Promise<PaymentRequestResult> {
    return this.#apiClient.call('hosting.requestPayment', { amountHOT });
  }
}
