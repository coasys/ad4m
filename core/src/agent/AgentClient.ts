import {ApiClient, CallOptions } from '../apiClient';
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
import { PersistentCache, createPersistentCache } from "../cache/PersistentCache";
import type { Agent as AgentData } from "../generated/api/Agent";
import { ServiceClient } from "../services/ServiceClient";
import { BillingLedger_1_0_0 } from "../generated/services/billing.ledger";
import type { BillingLedger_1_0_0_Events, BillingLedger_1_0_0_Methods } from "../generated/services/billing.ledger";
import { BillingSettlement_1_0_0 } from "../generated/services/billing.settlement";
import type { BillingSettlement_1_0_0_Events, BillingSettlement_1_0_0_Methods } from "../generated/services/billing.settlement";
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

export class AgentClient {
  #apiClient: ApiClient;
  #ledger: ServiceClient<BillingLedger_1_0_0_Methods, BillingLedger_1_0_0_Events>;
  #settlement: ServiceClient<BillingSettlement_1_0_0_Methods, BillingSettlement_1_0_0_Events>;

  // ── byDID cache ────────────────────────────────────────────────────
  // L1: in-memory promise cache with timestamps for TTL
  #memCache = new Map<string, { promise: Promise<Agent>; ts: number }>();
  // L2: persistent cache (IndexedDB in browser, NullCache in Node)
  // Entries are wrapped with a timestamp so the TTL can be enforced across restarts.
  #persistent: PersistentCache<{ agent: Agent; ts: number }>;
  // Self-DID for event-driven invalidation (no TTL for own profile)
  #selfDid: string | null = null;
  /** TTL for remote (non-self) agent profiles in L1 cache (ms). */
  static REMOTE_AGENT_TTL_MS = 5 * 60_000; // 5 minutes
  /** TTL for remote agent profiles in the L2 (IndexedDB) cache (ms).
   *  After this window any restart triggers a fresh network fetch. */
  static REMOTE_AGENT_TTL_L2_MS = 5 * 60_000; // 5 minutes

  constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
    this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    this.#ledger = new ServiceClient(this.#apiClient, BillingLedger_1_0_0);
    this.#settlement = new ServiceClient(this.#apiClient, BillingSettlement_1_0_0);
    this.#persistent = createPersistentCache<{ agent: Agent; ts: number }>('ad4m-agent-cache', 'agents');
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

  async byDID(did: string): Promise<Agent> {
    // agent-updated events keep cached entries fresh (the self-DID entry has no TTL).
    this.#listen();
    const now = Date.now();
    const cached = this.#memCache.get(did);

    if (cached) {
      const isSelf = did === this.#selfDid;
      // Self-DID: L1 always valid (event-driven invalidation)
      // Remote DID: L1 valid within TTL
      if (isSelf || (now - cached.ts) < AgentClient.REMOTE_AGENT_TTL_MS) {
        return cached.promise;
      }
    }

    // Deduplicate: store the promise immediately so concurrent calls share one RPC
    const promise = (async () => {
      // L2: check persistent cache before network
      const entry = await this.#persistent.get(did);
      // entry.agent may be undefined for pre-TTL cache entries (migration) — treat as expired
      if (entry?.agent && (now - (entry.ts ?? 0)) < AgentClient.REMOTE_AGENT_TTL_L2_MS) {
        return entry.agent;
      }

      // L3: network fetch
      const result = toAgent(await this.#apiClient.call('agent.byDid', { did }));
      this.#persistent.put(did, { agent: result, ts: Date.now() }); // fire-and-forget write to L2
      return result;
    })();

    this.#memCache.set(did, { promise, ts: now });

    // Clean up on failure so next call retries
    promise.catch(() => {
      if (this.#memCache.get(did)?.promise === promise) {
        this.#memCache.delete(did);
      }
    });

    return promise;
  }

  #cacheAgent(agent: Agent): void {
    if (!agent.did) return;
    this.#memCache.set(agent.did, { promise: Promise.resolve(agent), ts: Date.now() });
    this.#persistent.put(agent.did, { agent, ts: Date.now() }); // fire-and-forget
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
    this.#memCache.delete(did);
    this.#persistent.delete(did); // fire-and-forget
  }

  /**
   * Clear the entire L1 (memory) byDID cache.
   * L2 (IndexedDB) is preserved — agent data remains valid across sessions.
   */
  clearByDidCache(): void {
    this.#memCache.clear();
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
    const [account, wallet] = await Promise.all([
      this.#ledger.call('account', {}),
      this.#settlement.call('linkedWallet', {}),
    ]);
    const info = account ?? { email: '', credits: null, freeAccess: false };
    return new HostingUserInfo(
      info.email,
      info.freeAccess ? 'unlimited' : String(info.credits ?? 0),
      wallet || undefined,
      !!info.freeAccess,
    );
  }

  async computeLog(since?: string, limit?: number, userEmail?: string): Promise<ComputeLogEntry[]> {
    return this.#ledger.call('computeLog', { since, limit, userEmail });
  }

  async setHotWalletAddress(address: string): Promise<boolean> {
    return this.#settlement.call('linkWallet', { address });
  }

  async requestPayment(amountHOT: string): Promise<PaymentRequestResult> {
    return this.#settlement.call('requestPayment', { amountHOT });
  }
}
