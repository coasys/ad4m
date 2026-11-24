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
import { agentDid, agentStatusEntry } from "./statusEntry";
import type { Live } from "../mirror/Mirror";
import type { Entry } from "../mirror/Entry";
import { AGENT_KIND } from "./AgentKind";
import { LinkMutations, LinkExpression, LinkInput, linkEqual } from "../links/Links";
import { VerificationRequestResult } from "../runtime/RuntimeTypes";
import type { KeyedCache, StoredEntry } from "../cache/KeyedCache";
import type { PersistentCache } from "../cache/PersistentCache";
import type { Agent as AgentData } from "../generated/api/Agent";
import type { HostingUserInfo as HostingUserInfoData } from "../generated/api/HostingUserInfo";
import type { AgentSignature } from "../generated/api/AgentSignature";
import type { AgentWithIdentity } from "../generated/api/AgentWithIdentity";

export interface InitializeArgs {
  did: string;
  didDocument: string;
  keystore: string;
  passphrase: string;
}

function toAgent(data: AgentData | AgentWithIdentity | null): Agent | null {
  return data ? Agent.fromWire(data) : null;
}

/**
 * A store of agents whose reads return `Agent` instances: IndexedDB keeps what structured clone
 * leaves, plain objects, so the perspective's and links' methods are rebuilt on every read.
 */
function revivingAgents(store: PersistentCache<StoredEntry<Agent>>): PersistentCache<StoredEntry<Agent>> {
  return {
    get: async (key) => {
      const entry = await store.get(key);
      return entry?.value ? { ...entry, value: Agent.fromWire(entry.value as unknown as AgentData) } : entry;
    },
    put: (key, value) => store.put(key, value),
    delete: (key) => store.delete(key),
    clear: (prefix) => store.clear(prefix),
  };
}

/**
 * Replaces the own agent an entry holds. `agent.get` gives the identity the device key speaks
 * for; a profile update or an `agent-updated` event carries none, so only a fresh held agent can
 * say it. When the entry is not fresh (a revocation or a loss made it stale, nothing was read yet)
 * the identity is not known: the entry is invalidated and the next read asks.
 */
function pushAgent(entry: Entry<Agent>, agent: Agent): void {
  if (agent.identity === undefined) {
    const { state, value } = entry.getSnapshot();
    if (state !== 'fresh' || !value) return entry.invalidate();
    // Only a known identity: the agent is the caller's result too, which gains no empty key.
    if (value.identity !== undefined) agent.identity = value.identity;
  }
  entry.push(agent);
}

/** Most DIDs the executor takes in one `agent.byDIDs` call. */
const MAX_DIDS_PER_CALL = 200;

export class AgentClient {
  #apiClient: ApiClient;
  #me?: Entry<Agent>;
  #status?: Entry<AgentStatus>;
  #hosting?: Entry<HostingUserInfo>;

  /**
   * How long another agent's profile stays fresh (ms). `agent-updated` refreshes it
   * early, but only comes from this executor, so a profile changed through another
   * executor shows up when the entry expires.
   */
  static REMOTE_AGENT_TTL_MS = 5 * 60_000; // 5 minutes
  /** Most other agents' profiles kept in memory. */
  static MAX_MEM_ENTRIES = 200;
  /** Most bytes (of JSON) of the profiles kept in memory. */
  static MAX_MEM_BYTES = 5 * 1024 * 1024; // 5 MB

  constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
    this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
  }

  /**
   * The agent itself. One value is kept per client and followed: `agent-updated` for the
   * own DID and the results of {@link updatePublicPerspective} and
   * {@link updateDirectMessageLanguage} replace it. The returned object is shared: do not change it.
   */
  async me(): Promise<Agent> {
    return this.#meEntry().read();
  }

  /** The one `agent.get` entry of this client's mirror, made on first use. */
  #meEntry(): Entry<Agent> {
    this.#me ??= this.#apiClient.mirror.entry<Agent>({
      method: 'agent.get',
      load: async () => toAgent(await this.#apiClient.call('agent.get', {})) as Agent,
      on: {
        // The identity the device key speaks for is in the status too: when it moved (a revocation), the held one is old.
        // With none held, a first read may be on its way: invalidating drops that answer (used once, not kept).
        'agent-status-changed': (event, entry) => {
          const held = entry.getSnapshot().value;
          if (!held || (held.identity ?? null) !== (event.agent.identity ?? null)) entry.invalidate();
        },
        'agent-updated': (event, entry) => {
          const own = this.#ownDid();
          // Cold: the event cannot be matched to the own agent yet. Whatever the entry
          // read meanwhile may be older than it, so that answer is used once and not kept.
          if (!own) entry.invalidate();
          else {
            const agent = Agent.fromWire(event.agent);
            if (agent.did === own) pushAgent(entry, agent);
          }
        },
      },
    });
    return this.#me;
  }

  /**
   * The profiles of other agents: the mirror's cache of `agent.byDIDs`, fresh for
   * {@link AgentClient.REMOTE_AGENT_TTL_MS}, replaced at once by `agent-updated`, at most
   * {@link AgentClient.MAX_MEM_ENTRIES} / {@link AgentClient.MAX_MEM_BYTES} in memory.
   * Concurrent misses go out in one call; stored profiles are shown stale and read once per session.
   */
  #agents(): KeyedCache<Agent> {
    return this.#apiClient.mirror.cache<Agent>({
      method: 'agent.byDIDs',
      on: { 'agent-updated': (event, cache) => { const agent = Agent.fromWire(event.agent); if (agent.did) cache.set(agent.did, agent); } },
      options: () => ({
        loadMany: (dids) => this.#loadAgents(dids),
        maxBatch: MAX_DIDS_PER_CALL,
        persistent: revivingAgents(this.#apiClient.mirror.persistent<StoredEntry<Agent>>('ad4m-agent-cache', 'agents')),
        persistentStale: true,
        maxAgeMs: () => AgentClient.REMOTE_AGENT_TTL_MS,
        limits: () => ({ entries: AgentClient.MAX_MEM_ENTRIES, bytes: AgentClient.MAX_MEM_BYTES }),
      }),
    });
  }

  /**
   * The agent's status. One value is kept per client and followed: `agent-status-changed`
   * and the results of {@link generate}, {@link import}, {@link lock} and {@link unlock}
   * replace it, and a reconnect makes the next read ask the executor again. Concurrent
   * reads share one request. The returned object is shared: do not change it.
   */
  async status(): Promise<AgentStatus> {
    return this.#statusEntry().read();
  }

  /**
   * The agent's own DID, read from {@link status}, or `null` while the executor reports none.
   * A status with no DID is not kept: the next call asks again.
   */
  async did(): Promise<string | null> {
    return agentDid(this.#apiClient);
  }

  /** Views on the client's mirror entries: subscribe for changes, or read. */
  readonly live = {
    /** The agent's status; {@link status} reads it. */
    status: (): Live<AgentStatus> => this.#statusEntry(),
    /** The agent itself; {@link me} reads it. */
    me: (): Live<Agent> => this.#meEntry(),
    /** The hosting account of this client's user; {@link hostingUserInfo} reads it. */
    hostingUserInfo: (): Live<HostingUserInfo> => this.#hostingEntry(),
    /**
     * The agent with this DID; {@link byDID} reads it. The value is `undefined` until a read
     * answers. A DID the executor does not know is `fresh` with the value `null` (while the
     * view has a subscriber; the answer is not kept, so a read asks again).
     */
    byDID: (did: string): Live<Agent | null> => this.#agents().live(did),
  };

  #statusEntry(): Entry<AgentStatus> {
    return (this.#status ??= agentStatusEntry(this.#apiClient));
  }

  /** Keeps the status the executor returned from a call that changes it. */
  #keepStatus(wire: object): AgentStatus {
    const status = new AgentStatus(wire);
    this.#statusEntry().push(status);
    return status;
  }

  /** Creates the agent's keys and DID on first run. The executor refuses (409)
   *  an agent that is already initialised — {@link unlock} it instead; a second
   *  `generate` never replaces the existing keys and DID. */
  async generate(passphrase: string, options?: CallOptions): Promise<AgentStatus> {
    const result = await this.#apiClient.call('agent.generate', { passphrase }, options);
    return this.#keepStatus(result);
  }

  async import(args: InitializeArgs): Promise<AgentStatus> {
    const result = await this.#apiClient.call('agent.import', { ...args });
    return this.#keepStatus(result);
  }

  async lock(passphrase: string): Promise<AgentStatus> {
    const result = await this.#apiClient.call('agent.lock', { passphrase });
    return this.#keepStatus(result);
  }

  async unlock(passphrase: string, holochain = true, options?: CallOptions): Promise<AgentStatus> {
    const result = await this.#apiClient.call('agent.unlock', { passphrase, holochain }, options);
    return this.#keepStatus(result);
  }

  /**
   * The agent with this DID, or `null` when it is unknown. Concurrent calls
   * for any DIDs share one `agent.byDIDs` request.
   */
  async byDID(did: string): Promise<Agent> {
    return this.#agents().get(did) as Promise<Agent>;
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

  /**
   * Keeps the agent an own profile write returned: its profile entry and the own entry, unless
   * an event or an invalidation changed the own entry since the write went out (`since` is its
   * version then): what came is newer than the reply, which the executor may have computed earlier.
   */
  #cacheAgent(agent: Agent | null, since: number): void {
    if (!agent?.did || this.#meEntry().version !== since) return;
    this.#agents().set(agent.did, agent);
    pushAgent(this.#meEntry(), agent);
  }

  /** The own DID as far as the mirror knows it, without a request. */
  #ownDid(): string | undefined {
    return this.#statusEntry().getSnapshot().value?.did || this.#meEntry().getSnapshot().value?.did || undefined;
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
    const since = this.#meEntry().version;
    const agent = toAgent(await this.#apiClient.call('agent.updateProfile', { publicPerspective }));

    // Read-your-writes: me() and byDID() answer with the result without waiting for the agent-updated event.
    this.#cacheAgent(agent, since);

    return agent;
  }

  async mutatePublicPerspective({ additions, removals }: LinkMutations): Promise<Agent> {
    const added = additions.length > 0 ? await this.#signLinks(additions) : [];
    const { perspective } = await this.me();
    const kept = (perspective?.links ?? []).filter(link => !removals.some(r => linkEqual(link, r as LinkExpression)));
    return this.updatePublicPerspective({ links: [...kept, ...added] } as PerspectiveInput);
  }

  /**
   * Say what kind of agent this is in its public perspective (`AGENT_KIND`; a bot is
   * `AGENT_KIND_BOT`), replacing any kind it said before, or say none with `null`. Changes
   * nothing when the perspective already says exactly that, so it can be called on every start.
   */
  async setKind(kind: string | null): Promise<Agent> {
    const me = await this.me();
    const said = (me.perspective?.links ?? []).filter(
      (l) => l.data.source === me.did && l.data.predicate === AGENT_KIND,
    );
    if ((kind === null && said.length === 0) || (said.length === 1 && said[0].data.target === kind)) {
      return me;
    }
    return this.mutatePublicPerspective({
      additions: kind === null ? [] : [{ source: me.did, predicate: AGENT_KIND, target: kind } as LinkInput],
      removals: said as LinkMutations['removals'],
    });
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
    const since = this.#meEntry().version;
    const agent = toAgent(await this.#apiClient.call('agent.updateProfile', { dmLanguage: directMessageLanguage }));

    this.#cacheAgent(agent, since);

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

  /**
   * Asks the executor for a capability token. `agent` names the agent the token will act
   * as: a managed user's email or an agent's DID; absent, the executor's main agent. That
   * agent's owner approves the request (the operator for the main agent, the user for
   * theirs). Returns the request id that {@link generateJwt} redeems with the approver's
   * six-digit code.
   */
  async requestCapability(authInfo: AuthInfoInput, agent?: string): Promise<string> {
    return this.#apiClient.call(
      'agent.requestCapability',
      agent === undefined ? { authInfo } : { authInfo, agent },
    );
  }

  async permitCapability(auth: string): Promise<string> {
    return this.#apiClient.call('agent.permitCapability', { auth });
  }

  async generateJwt(requestId: string, rand: string): Promise<string> {
    return this.#apiClient.call('agent.generateJwt', { requestId, rand });
  }

  /**
   * Always one request: not mirrored. `apps-changed` is sent only when a grant is
   * minted, and `removeApp` / `revokeToken` send nothing, so no event says the list is stale.
   */
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

  /** Revokes this client's user token. The executor then closes the client's sockets. */
  async logout(): Promise<boolean> {
    return this.#apiClient.call('user.logout', {});
  }

  /**
   * Revokes every token of a user issued up to now, this client's own included, and closes
   * their sockets. A user leaves out `email` (or names themselves); the operator names the
   * user. Needs AGENT_UPDATE, which a login holds. Tokens from a later login work.
   */
  async revokeAllSessions(email?: string): Promise<boolean> {
    return this.#apiClient.call('user.revokeAllSessions', email === undefined ? {} : { email });
  }

  // Hosting methods
  /**
   * The hosting account of this client's user. One value is kept per client and followed:
   * `hosting-user-info-changed` replaces it, {@link setHotWalletAddress} makes it stale, and a
   * reconnect makes the next read ask the executor again. Concurrent reads share one request.
   * The returned object is shared: do not change it.
   */
  async hostingUserInfo(): Promise<HostingUserInfo> {
    return this.#hostingEntry().read();
  }

  /** The one `hosting.info` entry of this client's mirror, made on first use; it hears `hosting-user-info-changed`. */
  #hostingEntry(): Entry<HostingUserInfo> {
    this.#hosting ??= this.#apiClient.mirror.entry<HostingUserInfo>({
      method: 'hosting.info',
      load: async () => {
        const info = (await this.#apiClient.call('hosting.info', {})).userInfo ?? {
          email: '', credits: null, hotWalletAddress: null, freeAccess: false,
        };
        return new HostingUserInfo(
          info.email,
          info.freeAccess ? 'unlimited' : String(info.credits ?? 0),
          info.hotWalletAddress || undefined,
          !!info.freeAccess,
        );
      },
      // An operator's client hears the events of every user, so only an event for the user
      // this entry holds replaces it; any other event says the entry may be old.
      on: {
        'hosting-user-info-changed': (event: HostingUserInfoData, entry) => {
          if (entry.getSnapshot().value?.email === event.email) {
            entry.push(new HostingUserInfo(event.email, event.remainingCredits, event.hotWalletAddress || undefined, event.freeAccess));
          } else {
            entry.invalidate();
          }
        },
      },
    });
    return this.#hosting;
  }

  async computeLog(since?: string, limit?: number, userEmail?: string): Promise<ComputeLogEntry[]> {
    return this.#apiClient.call('runtime.computeLog', { since, limit, userEmail });
  }

  async setHotWalletAddress(address: string): Promise<boolean> {
    const set = await this.#apiClient.call('hosting.setHotWallet', { address });
    // The executor publishes no event for this write: the held info is old.
    this.#hostingEntry().invalidate();
    return set;
  }

  async requestPayment(amountHOT: string): Promise<PaymentRequestResult> {
    return this.#apiClient.call('hosting.requestPayment', { amountHOT });
  }
}
