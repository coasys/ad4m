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
import type { Live } from "../mirror/Mirror";
import { AGENT_KIND } from "./AgentKind";
import { LinkMutations, LinkExpression, LinkInput, linkEqual } from "../links/Links";
import { VerificationRequestResult } from "../runtime/RuntimeTypes";
import type { KeyedCache, StoredEntry } from "../cache/KeyedCache";
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

/** Most DIDs the executor takes in one `agent.byDIDs` call. */
const MAX_DIDS_PER_CALL = 200;

export class AgentClient {
  #apiClient: ApiClient;

  /**
   * How long another agent's profile stays fresh (ms). `agent-updated` refreshes it
   * early, but only comes from this executor, so a profile changed through another
   * executor shows up when the entry expires.
   */
  static REMOTE_AGENT_TTL_MS = 5 * 60_000; // 5 minutes

  constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
    this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    this.#apiClient.mirror.actingDid = () => this.did();
  }

  /**
   * The agent itself. One value is kept per client and followed: `agent-updated` for the
   * own DID and the results of {@link updatePublicPerspective} and
   * {@link updateDirectMessageLanguage} replace it. The returned object is shared: do not change it.
   */
  async me(): Promise<Agent> {
    return this.#meEntry().read();
  }

  /** The one `agent.get` entry of this client's mirror. */
  #meEntry() {
    this.#listen();
    return this.#apiClient.mirror.entry<Agent>({
      scope: 'agent',
      kind: 'pushed',
      method: 'agent.get',
      params: {},
      load: async () => toAgent(await this.#apiClient.call('agent.get', {})) as Agent,
    });
  }

  /**
   * The profiles of other agents: the mirror's `declared` cache of `agent.byDIDs`. Concurrent
   * misses go out in one call; stored profiles are shown stale and read once per session.
   */
  #agents(): KeyedCache<Agent> {
    return this.#apiClient.mirror.cache<Agent>({
      method: 'agent.byDIDs',
      kind: 'declared',
      options: () => ({
        loadMany: (dids) => this.#loadAgents(dids),
        maxBatch: MAX_DIDS_PER_CALL,
        persistent: this.#apiClient.mirror.persistent<StoredEntry<Agent>>('ad4m-agent-cache', 'agents'),
        persistentStale: true,
        maxAgeMs: () => AgentClient.REMOTE_AGENT_TTL_MS,
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
    const did = (await this.status()).did || null;
    if (!did) this.#statusEntry().invalidate();
    return did;
  }

  /** Views on the client's mirror entries: subscribe for changes, or read. */
  readonly live = {
    /** The agent's status; {@link status} reads it. */
    status: (): Live<AgentStatus> => this.#statusEntry(),
    /** The agent itself; {@link me} reads it. */
    me: (): Live<Agent> => this.#meEntry(),
    /** The hosting account of this client's user; {@link hostingUserInfo} reads it. */
    hostingUserInfo: (): Live<HostingUserInfo> => this.#hostingEntry(),
    /** The agent with this DID, `null` while unknown; {@link byDID} reads it. */
    byDID: (did: string): Live<Agent | null> => {
      this.#listen();
      return this.#agents().live(did);
    },
  };

  /** The one `agent.status` entry of this client's mirror; it hears `agent-status-changed`. */
  #statusEntry() {
    this.#apiClient.on('agent-status-changed', this.#onStatusChanged);
    return this.#apiClient.mirror.entry<AgentStatus>({
      scope: 'agent',
      kind: 'pushed',
      method: 'agent.status',
      params: {},
      load: async () => new AgentStatus(await this.#apiClient.call('agent.status', {})),
    });
  }

  #onStatusChanged = (event: { agent: object }): void => {
    this.#statusEntry().push(new AgentStatus(event.agent));
  };

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
    // agent-updated events refresh cached entries before their TTL runs out.
    this.#listen();
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

  /** Keeps an agent the executor just gave: its profile entry, and the own entry when it is the own agent. */
  #cacheAgent(agent: Agent | null, own = false): void {
    if (!agent?.did) return;
    this.#agents().set(agent.did, agent);
    this.#listen();
    if (own || agent.did === this.#ownDid()) this.#meEntry().push(agent);
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
    const agent = toAgent(await this.#apiClient.call('agent.updateProfile', { publicPerspective }));

    // Read-your-writes: me() and byDID() answer with the result without waiting for the agent-updated event.
    this.#cacheAgent(agent, true);

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
    const agent = toAgent(await this.#apiClient.call('agent.updateProfile', { dmLanguage: directMessageLanguage }));

    this.#cacheAgent(agent, true);

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

  /** The one `hosting.info` entry of this client's mirror; it hears `hosting-user-info-changed`. */
  #hostingEntry() {
    this.#apiClient.on('hosting-user-info-changed', this.#onHostingChanged);
    return this.#apiClient.mirror.entry<HostingUserInfo>({
      scope: 'agent',
      kind: 'pushed',
      method: 'hosting.info',
      params: {},
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
    });
  }

  /**
   * An operator's client hears the events of every user, so only an event for the user
   * this entry holds replaces it; any other event says the entry may be old.
   */
  #onHostingChanged = (event: HostingUserInfoData): void => {
    const entry = this.#hostingEntry();
    if (entry.getSnapshot().value?.email === event.email) {
      entry.push(new HostingUserInfo(event.email, event.remainingCredits, event.hotWalletAddress || undefined, event.freeAccess));
    } else {
      entry.invalidate();
    }
  };

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
