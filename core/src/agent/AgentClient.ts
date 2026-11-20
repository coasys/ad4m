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
import { AGENT_KIND } from "./AgentKind";
import { LinkMutations, LinkExpression, LinkInput, linkEqual } from "../links/Links";
import { VerificationRequestResult } from "../runtime/RuntimeTypes";
import { createPersistentCache } from "../cache/PersistentCache";
import { KeyedCache, StoredEntry } from "../cache/KeyedCache";
import type { Agent as AgentData } from "../generated/api/Agent";
import type { AgentSignature } from "../generated/api/AgentSignature";
import type { AgentWithIdentity } from "../generated/api/AgentWithIdentity";
import type { AuthInfoInput as AuthInfoWire } from "../generated/api/AuthInfoInput";
import type { EndedSessions } from "../generated/api/EndedSessions";
import type { KeySessionResult } from "../generated/api/KeySessionResult";
import type { RenewedSession } from "../generated/api/RenewedSession";
import type { SessionChallenge } from "../generated/api/SessionChallenge";
import type { SessionInfo } from "../generated/api/SessionInfo";
import type { SignedKeyStatement } from "../generated/api/SignedKeyStatement";
import type { UnsignedKeyStatement } from "../generated/api/UnsignedKeyStatement";

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

  // byDID cache: concurrent misses go out in one `agent.byDIDs` call.
  #agents: KeyedCache<Agent>;
  /**
   * TTL of every cached profile, the own one too, in memory and in the
   * persistent cache (ms). `agent-updated` only comes from this executor, so
   * a profile changed through another executor of the same agent shows up
   * when the entry expires.
   */
  static REMOTE_AGENT_TTL_MS = 5 * 60_000; // 5 minutes

  constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
    this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    this.#agents = new KeyedCache<Agent>({
      loadMany: (dids) => this.#loadAgents(dids),
      maxBatch: MAX_DIDS_PER_CALL,
      persistent: createPersistentCache<StoredEntry<Agent>>('ad4m-agent-cache', 'agents'),
      maxAgeMs: () => AgentClient.REMOTE_AGENT_TTL_MS,
      persistentMaxAgeMs: () => AgentClient.REMOTE_AGENT_TTL_MS,
    });
  }

  async me(): Promise<Agent> {
    return toAgent(await this.#apiClient.call('agent.get', {}));
  }

  async status(): Promise<AgentStatus> {
    const agentStatus = await this.#apiClient.call('agent.status', {});
    return new AgentStatus(agentStatus);
  }

  /** Creates the agent's keys and DID on first run. The executor refuses (409)
   *  an agent that is already initialised — {@link unlock} it instead; a second
   *  `generate` never replaces the existing keys and DID. */
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
    // agent-updated events refresh cached entries before their TTL runs out.
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

  /**
   * Revokes this client's token: a user's login, an app's, or a key session's. The executor
   * then closes the client's sockets. A key session's device key stays valid.
   */
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

  // Key sessions: login with one of the person's device keys

  /**
   * A one-use challenge (60 s) for a key statement, the executor's DID (the statement's
   * `executor`) and the challenge's expiry by the executor's clock. Each address holds at
   * most 100 open challenges (`too-many-challenges` past that). On a key session the
   * challenge is the session's renewal challenge: one per session, so asking again closes the
   * one before.
   */
  async sessionChallenge(): Promise<SessionChallenge> {
    return this.#apiClient.call('user.sessionChallenge', {});
  }

  /**
   * Logs in with a `login` statement that a device key signed under `coasys:session`. The
   * executor gives the operator role to its operator record's identity, or a session on the
   * local agent that speaks for the identity; it refuses with one reason: `challenge`,
   * `audience`, `no-public-origin`, `signature`, `key`, `not-known` or `store-unavailable`.
   * The statement's `audience` is the origin this client dialed (`https://host[:port]`): the
   * executor accepts only its configured `public_origins`, or without them its own loopback
   * origins, so a statement a relaying server obtained for its own origin is refused. The
   * token lives one hour.
   */
  async loginWithKey(proof: SignedKeyStatement, app: AuthInfoWire): Promise<KeySessionResult> {
    return this.#apiClient.call('user.loginWithKey', { proof, app });
  }

  /**
   * Renews this client's key session with a `renew` statement of the same key naming its
   * grant: one more hour, the same grant id. Open sockets stay open. Use the returned token for
   * new connections.
   */
  async renewSession(proof: SignedKeyStatement): Promise<RenewedSession> {
    return this.#apiClient.call('user.renewSession', { proof });
  }

  /** This client's own session: its role, agent, identity, grant and expiry. */
  async currentSession(): Promise<SessionInfo> {
    return this.#apiClient.call('user.currentSession', {});
  }

  /**
   * Log out everywhere on this executor: ends every session of this client's identity (and,
   * on a managed agent, every session of that agent). `keepCurrent` keeps this client's own;
   * `before` (Unix seconds) ends only sessions opened before it.
   */
  async endAllSessions(keepCurrent: boolean, before?: number): Promise<EndedSessions> {
    return this.#apiClient.call(
      'user.endAllSessions',
      before === undefined ? { keepCurrent } : { keepCurrent, before },
    );
  }

  /**
   * Signs a native key statement with this executor's own device key, as the identity it
   * speaks for. Local only: it answers only the operator on this machine, that is the admin
   * credential, or no token on an executor without one, on a loopback listener. Every other caller
   * gets 403 `local-only`, the admin credential on a network listener and an operator from a key
   * login included. Its caller is the shell on the machine that signs a login to another executor.
   */
  async signSessionStatement(statement: UnsignedKeyStatement): Promise<SignedKeyStatement> {
    return this.#apiClient.call('agent.signSessionStatement', { statement });
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
