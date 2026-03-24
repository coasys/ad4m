// lib/api.ts — Transport-agnostic executor API client (GraphQL now, REST-ready)

import type { RetryOptions } from './types.js';
import { retryWithBackoff } from './retry.js';

export interface APIResponse {
  data?: Record<string, unknown>;
  errors?: Array<{ message: string; path?: string[] }>;
}

export class ExecutorAPI {
  private endpoint: string;
  private auth?: string;

  constructor(endpoint: string, auth?: string) {
    this.endpoint = endpoint;
    this.auth = auth;
  }

  /** Set auth token (JWT or admin credential) */
  setAuth(auth: string): void {
    this.auth = auth;
  }

  /** Raw GraphQL query */
  async query(operation: string, variables?: Record<string, unknown>): Promise<APIResponse> {
    const headers: Record<string, string> = { 'Content-Type': 'application/json' };
    if (this.auth) headers['Authorization'] = this.auth;

    const response = await fetch(this.endpoint, {
      method: 'POST',
      headers,
      body: JSON.stringify({ query: operation, variables: variables ?? {} }),
    });

    if (!response.ok) {
      throw new Error(`API request failed: ${response.status} ${response.statusText}`);
    }

    return (await response.json()) as APIResponse;
  }

  /** Raw GraphQL mutation (alias for query — same transport) */
  async mutate(operation: string, variables?: Record<string, unknown>): Promise<APIResponse> {
    return this.query(operation, variables);
  }

  /** Wait for the executor GraphQL endpoint to be reachable */
  async waitForReady(timeoutMs: number = 30000): Promise<boolean> {
    const opts: RetryOptions = { timeoutMs, delayMs: 500, maxDelayMs: 2000, backoffFactor: 1.5 };
    return retryWithBackoff(async () => {
      const res = await this.query('{ agentStatus { isInitialized } }');
      if (res.errors) throw new Error(res.errors[0].message);
      return true;
    }, opts);
  }

  // --- Typed convenience methods ---

  async agentStatus(): Promise<{ isInitialized: boolean; isUnlocked: boolean; did?: string }> {
    const res = await this.query('{ agentStatus { isInitialized isUnlocked did } }');
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, Record<string, unknown>>)['agentStatus'] as { isInitialized: boolean; isUnlocked: boolean; did?: string };
  }

  async agentGenerate(passphrase: string): Promise<{ did: string }> {
    const res = await this.mutate(
      `mutation($passphrase: String!) { agentGenerate(passphrase: $passphrase) { did } }`,
      { passphrase }
    );
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, Record<string, unknown>>)['agentGenerate'] as { did: string };
  }

  async agentUnlock(passphrase: string): Promise<{ isUnlocked: boolean; did: string }> {
    const res = await this.mutate(
      `mutation($passphrase: String!) { agentUnlock(passphrase: $passphrase) { isUnlocked did } }`,
      { passphrase }
    );
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, Record<string, unknown>>)['agentUnlock'] as { isUnlocked: boolean; did: string };
  }

  async requestCapability(appName: string, appDesc: string, appUrl: string): Promise<string> {
    const allCaps = [
      { with: { domain: '*', pointers: ['*'] }, can: ['*'] },
    ];
    const res = await this.mutate(
      `mutation($appName: String!, $appDesc: String!, $appUrl: String!, $caps: [CapabilityInput!]) {
        agentRequestCapability(authInfo: { appName: $appName, appDesc: $appDesc, appUrl: $appUrl, appDomain: $appUrl, appIconPath: "", capabilities: $caps })
      }`,
      { appName, appDesc, appUrl, caps: allCaps }
    );
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, unknown>)['agentRequestCapability'] as string;
  }

  async permitCapability(token: string): Promise<string> {
    const res = await this.mutate(
      `mutation($token: String!) { agentPermitCapability(authInfo: $token) }`,
      { token }
    );
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, unknown>)['agentPermitCapability'] as string;
  }

  async generateJwt(requestId: string, rand: string): Promise<string> {
    const res = await this.mutate(
      `mutation($requestId: String!, $rand: String!) { agentGenerateJwt(requestId: $requestId, rand: $rand) }`,
      { requestId, rand }
    );
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, unknown>)['agentGenerateJwt'] as string;
  }

  async perspectiveAdd(name: string): Promise<{ uuid: string }> {
    const res = await this.mutate(
      `mutation($name: String!) { perspectiveAdd(name: $name) { uuid } }`,
      { name }
    );
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, Record<string, unknown>>)['perspectiveAdd'] as { uuid: string };
  }

  async neighbourhoodPublish(perspectiveUuid: string, linkLanguage: string, meta?: { name?: string; description?: string }): Promise<string> {
    const res = await this.mutate(
      `mutation($perspectiveUUID: String!, $linkLanguage: String!, $meta: PerspectiveInput!) {
        neighbourhoodPublishFromPerspective(
          perspectiveUUID: $perspectiveUUID
          linkLanguage: $linkLanguage
          meta: $meta
        )
      }`,
      { perspectiveUUID: perspectiveUuid, linkLanguage, meta: meta ? { links: [] } : { links: [] } }
    );
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, unknown>)['neighbourhoodPublishFromPerspective'] as string;
  }

  async neighbourhoodJoin(url: string): Promise<{ uuid: string }> {
    const res = await this.mutate(
      `mutation($url: String!) { neighbourhoodJoinFromUrl(url: $url) { uuid } }`,
      { url }
    );
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, Record<string, unknown>>)['neighbourhoodJoinFromUrl'] as { uuid: string };
  }

  async runtimeInfo(): Promise<Record<string, unknown>> {
    const res = await this.query('{ runtimeInfo { ad4mExecutorVersion } }');
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, Record<string, unknown>>)['runtimeInfo'];
  }

  async sfuSetConfig(neighbourhoodUrl: string, mode: string, opts?: { designatedPeer?: string; fallback?: string; maxMeshParticipants?: number }): Promise<void> {
    const res = await this.mutate(
      `mutation($nhUrl: String!, $mode: String!, $designatedPeer: String, $fallback: String, $maxMeshParticipants: Int) {
        sfuSetConfig(neighbourhoodUrl: $nhUrl, mode: $mode, designatedPeer: $designatedPeer, fallback: $fallback, maxMeshParticipants: $maxMeshParticipants)
      }`,
      { nhUrl: neighbourhoodUrl, mode, designatedPeer: opts?.designatedPeer ?? null, fallback: opts?.fallback ?? 'mesh', maxMeshParticipants: opts?.maxMeshParticipants ?? 4 }
    );
    if (res.errors) throw new Error(res.errors[0].message);
  }

  async sfuStartRoom(neighbourhoodUrl: string, roomId: string): Promise<unknown> {
    const res = await this.mutate(
      `mutation($nhUrl: String!, $roomId: String!) {
        sfuStartRoom(neighbourhoodUrl: $nhUrl, roomId: $roomId) {
          neighbourhoodUrl
          roomName
          participantCount
        }
      }`,
      { nhUrl: neighbourhoodUrl, roomId }
    );
    if (res.errors) throw new Error(res.errors[0].message);
    return (res.data as Record<string, unknown>)['sfuStartRoom'];
  }
}
