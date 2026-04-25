// REST client wrapper using native fetch

export interface LinkExpression {
  author: string
  timestamp: string
  data: {
    source: string
    predicate: string
    target: string
  }
  proof: { valid: boolean; key: string; signature: string }
}

export interface ExecutorEndpoint {
  label: string
  url: string
  adminCredential: string
}

export class RestBenchClient {
  endpoint: ExecutorEndpoint
  timeoutMs: number
  constructor(endpoint: ExecutorEndpoint, timeoutMs = 30_000) {
    this.endpoint = endpoint
    this.timeoutMs = timeoutMs
  }

  private get headers(): Record<string, string> {
    return {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${this.endpoint.adminCredential}`,
    }
  }

  private async request<T = unknown>(method: string, path: string, body?: unknown): Promise<T> {
    const resp = await fetch(`${this.endpoint.url}${path}`, {
      method,
      headers: this.headers,
      body: body != null ? JSON.stringify(body) : undefined,
      signal: AbortSignal.timeout(this.timeoutMs),
    })
    if (!resp.ok) {
      throw new Error(`REST request failed (${resp.status}): ${await resp.text()}`)
    }
    return resp.json() as Promise<T>
  }

  async addPerspective(name: string): Promise<string> {
    const data = await this.request<{ uuid: string }>('POST', '/api/v1/perspectives', { name })
    return data.uuid
  }

  async removePerspective(uuid: string): Promise<void> {
    await this.request('DELETE', `/api/v1/perspectives/${encodeURIComponent(uuid)}`)
  }

  async addLink(uuid: string, source: string, predicate: string, target: string): Promise<LinkExpression> {
    return this.request<LinkExpression>(
      'POST',
      `/api/v1/perspectives/${encodeURIComponent(uuid)}/links`,
      { source, predicate, target }
    )
  }

  async removeLink(uuid: string, link: LinkExpression): Promise<void> {
    await this.request(
      'POST',
      `/api/v1/perspectives/${encodeURIComponent(uuid)}/links/remove`,
      { author: link.author, timestamp: link.timestamp, data: link.data, proof: link.proof }
    )
  }

  async queryLinks(uuid: string, query: Record<string, string>): Promise<LinkExpression[]> {
    return this.request<LinkExpression[]>(
      'POST',
      `/api/v1/perspectives/${encodeURIComponent(uuid)}/links/query`,
      query
    )
  }

  async querySparql(uuid: string, queryStr: string): Promise<string> {
    return this.request<string>(
      'POST',
      `/api/v1/perspectives/${encodeURIComponent(uuid)}/sparql`,
      { query: queryStr }
    )
  }

  async queryProlog(uuid: string, queryStr: string): Promise<string> {
    return this.request<string>(
      'POST',
      `/api/v1/perspectives/${encodeURIComponent(uuid)}/prolog-query`,
      { query: queryStr }
    )
  }

  async agentStatus(): Promise<{ isInitialized: boolean; isUnlocked: boolean; did: string }> {
    return this.request('GET', '/api/v1/agent/status')
  }

  async agentGenerate(passphrase: string): Promise<string> {
    const data = await this.request<{ did: string }>('POST', '/api/v1/agent/generate', { passphrase })
    return data.did
  }

  async agentUnlock(passphrase: string): Promise<string> {
    const data = await this.request<{ did: string }>('POST', '/api/v1/agent/unlock', { passphrase })
    return data.did
  }

  async healthCheck(): Promise<boolean> {
    try {
      await this.agentStatus()
      return true
    } catch {
      return false
    }
  }
}

/** @deprecated Use RestBenchClient instead */
export const GraphQLClient = RestBenchClient
