// GraphQL client wrapper using native fetch

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

export class GraphQLClient {
  endpoint: ExecutorEndpoint
  timeoutMs: number
  constructor(endpoint: ExecutorEndpoint, timeoutMs = 30_000) {
    this.endpoint = endpoint
    this.timeoutMs = timeoutMs
  }

  private get headers(): Record<string, string> {
    return {
      'Content-Type': 'application/json',
      'Authorization': this.endpoint.adminCredential,
    }
  }

  async query<T = unknown>(gql: string, variables?: Record<string, unknown>): Promise<T> {
    const resp = await fetch(this.endpoint.url, {
      method: 'POST',
      headers: this.headers,
      body: JSON.stringify({ query: gql, variables }),
      signal: AbortSignal.timeout(this.timeoutMs),
    })
    if (!resp.ok) {
      throw new Error(`GraphQL request failed (${resp.status}): ${await resp.text()}`)
    }
    const json = await resp.json() as { data?: T; errors?: Array<{ message: string }> }
    if (json.errors?.length) {
      throw new Error(`GraphQL errors: ${json.errors.map(e => e.message).join('; ')}`)
    }
    return json.data as T
  }

  async addPerspective(name: string): Promise<string> {
    const data = await this.query<{ perspectiveAdd: { uuid: string } }>(
      `mutation($name: String!) { perspectiveAdd(name: $name) { uuid } }`,
      { name }
    )
    return data.perspectiveAdd.uuid
  }

  async removePerspective(uuid: string): Promise<void> {
    await this.query(
      `mutation($uuid: String!) { perspectiveRemove(uuid: $uuid) }`,
      { uuid }
    )
  }

  async addLink(uuid: string, source: string, predicate: string, target: string): Promise<LinkExpression> {
    const data = await this.query<{ perspectiveAddLink: LinkExpression }>(
      `mutation($uuid: String!, $link: LinkInput!) {
        perspectiveAddLink(uuid: $uuid, link: $link) {
          author timestamp
          data { source predicate target }
          proof { valid key signature }
        }
      }`,
      { uuid, link: { source, predicate, target } }
    )
    return data.perspectiveAddLink
  }

  async addLinks(uuid: string, links: Array<{ source: string; predicate: string; target: string }>): Promise<void> {
    const CHUNK = 500
    for (let i = 0; i < links.length; i += CHUNK) {
      const batch = links.slice(i, i + CHUNK)
      await this.query(
        `mutation($uuid: String!, $links: [LinkInput!]!) {
          perspectiveAddLinks(uuid: $uuid, links: $links) { author }
        }`,
        { uuid, links: batch }
      )
    }
  }

  async removeLink(uuid: string, link: LinkExpression): Promise<void> {
    await this.query(
      `mutation($uuid: String!, $link: LinkExpressionInput!) {
        perspectiveRemoveLink(uuid: $uuid, link: $link)
      }`,
      { uuid, link: { author: link.author, timestamp: link.timestamp, data: link.data, proof: link.proof } }
    )
  }

  async queryLinks(uuid: string, query: Record<string, string>): Promise<LinkExpression[]> {
    const data = await this.query<{ perspectiveQueryLinks: LinkExpression[] }>(
      `query($uuid: String!, $query: LinkQuery!) {
        perspectiveQueryLinks(uuid: $uuid, query: $query) {
          author timestamp
          data { source predicate target }
        }
      }`,
      { uuid, query }
    )
    return data.perspectiveQueryLinks
  }

  async querySparql(uuid: string, queryStr: string): Promise<string> {
    const data = await this.query<{ perspectiveQuerySparql: string }>(
      `query($uuid: String!, $query: String!) {
        perspectiveQuerySparql(uuid: $uuid, query: $query)
      }`,
      { uuid, query: queryStr }
    )
    return data.perspectiveQuerySparql
  }

  async queryProlog(uuid: string, queryStr: string): Promise<string> {
    const data = await this.query<{ perspectiveQueryProlog: string }>(
      `query($uuid: String!, $query: String!) {
        perspectiveQueryProlog(uuid: $uuid, query: $query)
      }`,
      { uuid, query: queryStr }
    )
    return data.perspectiveQueryProlog
  }

  async agentStatus(): Promise<{ isInitialized: boolean; isUnlocked: boolean; did: string }> {
    const data = await this.query<{ agentStatus: { isInitialized: boolean; isUnlocked: boolean; did: string } }>(
      `query { agentStatus { isInitialized isUnlocked did } }`
    )
    return data.agentStatus
  }

  async agentGenerate(passphrase: string): Promise<string> {
    const data = await this.query<{ agentGenerate: { did: string } }>(
      `mutation($passphrase: String!) { agentGenerate(passphrase: $passphrase) { did } }`,
      { passphrase }
    )
    return data.agentGenerate.did
  }

  async agentUnlock(passphrase: string): Promise<string> {
    const data = await this.query<{ agentUnlock: { did: string } }>(
      `mutation($passphrase: String!) { agentUnlock(passphrase: $passphrase) { did } }`,
      { passphrase }
    )
    return data.agentUnlock.did
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
