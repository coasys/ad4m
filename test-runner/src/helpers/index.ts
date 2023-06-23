import { Ad4mClient, Link, LinkExpression, LinkQuery, PerspectiveState } from "@perspect3vism/ad4m";
import getPort from "get-port";
import { startServer } from "../cli.js";
import { buildAd4mClient } from "../client.js";

class AgentLinkClass {
  client: Ad4mClient
  languageAddress: string
  perspective: string
  neighbourhood: string

  constructor(client: Ad4mClient, languageAddress: string, perspective: string, neighbourhood: string) {
      this.client = client;
      this.languageAddress = languageAddress
      this.neighbourhood = neighbourhood
      this.perspective = perspective
  }

  async waitAgent() {
    return await this.client.neighbourhood.otherAgents(this.perspective)
  }

  async addLink(link: Link) {
    const response = await this.client.perspective.addLink(this.perspective, link);

    await sleep(10000)
  
    return response;
  }

  async removeLink(link: LinkExpression) {
    const response = await this.client.perspective.removeLink(this.perspective, link);

    await sleep(10000)
  
    return response;
  }

  async updateLink(oldLink: LinkExpression, newLink: Link) {
    const response = await this.client.perspective.updateLink(this.perspective, oldLink, newLink);

    await sleep(10000)
  
    return response;
  }

  async queryLinks(query: LinkQuery) {
    const response = await this.client.perspective.queryLinks(this.perspective, query);
  
    return response;
  }

  async waitForSync() {
    await new Promise(async (resolve, reject) => {
      const syncCallback = (state: PerspectiveState) => {
        if (state === PerspectiveState.Synced) {
          resolve(null)
        };
        return null;
      };

      const proxy = await this.client.perspective.byUUID(this.perspective);
      if (proxy?.state === PerspectiveState.Synced) resolve(null);
      proxy?.addSyncStateChangeListener(syncCallback);
    });
  }
}

class AgentExpressionClass {
  client: Ad4mClient
  languageAddress: string

  constructor(client: Ad4mClient, languageAddress: string) {
      this.client = client;
      this.languageAddress = languageAddress
  }

  async create(content: any) {
      const response = await this.client.expression.create(content, this.languageAddress);

      return response;
  }

  async get(url: string) {
      const response = await this.client.expression.get(url);
      return response;
  }
}

export async function spawnExpressionAgent() {
  const { bundle, meta, defaultLangPath } = global.config;

  const port = await getPort();

  if (!global.agents) {
    global.agents = []
  }

  const relativePath = `ad4m-test-${global.agents.length}`;

  const { languageAddress, clear } = await startServer(relativePath, bundle!, meta!, 'expression', port, defaultLangPath);
  
  const client = await buildAd4mClient(port);
  
  const agent = new AgentExpressionClass(client, languageAddress);

  global.agents.push({
      port,
      relativePath: relativePath,
      client: agent,
      languageAddress,
      clear
  })

  return agent;
}

export async function spawnLinkAgent() {
  const { bundle, meta, defaultLangPath } = global.config;

  const port = await getPort();

  if (!global.agents) {
    global.agents = []
  }

  const relativePath = `ad4m-test-${global.agents.length}`;

  let isJoining = global.agents.length > 0;
  let { languageAddress, clear, perspective, neighbourhood } = await startServer(relativePath, bundle!, meta!, 'linkLanguage', port, defaultLangPath);

  const client = await buildAd4mClient(port);

  if (isJoining) {
    const previousAgent = global.agents[global.agents.length - 1];
    const neighbourhood = previousAgent.neighbourhood!;

    console.log(`Agent number ${global.agents.length}, is joining neighbourhood: `, neighbourhood);
  }
  
  const agent = new AgentLinkClass(client, languageAddress, perspective, neighbourhood);

  global.agents.push({
      port,
      relativePath: relativePath,
      client: agent,
      languageAddress,
      perspective, 
      neighbourhood,
      clear
  })

  return agent;
}

export function sleep(ms: number): Promise<any> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}