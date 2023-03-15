import { Ad4mClient, Link, LinkExpression, LinkQuery } from "@perspect3vism/ad4m";
import getPort from "get-port";
import { startServer } from "../cli.js";
import { buildAd4mClient } from "../client.js";

const retry = (run: any, maxRetires: number) => {
  let currRetry = 0;

  return new Promise((resolve, reject) => {
    const interval = setInterval(async () => {   
      currRetry += 1; 
      const arr = await run();

      if (arr.length > 1) {
        clearInterval(interval)
        resolve(arr)
      }

      if (currRetry === maxRetires) {
        reject("Max Retries exceeded when trying to sync agent.")
      }
    }, 10000);
 })
}

export const waitForAgentsToSync = async (maxRetires = 50) => {
 const promiseClearList = []
 const promiseList = []
 for (const agent of global.agents) {
   const link = await agent.client.addLink({source:"root", predicate: "soic://test", target:"QmYVsrMpiFmV9S7bTWNAkUzSqjRJskQ8g4TWKKwKrHAPqL://QmSsCCtXMDAZXMpyiNLzwjGEU4hLmhG7fphidhEEodQ4Wy"})
   
   promiseClearList.push(link)
 
   promiseList.push(retry(async () => await agent.client.queryLinks({}), maxRetires))
 }

 await Promise.all(promiseList)

 for (let index = 0; index < global.agents.length; index++) {
   const agent = global.agents[index];

   await agent.client.removeLink(promiseClearList[index])
 }
}

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