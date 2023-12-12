import { Ad4mClient, Link, LinkExpression, LinkQuery } from "@coasys/ad4m";
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

  async addLink(link: Link) {
    const response = await this.client.perspective.addLink(this.perspective, link);
  
    return response;
  }

  async removeLink(link: LinkExpression) {
    const response = await this.client.perspective.removeLink(this.perspective, link);
  
    return response;
  }

  async updateLink(oldLink: LinkExpression, newLink: Link) {
    const response = await this.client.perspective.updateLink(this.perspective, oldLink, newLink);
  
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

  const { languageAddress, clear, perspective, neighbourhood } = await startServer(relativePath, bundle!, meta!, 'linkLanguage', port, defaultLangPath);
  
  const client = await buildAd4mClient(port);
  
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