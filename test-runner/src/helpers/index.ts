import { Ad4mClient, Link, LinkExpression, LinkQuery } from "@perspect3vism/ad4m";
import getPort from "get-port";
import { startServer } from "../cli.js";
import { buildAd4mClient } from "../client.js";

// * Link Language
export async function addLink(link: Link) {
  const { perspective } = global;

  const client = await buildAd4mClient(4000);

  const response = await client.perspective.addLink(perspective, link);

  return response;
}

export async function removeLink(link: LinkExpression) {
  const { perspective } = global;
  
  const client = await buildAd4mClient(4000);

  const response = await client.perspective.removeLink(perspective, link);

  return response;
}

export async function updateLink(oldLink: LinkExpression, newLink: Link) {
  const { perspective } = global;
  
  const client = await buildAd4mClient(4000);

  const response = await client.perspective.updateLink(perspective, oldLink, newLink);

  return response;
}

export async function queryLinks(query: LinkQuery) {
  const { perspective } = global;
  
  const client = await buildAd4mClient(4000);

  const response = await client.perspective.queryLinks(perspective, query);

  return response;
}

export async function addCallback() {}

// * Expression Language
export async function createExpression(content: any) {
  const client = await buildAd4mClient(4000);

  const response = await client.expression.create(content, languageAddress);

  return response;
}

export async function getExpression(url: string) {
  const client = await buildAd4mClient(4000);

  const response = await client.expression.get(url);
  return response;
}

export async function getByAuthor() {}

// * Direct Message Language
export async function sendPrivate() {}

export async function inbox() {}

class AgentLinkClass {
  constructor() {}

  async addLink() {}

  async removeLink() {}

  async updateLink() {}

  async querylink() {}
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
      languageAddress
  })

  return agent;
}

function spawnLinkAgent() {
  const agent = new AgentLinkClass()

  return agent;
}