import { Link, LinkExpression, LinkQuery } from "@perspect3vism/ad4m";
import { buildAd4mClient } from "../client.js";

// * Link Language
export async function addLink(link: Link) {
  const { perspective } = global;

  const client = await buildAd4mClient();

  const response = await client.perspective.addLink(perspective, link);

  return response;
}

export async function removeLink(link: LinkExpression) {
  const { perspective } = global;
  
  const client = await buildAd4mClient();

  const response = await client.perspective.removeLink(perspective, link);

  return response;
}

export async function updateLink(oldLink: LinkExpression, newLink: Link) {
  const { perspective } = global;
  
  const client = await buildAd4mClient();

  const response = await client.perspective.updateLink(perspective, oldLink, newLink);

  return response;
}

export async function queryLinks(query: LinkQuery) {
  const { perspective } = global;
  
  const client = await buildAd4mClient();

  const response = await client.perspective.queryLinks(perspective, query);

  return response;
}

export async function addCallback() {}

// * Expression Language
export async function createExpression(content: any) {
  const client = await buildAd4mClient();

  const response = await client.expression.create(content, languageAddress);

  return response;
}

export async function getExpression(url: string) {
  const client = await buildAd4mClient();

  const response = await client.expression.get(url);
  return response;
}

export async function getByAuthor() {}

// * Direct Message Language
export async function sendPrivate() {}

export async function inbox() {}