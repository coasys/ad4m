import type { Address, Language, HolochainLanguageDelegate, LanguageContext, Interaction, ExpressionUI } from "@perspect3vism/ad4m";
import LangAdapter from "./languageAdapter";
import Adapter from "./adapter";

export const name = "languages";

export const PROXY_URL = "https://bootstrap-store-gateway.perspect3vism.workers.dev";

function interactions(expression: Address): Interaction[] {
  return [];
}

export default async function create(context: LanguageContext): Promise<Language> {
  const expressionAdapter = new Adapter(context);
  const languageAdapter = new LangAdapter(context);

  return {
    name,
    expressionAdapter,
    languageAdapter,
    interactions,
  } as Language;
}
