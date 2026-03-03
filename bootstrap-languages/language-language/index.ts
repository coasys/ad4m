import type { Address, Language, LanguageContext, Interaction } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import LangAdapter from "./languageAdapter.ts";
import Adapter from "./adapter.ts";

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
