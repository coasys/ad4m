import type { Address, Language, LanguageContext, Interaction } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import LangAdapter from "./languageAdapter.ts";
import Adapter from "./adapter.ts";

export const name = "languages";

function interactions(expression: Address): Interaction[] {
  return [];
}

export default async function create(context: LanguageContext): Promise<Language> {
  const Holochain = context.Holochain as HolochainLanguageDelegate;
  // @ts-ignore
  await Holochain.registerDNAs([{ file: DNA, nick: DNA_NICK }]);

  const expressionAdapter = new Adapter(context);
  const languageAdapter = new LangAdapter(context);

  return {
    name,
    expressionAdapter,
    languageAdapter,
    interactions,
  } as Language;
}
