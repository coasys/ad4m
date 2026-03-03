import type { Address, Language, LanguageContext, Interaction } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import DMAdapter from "./adapter.ts";

export const name = "direct-message-language";

function interactions(expression: Address): Interaction[] {
  return [];
}

export default async function create(context: LanguageContext): Promise<Language> {
  const directMessageAdapter = new DMAdapter(context);
  await directMessageAdapter.init()

  return {
    name,
    directMessageAdapter,
    interactions,
  } as Language;
}
