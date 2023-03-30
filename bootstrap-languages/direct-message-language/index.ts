import type { Address, Language, LanguageContext, Interaction } from "@perspect3vism/ad4m";
import DMAdapter from "./adapter";

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
