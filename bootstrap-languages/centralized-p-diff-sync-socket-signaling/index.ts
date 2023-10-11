import type { Address, Language, Interaction, HolochainLanguageDelegate, LanguageContext, AgentService } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import { LinkAdapter } from "./linksAdapter.ts";
import { TelepresenceAdapterImplementation } from "./telepresenceAdapter.ts";

function interactions(expression: Address): Interaction[] {
  return [];
}

//!@ad4m-template-variable
const name = "centralized-perspective-diff-sync";

//!@ad4m-template-variable
const uid = "centralized-perspective-diff-sync-uuid";

export default async function create(context: LanguageContext): Promise<Language> {
  const linksAdapter = new LinkAdapter(context, uid);
  const telepresenceAdapter = new TelepresenceAdapterImplementation(context);

  //@ts-ignore
  return {
    name,
    linksAdapter,
    interactions,
    telepresenceAdapter
  } as Language;
}
