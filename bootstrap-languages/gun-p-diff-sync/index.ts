import type { Address, Language, Interaction, LanguageContext, AgentService } from "@perspect3vism/ad4m";
import { LinkAdapter } from "./linksAdapter";
//import { TelepresenceAdapterImplementation } from "./telepresenceAdapter";

function interactions(expression: Address): Interaction[] {
  return [];
}

//@ad4m-template-variable
const name = "gun-perspective-diff-sync";

export { name };

export default async function create(context: LanguageContext): Promise<Language> {
  const linksAdapter = new LinkAdapter(context);
  //const telepresenceAdapter = new TelepresenceAdapterImplementation(context);

  //@ts-ignore
  return {
    name,
    linksAdapter,
    interactions,
    //telepresenceAdapter
  } as Language;
}
