import type { Address, Language, LanguageContext, ExpressionUI, Interaction, HolochainLanguageDelegate } from "@perspect3vism/ad4m";
import Adapter from "./adapter";
import { DNA, DNA_NICK } from "./dna";

function interactions(expression: Address): Interaction[] {
  return [];
}

export class UI implements ExpressionUI {
  icon(): string {
    return "";
  }

  constructorIcon(): string {
    return "";
  }
}

export const name = "neighbourhood-store";

export default async function create(context: LanguageContext): Promise<Language> {
  const Holochain = context.Holochain as HolochainLanguageDelegate;
  // @ts-ignore
  await Holochain.registerDNAs([{ file: DNA, nick: DNA_NICK }]);

  const expressionAdapter = new Adapter(context);
  //const expressionUI = new UI();

  return {
    name,
    expressionAdapter,
    //expressionUI,
    interactions,
  } as Language;
}
