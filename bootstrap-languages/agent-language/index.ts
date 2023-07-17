import type { Address, Language, LanguageContext, HolochainLanguageDelegate, Interaction } from "https://esm.sh/@perspect3vism/ad4m@0.5.0";
import ExpressionAdapter from "./adapter.ts";
import Icon from "./build/Icon.js";
import ConstructorIcon from "./build/ConstructorIcon.js";
import { UI } from "./build/expressionUI.js";
import { DNA, DNA_NICK } from "./build/dna.js";

function iconFor(expression: Address): string {
  return Icon as unknown as string;
}

function constructorIcon(): string {
  return ConstructorIcon as unknown as string;
}

function interactions(expression: Address): Interaction[] {
  return [];
}

//!@ad4m-template-variable
export const name = "agent-expression-store";

export default async function create(context: LanguageContext): Promise<Language> {
  const Holochain = context.Holochain as HolochainLanguageDelegate;
  await Holochain.registerDNAs(
    //@ts-ignore
    [{ file: DNA, nick: DNA_NICK, zomeCalls:
      [
        ["agent_store", "create_agent_expression"],
        ["agent_store", "get_agent_expression"]
      ] 
    }], 
  );

  const expressionAdapter = new ExpressionAdapter(context);
  const expressionUI = new UI();

  return {
    name,
    expressionAdapter,
    iconFor,
    constructorIcon,
    interactions,
    expressionUI,
  } as Language;
}
