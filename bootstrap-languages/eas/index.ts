import type { Address, Language, LanguageContext, Interaction, ExpressionUI } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import ExpressionAdapterImpl from "./adapter.ts";

function iconFor(expression: Address): string {
  return "";
}

function constructorIcon(): string {
  return "";
}

function interactions(expression: Address): Interaction[] {
  return [];
}

export class UI implements ExpressionUI {
  icon(): string {
      return ""
  }

  constructorIcon(): string {
      return ""
  }
}

//!@ad4m-template-variable
export const name = "aes-language";

export default async function create(context: LanguageContext): Promise<Language> {
  const expressionAdapter = new ExpressionAdapterImpl(context);
  const isImmutableExpression = (expression: Address): boolean => {
    return false;
  };
  const expressionUI = new UI();

  return {
    name,
    iconFor,
    constructorIcon,
    isImmutableExpression,
    expressionAdapter,
    interactions,
    expressionUI
  } as Language;
}
