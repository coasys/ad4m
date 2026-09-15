import type {
  Address,
  Language,
  LanguageContext,
  Interaction,
  ExpressionUI,
} from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import SoAExpressionAdapter from "./adapter.ts";

function iconFor(_expression: Address): string {
  return "";
}

function constructorIcon(): string {
  return "";
}

function interactions(_expression: Address): Interaction[] {
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

//!@ad4m-template-variable
export const name = "soa-language";

/**
 * SoA Language — a static, self-documenting vocabulary for
 * State of Affairs ontology.
 * 
 * This language defines the semantic predicates used in SoA trees:
 * - Properties: title, modality, description, confidence, status, tags, priority, source
 * - Relationships: rel_supports, rel_contradicts, rel_similar, rel_same,
 *                  rel_requires, rel_enables, rel_parent, rel_refines, rel_blocks
 * 
 * Every soa:// expression resolves to structured documentation
 * that helps AI agents and humans understand the ontology.
 * 
 * This is a ReadOnlyLanguage — no new expressions can be created.
 * The vocabulary is fixed and versioned with AD4M.
 * 
 * Pattern: This serves as an example for how AD4M languages can
 * act as self-documenting semantic vocabularies. Any domain can
 * create a similar language to define its own predicates.
 */
export default async function create(
  context: LanguageContext
): Promise<Language> {
  const expressionAdapter = new SoAExpressionAdapter(context);
  const expressionUI = new UI();

  // All expressions are immutable (static documentation)
  const isImmutableExpression = (_expression: Address): boolean => {
    return true;
  };

  const language = {
    name,
    expressionAdapter,
    expressionUI,
    iconFor,
    constructorIcon,
    isImmutableExpression,
    interactions,
  } satisfies Language;
  return language;
}
