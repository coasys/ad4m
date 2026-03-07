import type {
  Address,
  Expression,
  ExpressionAdapter,
  ReadOnlyLanguage,
  LanguageContext,
  AgentService,
} from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import { SOA_EXPRESSIONS } from "./expressions.ts";

/**
 * SoA Expression Adapter
 * 
 * Resolves soa:// addresses to structured documentation.
 * This is a ReadOnlyLanguage — expressions are predefined,
 * not created by users.
 * 
 * When an AI agent or application resolves "soa://rel_supports",
 * they get back a rich documentation object explaining what
 * "supports" means, how to use it, and example link triples.
 */
export default class SoAExpressionAdapter implements ExpressionAdapter {
  #agent: AgentService;
  putAdapter: ReadOnlyLanguage;

  constructor(context: LanguageContext) {
    this.#agent = context.agent;
    this.putAdapter = new SoAReadOnly();
  }

  async get(address: Address): Promise<Expression | null> {
    // Strip the soa:// prefix if present
    const key = address.replace(/^soa:\/\//, '');

    const doc = SOA_EXPRESSIONS[key];
    if (!doc) {
      // Return a helpful error expression for unknown addresses
      const knownKeys = Object.keys(SOA_EXPRESSIONS);
      const expression = await this.#agent.createSignedExpression({
        error: `Unknown SoA expression: ${key}`,
        message: `"${key}" is not a recognized SoA expression.`,
        knownExpressions: knownKeys.map(k => `soa://${k}`),
        hint: 'Use one of the known expressions listed above. Properties go on StateOfAffair nodes, relationships link between them.',
      });
      return expression;
    }

    // Return the documentation as a signed expression
    const expression = await this.#agent.createSignedExpression({
      '@type': 'SoADocumentation',
      address: `soa://${key}`,
      ...doc,
    });
    return expression;
  }
}

class SoAReadOnly implements ReadOnlyLanguage {
  async addressOf(content: object): Promise<Address> {
    // For a read-only language, the address is the key itself
    const c = content as Record<string, string>;
    return c.key || c.address || JSON.stringify(content);
  }
}
