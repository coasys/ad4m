import type { Address, Expression, ExpressionAdapter, PublicSharing, LanguageContext, HolochainLanguageDelegate } from "@perspect3vism/ad4m";
import { LanguageStoragePutAdapter } from "./putAdapter";
import { DNA_NICK } from "./dna";
import { LanguageStorage } from "./languageStorage";
import type { LanguageExpression } from "./types";

export default class Adapter implements ExpressionAdapter {
  putAdapter: PublicSharing;
  #DNA: HolochainLanguageDelegate;

  constructor(context: LanguageContext) {
    this.putAdapter = new LanguageStoragePutAdapter(context);
    this.#DNA = context.Holochain as HolochainLanguageDelegate;
  }

  async get(address: Address): Promise<Expression | null> {
    const storage = new LanguageStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "language_storage", fn_name, payload));

    let addressBuffer = Buffer.from(address, 'hex');
    const expression = (await storage.getLanguageExpression(addressBuffer)) as LanguageExpression

    if (!expression) {
      return null;
    };

    return expression
  }
}
