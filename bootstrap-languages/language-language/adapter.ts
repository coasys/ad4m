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
    //Check the first two characters of address are equal to Qm
    if (address.substring(0, 2) != "Qm") {
      console.error("LanguageLanguage.get(): The address is not a valid hash");
      return null;
    }

    const storage = new LanguageStorage((fn_name, payload) => this.#DNA.call(DNA_NICK, "language-language", fn_name, payload));

    let addressBuffer = Buffer.from(address, 'hex');
    const expression = (await storage.getLanguageExpression(addressBuffer)) as LanguageExpression

    if (!expression) {
      return null;
    };
    if (expression.data.chunks_hashes === 0 || expression.data.chunks_hashes === undefined) {
        expression.data.data_base64 = "";
        return expression;
    };

    return expression
  }
}
