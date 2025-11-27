import type { Address, Interaction, Language, LanguageContext, HolochainLanguageDelegate } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import Adapter from './adapter.ts'
import { FileStorageUI } from "./build/expressionUI.js";
import { DNA, DNA_NICK } from "./build/dna.js";

function interactions(expression: Address): Interaction[] {
    return []
}

function isImmutableExpression(expression: Address): boolean {
    return true
}

//!@ad4m-template-variable
const name = "file-storage";

export default async function create(context: LanguageContext): Promise<Language> {
    const Holochain = context.Holochain as HolochainLanguageDelegate;
    // @ts-ignore
    await Holochain.registerDNAs([{ file: DNA, nick: DNA_NICK }]);

    const expressionAdapter = new Adapter(context)
    const expressionUI = new FileStorageUI()

    return {
        name,
        expressionAdapter,
        expressionUI,
        interactions,
        isImmutableExpression
    } as Language
}