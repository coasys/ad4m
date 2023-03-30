import type { Address, Interaction, Agent, Language, LanguageContext, HolochainLanguageDelegate } from "@perspect3vism/ad4m";
import Adapter from './adapter'
import { FileStorageUI } from './noteExpressionUI'
import { DNA, DNA_NICK } from "./dna";

function interactions(expression: Address): Interaction[] {
    return []
}

function isImmutableExpression(expression: Address): boolean {
    return true
}

//@ad4m-template-variable
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