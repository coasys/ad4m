import type { Address, Interaction, Language, LanguageContext } from "https://esm.sh/v135/@perspect3vism/ad4m@0.5.0";
import Adapter from './adapter.ts'
import { UI } from "./build/expressionUI.js";

function interactions(expression: Address): Interaction[] {
    return []
}

export default function create(context: LanguageContext): Language {
    const expressionAdapter = new Adapter(context)
    const expressionUI = new UI()

    return {
        name: 'perspective-language',
        expressionAdapter,
        expressionUI,
        interactions,
    } as Language
}

export const name: string = "perspective-language"