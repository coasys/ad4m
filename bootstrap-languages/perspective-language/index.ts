import type { Address, Interaction, Agent, Language, LanguageContext } from "@perspect3vism/ad4m";
import Adapter from './adapter'
import { NoteExpressionUI } from './noteExpressionUI'

function interactions(expression: Address): Interaction[] {
    return []
}

export default function create(context: LanguageContext): Language {
    const expressionAdapter = new Adapter(context)
    const expressionUI = new NoteExpressionUI()

    return {
        name: 'perspective-language',
        expressionAdapter,
        expressionUI,
        interactions,
    } as Language
}

export const name: string = "perspective-language"