import type { Address, Interaction, Agent, Language, LanguageContext } from "@perspect3vism/ad4m";
import Adapter from './adapter'
import { NoteExpressionUI } from './noteExpressionUI'

function interactions(expression: Address): Interaction[] {
    return []
}

function isImmutableExpression(expression: Address): boolean {
    return true
}

export default function create(context: LanguageContext): Language {
    const expressionAdapter = new Adapter(context)
    const expressionUI = new NoteExpressionUI()

    return {
        name: 'note-ipfs',
        expressionAdapter,
        expressionUI,
        interactions,
        isImmutableExpression
    } as Language
}

export const name: string = "note-ipfs"