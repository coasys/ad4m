import type { Address, Interaction, Expression, Language, LanguageContext, PublicSharing } from "@perspect3vism/ad4m";

export const name: string = "test-language"

export default function create(context: LanguageContext): Language {
    const expressions = new Array<Expression>()

    function interactions(expressionAddress: Address): Interaction[] {
        return [{
            label: 'Modify an expression',
            name: 'modify',
            parameters: [{name: 'newValue', type: 'object'}],
            execute: async (parameters: object) => {
                const addr = parseInt(expressionAddress)
                if(addr > expressions.length) return "Non-existant expression"

                //@ts-ignore
                const content = parameters['newValue']
                const expr = context.agent.createSignedExpression(content)
                expressions[addr] = expr

                return "ok"
            }
        }]
    }

    return {
        name,
        interactions,
        expressionAdapter: {
            get: async (address: Address) => expressions[parseInt(address)],
            putAdapter: {
                createPublic: async (content: object): Promise<Address> => {
                    const expr = context.agent.createSignedExpression(content)
                    const addr = expressions.length
                    expressions[addr] = expr
                    return addr.toString()
                }
            }
        }
    } as Language
}

