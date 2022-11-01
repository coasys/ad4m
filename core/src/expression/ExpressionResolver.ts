import { Arg, Mutation, Query, Resolver } from "type-graphql";
import { LanguageRef } from "../language/LanguageRef";
import { ExpressionProof, ExpressionRendered } from "./Expression";
import { InteractionCall, InteractionMeta } from "../language/Language"


const e = new ExpressionRendered()
e.author = 'did:ad4m:test'
e.timestamp = Date.now().toString()
e.proof = new ExpressionProof('', '')
e.data = JSON.stringify({ type: 'test expression', content: 'test'})
e.language = new LanguageRef('test-language-address')
const testExpression = e
@Resolver()
export default class ExpressionResolver {
    @Query(returns => ExpressionRendered, {nullable: true})
    expression(@Arg('url') url: string): ExpressionRendered {
        if(url === 'neighbourhood://Qm123') {
            return testExpression
        } else {
            return null
        }
    }

    @Query(returns => [ExpressionRendered], {nullable: "items"})
    expressionMany(@Arg('urls', type => [String]) urls: string[]): (ExpressionRendered | null)[] {
        return [testExpression, null]
    }

    @Query(returns => String, {nullable: true})
    expressionRaw(@Arg('url') url: string): string {
        if(url === 'neighbourhood://Qm123') {
            return JSON.stringify(testExpression)
        } else {
            return null
        }
    }

    @Mutation(returns => String)
    expressionCreate(
        @Arg('content') content: string, 
        @Arg('languageAddress') languageAddress: string
    ): string {
        return "Qm1234"
    }

    @Query(returns => [InteractionMeta])
    expressionInteractions(@Arg('url') url: string): InteractionMeta[] {
        const interaction = {
            label: "Add a comment",
            name: "add_comment",
            parameters: [{name: 'comment', type: 'string'}]
        } as InteractionMeta
        return [interaction]
    }

    @Mutation(returns => String, {nullable: true})
    expressionInteract(
        @Arg('url') url: string,
        @Arg('interactionCall') interactionCall: InteractionCall
    ): string|null {
        return "test result"
    }
}