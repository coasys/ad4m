import { TestContext } from './integration.test'

export default function expressionTests(testContext: TestContext) {
    return () => {
        describe('Expressions', () => {
            it('can get() my agent expression', async () => {
                const ad4mClient = testContext.ad4mClient

                const me = await ad4mClient.agent.me()
                
                const agent = await ad4mClient.expression.get(me.did)
                expect(agent).toEqual(me);
            })
        })
    }
}