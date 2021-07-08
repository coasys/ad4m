import { TestContext } from './integration.test'

export default function expressionTests(testContext: TestContext) {
    return () => {
        describe('Expressions', () => {
            it('can get() my agent expression', async () => {
                const ad4mClient = testContext.ad4mClient
                const me = await ad4mClient.agent.me()
                
                const agent = await ad4mClient.expression.get(me.did)
                expect(JSON.parse(agent.data)).toEqual(me);
            })

            it('can getRaw() my agent expression', async () => {
                const ad4mClient = testContext.ad4mClient
                const me = await ad4mClient.agent.me()
                
                const agent = await ad4mClient.expression.getRaw(me.did)
                expect(JSON.parse(agent).data).toEqual(me);
            })

            it('can create()', async () => {
                const ad4mClient = testContext.ad4mClient
                let me = await ad4mClient.agent.me()

                me.directMessageLanguage = "test 2"
                const result = await ad4mClient.expression.create(me, "did")
                expect(result).toBe(me.did)


            })
        })
    }
}