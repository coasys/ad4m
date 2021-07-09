import { TestContext } from './integration.test'

export default function expressionTests(testContext: TestContext) {
    return () => {
        describe('Expressions', () => {
            it('can get() my agent expression', async () => {
                const ad4mClient = testContext.ad4mClient!
                const me = await ad4mClient.agent.me()
                
                const agent = await ad4mClient.expression.get(me.did)

                expect(agent.proof.valid).toBeTruthy()
                expect(agent.proof.invalid).toBeFalsy()
            })

            it('can getRaw() my agent expression', async () => {
                const ad4mClient = testContext.ad4mClient!
                const me = await ad4mClient.agent.me()
                
                const agent = await ad4mClient.expression.getRaw(me.did)
                expect(JSON.parse(agent).data.did).toEqual(me.did);
                expect(JSON.parse(agent).data.directMessageLanguage).toEqual(me.directMessageLanguage);
            })

            it('can create()', async () => {
                const ad4mClient = testContext.ad4mClient!
                let me = await ad4mClient.agent.me()

                me.directMessageLanguage = "test 2"
                const result = await ad4mClient.expression.create(me, "did")
                expect(result).toBe(me.did)
            })

            it('can create valid signatures', async () => {
                const ad4mClient = testContext.ad4mClient!
                //@ts-ignore
                const noteIpfs = (await ad4mClient.languages.byFilter('')).find(l=>l.name ==='note-ipfs')
                expect(noteIpfs).toBeDefined()

                const exprAddr = await ad4mClient.expression.create("test note", noteIpfs!.address)
                expect(exprAddr).toBeDefined()

                const expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).toBeDefined()
                expect(expr.proof.valid).toBeTruthy()
            })
        })
    }
}