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

            it('can getManyExpressions()', async () => {
                const ad4mClient = testContext.ad4mClient!;
                const me = await ad4mClient.agent.me();

                const agentAndNull = await ad4mClient.expression.getMany([me.did, "lang://getNull", me.did]);
                expect(agentAndNull.length).toBe(3);
                expect(JSON.parse(agentAndNull[0].data).did).toEqual(me.did);
                expect(agentAndNull[1]).toBeNull();
                expect(JSON.parse(agentAndNull[2].data).did).toEqual(me.did);
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

            it('can get expression from cache', async () => {
                const ad4mClient = testContext.ad4mClient!
                //@ts-ignore
                const noteIpfs = (await ad4mClient.languages.byFilter('')).find(l=>l.name ==='note-ipfs')
                expect(noteIpfs).toBeDefined()

                const exprAddr = await ad4mClient.expression.create("test note", noteIpfs!.address)
                expect(exprAddr).toBeDefined()

                const expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).toBeDefined()
                expect(expr.proof.valid).toBeTruthy()
                expect(expr.data).toBe("\"test note\"");

                const exprCacheHit = await ad4mClient.expression.get(exprAddr)
                expect(exprCacheHit).toBeDefined()
                expect(exprCacheHit.proof.valid).toBeTruthy()
                expect(exprCacheHit.data).toBe("\"test note\"");

                const objExpr = await ad4mClient.expression.create(JSON.stringify({"key": "value"}), noteIpfs!.address)
                expect(objExpr).toBeDefined()

                const exprObj = await ad4mClient.expression.get(objExpr)
                expect(exprObj).toBeDefined()
                expect(exprObj.proof.valid).toBeTruthy()
                expect(exprObj.data).toBe(JSON.stringify({"key": "value"}));

                const exprObjCacheHit = await ad4mClient.expression.get(objExpr)
                expect(exprObjCacheHit).toBeDefined()
                expect(exprObjCacheHit.proof.valid).toBeTruthy()
                expect(exprObjCacheHit.data).toBe(JSON.stringify({"key": "value"}));
            })
        })
    }
}