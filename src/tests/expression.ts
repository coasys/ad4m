import { InteractionCall, LanguageMetaInput, Literal, parseExprUrl } from '@perspect3vism/ad4m'
import { TestContext } from './integration.test'
import fs from "fs";

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
                const noteIpfs = fs.readFileSync("./scripts/note-ipfs-hash").toString();

                const exprAddr = await ad4mClient.expression.create("test note", noteIpfs)
                expect(exprAddr).toBeDefined()

                const expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).toBeDefined()
                expect(expr.proof.valid).toBeTruthy()
            })

            it('can get expression from cache', async () => {
                const ad4mClient = testContext.ad4mClient!
                const noteIpfs = fs.readFileSync("./scripts/note-ipfs-hash").toString();

                const exprAddr = await ad4mClient.expression.create("test note", noteIpfs)
                expect(exprAddr).toBeDefined()

                const expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).toBeDefined()
                expect(expr.proof.valid).toBeTruthy()
                expect(expr.data).toBe("\"test note\"");

                const exprCacheHit = await ad4mClient.expression.get(exprAddr)
                expect(exprCacheHit).toBeDefined()
                expect(exprCacheHit.proof.valid).toBeTruthy()
                expect(exprCacheHit.data).toBe("\"test note\"");

                const objExpr = await ad4mClient.expression.create(JSON.stringify({"key": "value"}), noteIpfs)
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

            it('can use expression interactions', async () => {
                const ad4mClient = testContext.ad4mClient!
                //Publish mocking interactions language so it can be used
                const publish = await ad4mClient.languages.publish("./src/test-temp/languages/test-language/build/bundle.js", {name: "test-language", description: "A test language for interactions"} as LanguageMetaInput)

                //@ts-ignore
                const testLangAddress = publish.address;

                const exprAddr = await ad4mClient.expression.create("test note", testLangAddress)
                expect(exprAddr).toBeDefined()

                let expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).toBeDefined()
                expect(expr.proof.valid).toBeTruthy()
                expect(expr.data).toBe("\"test note\"");

                const interactions = await ad4mClient.expression.interactions(exprAddr)

                expect(interactions.length).toBe(1)
                expect(interactions[0].name).toBe('modify')

                const interactionCall = new InteractionCall('modify', { newValue: 'modified note' })
                const result = await ad4mClient.expression.interact(exprAddr, interactionCall)
                expect(result).toBe('ok')

                expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).toBeDefined()
                expect(expr.proof.valid).toBeTruthy()
                expect(expr.data).toBe("\"modified note\"");
            })

            it('Literal language expressions can be created with signature and can get resolved from URL', async () => {
                const ad4mClient = testContext.ad4mClient!

                const TEST_DATA = "Hello World"
                const addr = await ad4mClient.expression.create(TEST_DATA, "literal")
                const exprRef = parseExprUrl(addr)
                expect(exprRef.language.address).toBe("literal")

                const expr = Literal.fromUrl(addr).get()
                delete expr.proof.invalid

                expect(expr.data).toBe(TEST_DATA)

                const expr2Raw = await ad4mClient.expression.getRaw(addr)
                const expr2 = JSON.parse(expr2Raw)
                console.log(expr2)

                expect(expr2).toStrictEqual(expr)
            })
        })
    }
}