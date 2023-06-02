import { InteractionCall, LanguageMetaInput, Literal, parseExprUrl } from '@perspect3vism/ad4m'
import { TestContext } from './full-integration.test'
import fs from "fs";
import { expect } from "chai";

export default function expressionTests(testContext: TestContext) {
    return () => {
        describe('Expressions', () => {
            let noteLangAddress = "";

            before(async () => {
                const ad4mClient = testContext.ad4mClient!
                
                //Publish mocking interactions language so it can be used
                const publish = await ad4mClient.languages.publish("./languages/note-store/build/bundle.js", {name: "note-store", description: "A test language for saving simple strings"} as LanguageMetaInput)

                noteLangAddress = publish.address;
            })
    
            it('can get() my agent expression', async () => {
                const ad4mClient = testContext.ad4mClient!
                const me = await ad4mClient.agent.me()
                
                const agent = await ad4mClient.expression.get(me.did)
                console.warn(agent);

                expect(agent.proof.valid).to.be.true;
                expect(agent.proof.invalid).to.be.false;
            })

            it('can getManyExpressions()', async () => {
                const ad4mClient = testContext.ad4mClient!;
                const me = await ad4mClient.agent.me();

                const agentAndNull = await ad4mClient.expression.getMany([me.did, "lang://getNull", me.did]);
                expect(agentAndNull.length).to.be.equal(3);
                expect(JSON.parse(agentAndNull[0].data).did).to.be.equal(me.did);
                expect(agentAndNull[1]).to.be.null;
                expect(JSON.parse(agentAndNull[2].data).did).to.be.equal(me.did);
            })

            it('can getRaw() my agent expression', async () => {
                const ad4mClient = testContext.ad4mClient!
                const me = await ad4mClient.agent.me()
                
                const agent = await ad4mClient.expression.getRaw(me.did)
                expect(JSON.parse(agent).data.did).to.be.equal(me.did);
                expect(JSON.parse(agent).data.directMessageLanguage).to.be.equal(me.directMessageLanguage);
            })

            it('can create()', async () => {
                const ad4mClient = testContext.ad4mClient!
                let me = await ad4mClient.agent.me()

                const result = await ad4mClient.expression.create(me, "did")
                expect(result).to.be.equal(me.did)
            })

            it('can create valid signatures', async () => {
                const ad4mClient = testContext.ad4mClient!

                const exprAddr = await ad4mClient.expression.create("test note", noteLangAddress)
                expect(exprAddr).not.to.be.undefined;

                const expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).not.to.be.undefined;
                expect(expr.proof.valid).to.be.true;
            })

            it('can get expression from cache', async () => {
                const ad4mClient = testContext.ad4mClient!

                const exprAddr = await ad4mClient.expression.create("test note", noteLangAddress)
                expect(exprAddr).not.to.be.undefined;

                const expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).not.to.be.undefined;
                expect(expr.proof.valid).to.be.true;
                expect(expr.data).to.be.equal("\"test note\"");

                const exprCacheHit = await ad4mClient.expression.get(exprAddr)
                expect(exprCacheHit).not.to.be.undefined;
                expect(exprCacheHit.proof.valid).to.be.true;
                expect(exprCacheHit.data).to.be.equal("\"test note\"");

                const objExpr = await ad4mClient.expression.create({"key": "value"}, noteLangAddress)
                expect(objExpr).not.to.be.undefined;

                const exprObj = await ad4mClient.expression.get(objExpr)
                expect(exprObj).not.to.be.undefined;
                expect(exprObj.proof.valid).to.be.true;
                expect(exprObj.data).to.be.equal(JSON.stringify({"key": "value"}));

                const exprObjCacheHit = await ad4mClient.expression.get(objExpr)
                expect(exprObjCacheHit).not.to.be.undefined;
                expect(exprObjCacheHit.proof.valid).to.be.true;
                expect(exprObjCacheHit.data).to.be.equal(JSON.stringify({"key": "value"}));
            })

            it('can use expression interactions', async () => {
                const ad4mClient = testContext.ad4mClient!
                //Publish mocking interactions language so it can be used
                const publish = await ad4mClient.languages.publish("./languages/test-language/build/bundle.js", {name: "test-language", description: "A test language for interactions"} as LanguageMetaInput)

                //@ts-ignore
                const testLangAddress = publish.address;

                const exprAddr = await ad4mClient.expression.create("test note", testLangAddress)
                expect(exprAddr).not.to.be.undefined;

                let expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).not.to.be.undefined;
                expect(expr.proof.valid).to.be.true;
                expect(expr.data).to.be.equal("\"test note\"");

                const interactions = await ad4mClient.expression.interactions(exprAddr)

                expect(interactions.length).to.be.equal(1)
                expect(interactions[0].name).to.be.equal('modify')

                const interactionCall = new InteractionCall('modify', { newValue: 'modified note' })
                const result = await ad4mClient.expression.interact(exprAddr, interactionCall)
                expect(result).to.be.equal('ok')

                expr = await ad4mClient.expression.get(exprAddr)
                expect(expr).not.to.be.undefined;
                expect(expr.proof.valid).to.be.true;
                expect(expr.data).to.be.equal("\"modified note\"");
            })

            it('Literal language expressions can be created with signature and can get resolved from URL', async () => {
                const ad4mClient = testContext.ad4mClient!

                const TEST_DATA = "Hello World"
                const addr = await ad4mClient.expression.create(TEST_DATA, "literal")
                const exprRef = parseExprUrl(addr)
                expect(exprRef.language.address).to.be.equal("literal")

                const expr = Literal.fromUrl(addr).get()

                expect(expr.data).to.be.equal(TEST_DATA)

                const expr2Raw = await ad4mClient.expression.getRaw(addr)
                const expr2 = JSON.parse(expr2Raw)
                console.log(expr2)

                expect(expr2).to.be.eql(expr)
            })
        })
    }
}