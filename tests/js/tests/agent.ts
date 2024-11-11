import { Perspective, LinkExpression, Link, ExpressionProof, EntanglementProofInput } from "@coasys/ad4m";
import { TestContext } from './integration.test'
import { sleep } from '../utils/utils'
import { expect } from "chai";
import * as sinon from "sinon";

export default function agentTests(testContext: TestContext) {
    return () => {
        describe('basic agent operations', () => {
            it('can get and create agent store', async () => {
                const ad4mClient = testContext.ad4mClient!

                const agentUpdated = sinon.fake()
                ad4mClient.agent.addAgentStatusChangedListener(agentUpdated)
                
                const generate = await ad4mClient.agent.generate("passphrase")
                expect(generate.isInitialized).to.be.true;
                expect(generate.isUnlocked).to.be.true;

                await sleep(1000)
                expect(agentUpdated.calledOnce).to.be.true;
    
                // //Should be able to create a perspective
                // const create = await ad4mClient.perspective.add("test");
                // expect(create.name).to.equal("test");
    
                const lockAgent = await ad4mClient.agent.lock("passphrase");
                
                expect(lockAgent.isInitialized).to.be.true;
                expect(lockAgent.isUnlocked).to.be.false;

                await sleep(1000)
                expect(agentUpdated.calledTwice).to.be.true;
    
                // //Should not be able to create a perspective
                // const createLocked = await ad4mClient.perspective.add("test2");
                // console.log(createLocked);
    
                const unlockAgent = await ad4mClient.agent.unlock("passphrase");
                expect(unlockAgent.isInitialized).to.be.true;
                expect(unlockAgent.isUnlocked).to.be.true;

                await sleep(1000)
                expect(agentUpdated.calledThrice).to.be.true;
    
                // //Should be able to create a perspective
                // const create = await ad4mClient.perspective.add("test3");
                // expect(create.name).to.equal("test3");
    
                const agentDump = await ad4mClient.agent.status();
                expect(agentDump.isInitialized).to.be.true;
                expect(agentDump.isUnlocked).to.be.true;
            }),
            it('can get and create agent expression profile', async () => {
                const ad4mClient = testContext.ad4mClient!

                const agentUpdated = sinon.fake()
                ad4mClient.agent.addUpdatedListener(agentUpdated)

                const currentAgent = await ad4mClient.agent.me();
                expect(currentAgent.perspective).not.to.be.undefined;
                expect(currentAgent.perspective!.links.length).to.equal(0);
                expect(currentAgent.directMessageLanguage).not.to.be.undefined;
                const oldDmLang = currentAgent.directMessageLanguage!

                let link = new LinkExpression();
                link.author = "did:test";
                link.timestamp = new Date().toISOString();
                link.data = new Link({source: "src", target: "target", predicate: "pred"});
                link.proof = new ExpressionProof("sig", "key")
                const updatePerspective = await ad4mClient.agent.updatePublicPerspective(new Perspective([link]))
                expect(currentAgent.perspective).not.to.be.undefined
                expect(updatePerspective.perspective!.links.length).to.equal(1);

                await sleep(500)
                expect(agentUpdated.calledOnce).to.be.true;
                expect(agentUpdated.getCall(0).args[0]).to.deep.equal(updatePerspective)

                const updatePublicLanguage = await ad4mClient.agent.updateDirectMessageLanguage("newlang");
                expect(currentAgent.perspective).not.to.be.undefined
                expect(updatePublicLanguage.perspective!.links.length).to.equal(1);
                expect(updatePublicLanguage.directMessageLanguage).to.equal("newlang");

                await sleep(500)
                expect(agentUpdated.calledTwice).to.be.true;
                expect(agentUpdated.getCall(1).args[0]).to.deep.equal(updatePublicLanguage)

                const currentAgentPostUpdate = await ad4mClient.agent.me();
                expect(currentAgent.perspective).not.to.be.undefined;
                expect(currentAgentPostUpdate.perspective!.links.length).to.equal(1);
                expect(currentAgentPostUpdate.directMessageLanguage).to.equal("newlang");

                const getByDid = await ad4mClient.agent.byDID(currentAgent.did);
                expect(getByDid.did).to.equal(currentAgent.did);
                expect(currentAgent.perspective).not.to.be.undefined;
                expect(getByDid.perspective!.links.length).to.equal(1);
                expect(getByDid.directMessageLanguage).to.equal("newlang");

                await ad4mClient.agent.updateDirectMessageLanguage(oldDmLang);

                const getInvalidDid = await ad4mClient.agent.byDID("na");
                expect(getInvalidDid).to.equal(null);
            })
            it('can mutate agent public profile', async () => {
                const ad4mClient = testContext.ad4mClient!;

                const currentAgent = await ad4mClient.agent.me();
                expect(currentAgent.perspective).not.to.be.undefined;
                expect(currentAgent.perspective!.links.length).to.equal(1);
                expect(currentAgent.directMessageLanguage).not.to.be.undefined;

                await ad4mClient.agent.mutatePublicPerspective({
                    additions: [new Link({
                        source: "test://source-test",
                        predicate: "test://predicate-test",
                        target: "test://target-test"
                    })],
                    removals: []
                });

                const currentAgentPostMutation = await ad4mClient.agent.me();
                expect(currentAgentPostMutation.perspective).not.to.be.undefined;
                expect(currentAgentPostMutation.perspective!.links.length).to.equal(2);
                const link = currentAgentPostMutation.perspective!.links[0];

                await ad4mClient.agent.mutatePublicPerspective({
                    additions: [],
                    removals: [link]
                });

                const currentAgentPostDeletion = await ad4mClient.agent.me();
                expect(currentAgentPostDeletion.perspective).not.to.be.undefined;
                expect(currentAgentPostDeletion.perspective!.links.length).to.equal(1);
            })
            it('can create entanglementProofPreFlight', async () => {
                const ad4mClient = testContext.ad4mClient!;

                //Check can generate a preflight key
                const preFlight = await ad4mClient.agent.entanglementProofPreFlight("ethAddr", "ethereum");
                expect(preFlight.deviceKey).to.equal("ethAddr");
                expect(preFlight.deviceKeyType).to.equal("ethereum");
                expect(preFlight.didSignedByDeviceKey).to.be.null;

                const verify = await ad4mClient.runtime.verifyStringSignedByDid(preFlight.did, preFlight.didSigningKeyId, "ethAddr", preFlight.deviceKeySignedByDid);
                expect(verify).to.be.true;

                //Check can save a entanglement proof
                preFlight.didSignedByDeviceKey = "ethSignedDID";
                const addProof = await ad4mClient.agent.addEntanglementProofs([preFlight as EntanglementProofInput]);
                expect(addProof[0]).to.deep.equal(preFlight);

                //Check can get entanglment proofs
                const getProofs = await ad4mClient.agent.getEntanglementProofs();
                expect(getProofs[0]).to.deep.equal(preFlight);

                //Check can delete entanglement proofs
                const deleteProofs = await ad4mClient.agent.deleteEntanglementProofs([preFlight as EntanglementProofInput]);
                expect(deleteProofs.length).to.be.equal(0);

                //Check entanglement proof is deleted on get
                const getProofsPostDelete = await ad4mClient.agent.getEntanglementProofs();
                expect(getProofsPostDelete.length).to.be.equal(0);
            })
            it('can signMessage', async () => {
                const ad4mClient = testContext.ad4mClient!;

                const signed = await ad4mClient.agent.signMessage("test");
                expect(signed).to.not.be.null;
            })
        })
    }
}