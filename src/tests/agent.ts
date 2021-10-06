import { Perspective, LinkExpression, Link, ExpressionProof, EntanglementProofInput } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from './sleep'

export default function agentTests(testContext: TestContext) {
    return () => {
        describe('basic agent operations', () => {
            it('can get and create agent store', async () => {
                const ad4mClient = testContext.ad4mClient!
                
                const generate = await ad4mClient.agent.generate("passphrase")
                expect(generate.isInitialized).toBe(true);
                expect(generate.isUnlocked).toBe(true);
    
                // //Should be able to create a perspective
                // const create = await ad4mClient.perspective.add("test");
                // expect(create.name).toBe("test");
    
                const lockAgent = await ad4mClient.agent.lock("passphrase");
                expect(lockAgent.isInitialized).toBe(true);
                expect(lockAgent.isUnlocked).toBe(false);
    
                // //Should not be able to create a perspective
                // const createLocked = await ad4mClient.perspective.add("test2");
                // console.log(createLocked);
    
                const unlockAgent = await ad4mClient.agent.unlock("passphrase");
                expect(unlockAgent.isInitialized).toBe(true);
                expect(unlockAgent.isUnlocked).toBe(true);
    
                // //Should be able to create a perspective
                // const create = await ad4mClient.perspective.add("test3");
                // expect(create.name).toBe("test3");
    
                const agentDump = await ad4mClient.agent.status();
                expect(agentDump.isInitialized).toBe(true);
                expect(agentDump.isUnlocked).toBe(true);
            }),
            it('can get and create agent expression profile', async () => {
                const ad4mClient = testContext.ad4mClient!

                const agentUpdated = jest.fn()
                ad4mClient.agent.addUpdatedListener(agentUpdated)

                const currentAgent = await ad4mClient.agent.me();
                expect(currentAgent.perspective).toBeDefined()
                expect(currentAgent.perspective!.links.length).toBe(0);
                expect(currentAgent.directMessageLanguage).toBe(null);

                let link = new LinkExpression();
                link.author = "did:test";
                link.timestamp = new Date().toISOString();
                link.data = new Link({source: "src", target: "target", predicate: "pred"});
                link.proof = new ExpressionProof("sig", "key")
                const updatePerspective = await ad4mClient.agent.updatePublicPerspective(new Perspective([link]))
                expect(currentAgent.perspective).toBeDefined()
                expect(updatePerspective.perspective!.links.length).toBe(1);
                await sleep(500)
                expect(agentUpdated.mock.calls.length).toBe(1)
                expect(agentUpdated.mock.calls[0][0]).toEqual(updatePerspective)

                const updatePublicLanguage = await ad4mClient.agent.updateDirectMessageLanguage("newlang");
                expect(currentAgent.perspective).toBeDefined()
                expect(updatePublicLanguage.perspective!.links.length).toBe(1);
                expect(updatePublicLanguage.directMessageLanguage).toBe("newlang");
                await sleep(500)
                expect(agentUpdated.mock.calls.length).toBe(2)
                expect(agentUpdated.mock.calls[1][0]).toEqual(updatePublicLanguage)

                const currentAgentPostUpdate = await ad4mClient.agent.me();
                expect(currentAgent.perspective).toBeDefined()
                expect(currentAgentPostUpdate.perspective!.links.length).toBe(1);
                expect(currentAgentPostUpdate.directMessageLanguage).toBe("newlang");

                const getByDid = await ad4mClient.agent.byDID(currentAgent.did);
                expect(getByDid.did).toBe(currentAgent.did);
                expect(currentAgent.perspective).toBeDefined()
                expect(getByDid.perspective!.links.length).toBe(1);
                expect(getByDid.directMessageLanguage).toBe("newlang");

                const getInvalidDid = await ad4mClient.agent.byDID("na");
                expect(getInvalidDid).toBe(null);
            })
            it('can create entanglementProofPreFlight', async () => {
                const ad4mClient = testContext.ad4mClient!;

                //Check can generate a preflight key
                const preFlight = await ad4mClient.agent.entanglementProofPreFlight("ethAddr", "ethereum");
                expect(preFlight.deviceKey).toBe("ethAddr");
                expect(preFlight.deviceKeyType).toBe("ethereum");
                expect(preFlight.didSignedByDeviceKey).toBeNull();

                const verify = await ad4mClient.runtime.verifyStringSignedByDid(preFlight.did, preFlight.didSigningKeyId, "ethAddr", preFlight.deviceKeySignedByDid);
                expect(verify).toBe(true);

                //Check can save a entanglement proof
                preFlight.didSignedByDeviceKey = "ethSignedDID";
                const addProof = await ad4mClient.agent.addEntanglementProofs([preFlight as EntanglementProofInput]);
                expect(addProof[0]).toStrictEqual(preFlight);

                //Check can get entanglment proofs
                const getProofs = await ad4mClient.agent.getEntanglementProofs();
                expect(getProofs[0]).toStrictEqual(preFlight);

                //Check can delete entanglement proofs
                const deleteProofs = await ad4mClient.agent.deleteEntanglementProofs([preFlight as EntanglementProofInput]);
                expect(deleteProofs.length).toBe(0);

                //Check entanglement proof is deleted on get
                const getProofsPostDelete = await ad4mClient.agent.getEntanglementProofs();
                expect(getProofsPostDelete.length).toBe(0);
            })
        })
    }
}