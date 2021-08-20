import { Perspective, LinkExpression, Link, ExpressionProof } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from './sleep'

export default function runtimeTests(testContext: TestContext) {
    return () => {
        describe('can create and get trusted agent ', () => {
            it('can get and create agent store', async () => {
                const ad4mClient = testContext.ad4mClient!
                
                const addAgents = await ad4mClient.runtime.addTrustedAgents(["agentPubKey", "agentPubKey2"]);
                expect(addAgents).toStrictEqual([ 'agentPubKey', 'agentPubKey2' ])

                //Add the agents again to be sure we cannot get any duplicates
                const addAgentsDuplicate = await ad4mClient.runtime.addTrustedAgents(["agentPubKey", "agentPubKey2"]);
                expect(addAgentsDuplicate).toStrictEqual([ 'agentPubKey', 'agentPubKey2' ])

                const getAgents = await ad4mClient.runtime.getTrustedAgents();
                expect(getAgents).toStrictEqual([ 'agentPubKey', 'agentPubKey2' ])

                const deleteAgents = await ad4mClient.runtime.deleteTrustedAgents(["agentPubKey", "agentPubKey2"])
                expect(deleteAgents).toStrictEqual([])

                const getAgentsPostDelete = await ad4mClient.runtime.getTrustedAgents();
                expect(getAgentsPostDelete).toStrictEqual([])
            })
        })
    }
}
