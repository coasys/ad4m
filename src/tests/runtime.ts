import { Perspective, LinkExpression, Link, ExpressionProof } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from './sleep'

export default function runtimeTests(testContext: TestContext) {
    return () => {


        it('Trusted Agents CRUD', async () => {
            const ad4mClient = testContext.ad4mClient!
            const { did } = await ad4mClient.agent.status()

            const initalAgents = await ad4mClient.runtime.getTrustedAgents();
            expect(initalAgents).toEqual([ did ])
            
            const addAgents = await ad4mClient.runtime.addTrustedAgents(["agentPubKey", "agentPubKey2"]);
            expect(addAgents).toStrictEqual([ did, 'agentPubKey', 'agentPubKey2' ])

            //Add the agents again to be sure we cannot get any duplicates
            const addAgentsDuplicate = await ad4mClient.runtime.addTrustedAgents(["agentPubKey", "agentPubKey2"]);
            expect(addAgentsDuplicate).toStrictEqual([ did, 'agentPubKey', 'agentPubKey2' ])

            const getAgents = await ad4mClient.runtime.getTrustedAgents();
            expect(getAgents).toStrictEqual([ did, 'agentPubKey', 'agentPubKey2' ])

            const deleteAgents1 = await ad4mClient.runtime.deleteTrustedAgents(["agentPubKey2"])
            expect(deleteAgents1).toStrictEqual([ did, "agentPubKey" ])

            const deleteAgents2 = await ad4mClient.runtime.deleteTrustedAgents(["agentPubKey", "agentPubKey2"])
            expect(deleteAgents2).toStrictEqual([ did ])

            const getAgentsPostDelete = await ad4mClient.runtime.getTrustedAgents();
            expect(getAgentsPostDelete).toStrictEqual([ did ])
        })    


        it('CRUD for known LinkLanguage templates', async () => {
            const ad4mClient = testContext.ad4mClient!

            const addresses = await ad4mClient.runtime.knownLinkLanguageTemplates();
            expect(addresses).toEqual([ ])
            
            const addAddresses = await ad4mClient.runtime.addKnownLinkLanguageTemplates(["Qm123", "Qmabc"]);
            expect(addAddresses).toStrictEqual([ 'Qm123', 'Qmabc' ])

            //Add the agents again to be sure we cannot get any duplicates
            const addDuplicate = await ad4mClient.runtime.addKnownLinkLanguageTemplates(["Qm123", "Qmabc"]);
            expect(addDuplicate).toStrictEqual([ 'Qm123', 'Qmabc' ])

            const get = await ad4mClient.runtime.knownLinkLanguageTemplates();
            expect(get).toStrictEqual([ 'Qm123', 'Qmabc' ])

            const deleted = await ad4mClient.runtime.removeKnownLinkLanguageTemplates(["Qm123"])
            expect(deleted).toStrictEqual([ "Qmabc" ])

            const deleted2 = await ad4mClient.runtime.removeKnownLinkLanguageTemplates(["Qm123", "Qmabc"])
            expect(deleted2).toStrictEqual([])

            const getPostDelete = await ad4mClient.runtime.knownLinkLanguageTemplates();
            expect(getPostDelete).toStrictEqual([  ])
        })

        it('CRUD for friends', async () => {
            const ad4mClient = testContext.ad4mClient!

            const dids = await ad4mClient.runtime.friends();
            expect(dids).toEqual([ ])
            
            const added = await ad4mClient.runtime.addFriends(["did:test:1", "did:test:2"]);
            expect(added).toStrictEqual(["did:test:1", "did:test:2"])

            //Add the agents again to be sure we cannot get any duplicates
            const addDuplicate = await ad4mClient.runtime.addFriends(["did:test:1", "did:test:2"]);
            expect(addDuplicate).toStrictEqual(["did:test:1", "did:test:2"])

            const get = await ad4mClient.runtime.friends();
            expect(get).toStrictEqual(["did:test:1", "did:test:2"])

            const deleted = await ad4mClient.runtime.removeFriends(["did:test:1"])
            expect(deleted).toStrictEqual([ "did:test:2" ])

            const deleted2 = await ad4mClient.runtime.removeFriends(["did:test:1", "did:test:2"])
            expect(deleted2).toStrictEqual([])

            const getPostDelete = await ad4mClient.runtime.friends();
            expect(getPostDelete).toStrictEqual([  ])
        })

        it("doesn't mix up stores", async () => {
            const ad4mClient = testContext.ad4mClient!
            const { did } = await ad4mClient.agent.status()

            await ad4mClient.runtime.addFriends(["did:test:1", "did:test:2"]);

            const addresses = await ad4mClient.runtime.knownLinkLanguageTemplates();
            expect(addresses).toEqual([ ])

            const initalAgents = await ad4mClient.runtime.getTrustedAgents();
            expect(initalAgents).toEqual([ did ])


            const addAddresses = await ad4mClient.runtime.addKnownLinkLanguageTemplates(["Qm123", "Qmabc"]);
            expect(addAddresses).toStrictEqual([ 'Qm123', 'Qmabc' ])

            const addAgents = await ad4mClient.runtime.addTrustedAgents(["agentPubKey", "agentPubKey2"]);
            expect(addAgents).toStrictEqual([ did, 'agentPubKey', 'agentPubKey2' ])

            const dids = await ad4mClient.runtime.friends()
            expect(dids).toEqual(["did:test:1", "did:test:2"])


            const deleted = await ad4mClient.runtime.removeFriends(["did:test:1", "agentPubKey", "Qm123"])
            expect(deleted).toStrictEqual([ ])

            const postDeleteAddresses = await ad4mClient.runtime.knownLinkLanguageTemplates()
            expect(postDeleteAddresses).toStrictEqual([ 'Qm123', 'Qmabc' ])

            const postDeleteAgents = await ad4mClient.runtime.getTrustedAgents()
            expect(postDeleteAgents).toStrictEqual([ did, 'agentPubKey', 'agentPubKey2' ])
        })
    }
}
