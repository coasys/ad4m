import { TestContext } from './test-context'
import { pollUntil } from '../utils/utils'
import { expect } from "chai";

export default function agentLanguageTests(testContext: TestContext) {
    return () => {
        it.skip("works across remote agents", async function() {
            this.retries(2)
            const alice = testContext.alice!
            const didAlice = (await alice.agent.status()).did!
            const bob = testContext.bob!
            const didBob = (await bob.agent.status()).did!

            const aliceHerself = await alice.agent.me()
            const bobHimself = await bob.agent.me()

            async function retryAgentLookup(
                client: typeof alice,
                targetDid: string,
                clientName: string,
                targetName: string,
            ) {
                let result: any = null;
                await pollUntil(async () => {
                    result = await client.agent.byDID(targetDid);
                    if (!result) console.log(`${clientName} looking up ${targetName}...`);
                    return !!result;
                }, { timeoutMs: 25000, intervalMs: 1000, label: `${clientName} finds ${targetName}` });
                return result;
            }

            // Both lookups now have retry logic
            const bobSeenFromAlice = await retryAgentLookup(alice, didBob, "Alice", "Bob")
            expect(bobSeenFromAlice, "Alice should be able to see Bob's agent profile").to.not.be.null
            expect(bobSeenFromAlice).to.be.eql(bobHimself)

            const aliceSeenFromBob = await retryAgentLookup(bob, didAlice, "Bob", "Alice")
            expect(aliceSeenFromBob, "Bob should be able to see Alice's agent profile").to.not.be.null
            expect(aliceSeenFromBob).to.be.eql(aliceHerself)
        })
    }
}