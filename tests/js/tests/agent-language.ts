import { TestContext } from './integration.test'
import { sleep } from '../utils/utils'
import { expect } from "chai";

export default function agentLanguageTests(testContext: TestContext) {
    return () => {
        it("works across remote agents", async () => {
            const alice = testContext.alice!
            const didAlice = (await alice.agent.status()).did!
            const bob = testContext.bob!
            const didBob = (await bob.agent.status()).did!

            const aliceHerself = await alice.agent.me()
            const bobHimself = await bob.agent.me()

            // Helper function to retry agent lookup with logging
            async function retryAgentLookup(
                client: typeof alice,
                targetDid: string,
                clientName: string,
                targetName: string,
                maxAttempts: number = 90
            ) {
                let result = await client.agent.byDID(targetDid)
                let attempts = 0
                while (!result && attempts < maxAttempts) {
                    if (attempts % 10 === 0) {
                        console.log(`${clientName} looking up ${targetName}... attempt ${attempts}/${maxAttempts}`)
                    }
                    await sleep(1000)
                    result = await client.agent.byDID(targetDid)
                    attempts++
                }
                if (!result) {
                    console.error(`${clientName} failed to find ${targetName} after ${maxAttempts} attempts`)
                    console.error(`Target DID: ${targetDid}`)
                }
                return result
            }

            await sleep(5000)

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