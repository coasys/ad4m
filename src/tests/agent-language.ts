import { TestContext } from './integration.test'

export default function agentLanguageTests(testContext: TestContext) {
    return () => {
        it("works across remote agents", async () => {

            const alice = testContext.alice!
            const didAlice = (await alice.agent.status()).did!
            const bob = testContext.bob!
            const didBob = (await bob.agent.status()).did!

            const aliceHerself = await alice.agent.me()
            const bobHimself = await bob.agent.me()

            const bobSeenFromAlice = await alice.agent.byDID(didBob)
            expect(bobSeenFromAlice).toBeDefined()
            expect(bobSeenFromAlice).toEqual(bobHimself)

            const aliceSeenFromBob = await bob.agent.byDID(didAlice)
            expect(aliceSeenFromBob).toBeDefined()
            expect(aliceSeenFromBob).toEqual(aliceHerself)
        })    
    }
}