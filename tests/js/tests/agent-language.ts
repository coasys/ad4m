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

            await sleep(5000)

            let bobSeenFromAlice = await alice.agent.byDID(didBob)
            let attempts = 0;
            while (!bobSeenFromAlice && attempts < 30) {
                await sleep(1000)
                bobSeenFromAlice = await alice.agent.byDID(didBob)
                attempts++
            }
            expect(bobSeenFromAlice).not.to.be.undefined
            expect(bobSeenFromAlice).to.be.eql(bobHimself)

            const aliceSeenFromBob = await bob.agent.byDID(didAlice)
            expect(aliceSeenFromBob).not.to.be.undefined
            expect(aliceSeenFromBob).to.be.eql(aliceHerself)
        })
    }
}