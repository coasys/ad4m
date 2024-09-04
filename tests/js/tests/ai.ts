import { TestContext } from './integration.test'
import { expect } from "chai";

export default function aiTests(testContext: TestContext) {
    return () => {
        describe('AI service', () => {
            it('can embed text to vectors', async () => {
                const ad4mClient = testContext.ad4mClient!

                let vector = await ad4mClient.ai.embed("Bert", "Test string");
                expect(typeof vector).to.equal("Array")
                expect(vector.length).to.be.greaterThan(300)
            }),
        })
    }
}