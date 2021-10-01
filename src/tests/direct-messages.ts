import { ExpressionProof, Link, LinkExpressionInput, Literal, Perspective } from '@perspect3vism/ad4m'
import { TestContext } from './integration.test'
import sleep from './sleep'

export default function directMessageTests(testContext: TestContext) {
    return () => {


        it("don't work when we're not friends", async () => {
            const alice = testContext.alice!
            const bob = testContext.bob!
            const { did } = await bob.agent.status()

            const status = await alice.runtime.friendStatus(did!)
            expect(status).toBe(null)
        })    

        describe("with Alice and Bob being friends", () => {
            //@ts-ignore
            let alice, bob, didAlice, didBob

            beforeAll(async () => {
                alice = testContext.alice!
                didAlice = (await alice.agent.status()).did
                bob = testContext.bob!
                didBob = (await bob.agent.status()).did

                await alice.runtime.addTrustedAgents([didBob!])
                await alice.runtime.addFriends([didBob!])
                await bob.runtime.addTrustedAgents([didAlice!])
                await bob.runtime.addFriends([didAlice!])
            })

            it("Alice can get Bob's status", async () => {    
                //@ts-ignore
                const status = await alice.runtime.friendStatus(didBob)
                expect(status).toBeDefined()
            })

            it("Alice can send a message to Bob", async () => {
                let link = new LinkExpressionInput()
                link.author = "did:test";
                link.timestamp = new Date().toISOString();
                link.data = new Link({
                    source: Literal.from("me").toUrl(),  
                    predicate: Literal.from("thinks").toUrl(),
                    target: Literal.from("nothing").toUrl()
                });
                link.proof = new ExpressionProof("sig", "key");
                const message = new Perspective([link])
                //@ts-ignore
                await alice.runtime.friendSendMessage(didBob, message)
                await sleep(1000)
                //@ts-ignore
                const bobsInbox = await bob.runtime.messageInbox()
                expect(bobsInbox.length).toBe(1)
                expect(bobsInbox[0].data).toStrictEqual(message)
            })
        })
    }
}
