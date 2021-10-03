import { ExpressionProof, Link, LinkExpressionInput, Literal, Perspective } from '@perspect3vism/ad4m'
import { TestContext } from './integration.test'
import sleep from './sleep'

export default function directMessageTests(testContext: TestContext) {
    return () => {


        it("don't work when we're not friends", async () => {
            const alice = testContext.alice!
            const bob = testContext.bob!
            const { did } = await bob.agent.status()

            let hasThrown = false
            try{
                await alice.runtime.friendStatus(did!)
            }catch(e) {
                hasThrown = true
            }
            
            expect(hasThrown).toBe(true)
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

            afterAll(async () => {
                alice = testContext.alice!
                didAlice = (await alice.agent.status()).did
                bob = testContext.bob!
                didBob = (await bob.agent.status()).did

                await alice.runtime.deleteTrustedAgents([didBob!])
                await bob.runtime.deleteTrustedAgents([didAlice!])
            })

            it("Alice can get Bob's status", async () => {
                let link = new LinkExpressionInput()
                link.author = "did:test";
                link.timestamp = new Date().toISOString();
                link.data = new Link({
                    //@ts-ignore
                    source: didBob,  
                    predicate: Literal.from("is").toUrl(),
                    target: Literal.from("online").toUrl()
                });
                link.proof = new ExpressionProof("sig", "key");
                const statusBob = new Perspective([link])
                //@ts-ignore
                await bob.runtime.setStatus(statusBob)
                await sleep(100)    
                //@ts-ignore
                const statusAlice = await alice.runtime.friendStatus(didBob)
                expect(statusAlice).toBeDefined()
                delete statusAlice.data.links[0].proof.invalid
                delete statusAlice.data.links[0].proof.valid
                expect(statusAlice.data).toEqual(statusBob)
            })

            it("Alice can send a message to Bob", async () => {
                const bobMessageCallback = jest.fn()
                //@ts-ignore
                await bob.runtime.addMessageCallback(bobMessageCallback)

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
                await sleep(100)
                //@ts-ignore
                const bobsInbox = await bob.runtime.messageInbox()
                expect(bobsInbox.length).toBe(1)

                expect(bobMessageCallback.mock.calls.length).toBe(1)
                expect(bobMessageCallback.mock.calls[0][0]).toEqual(bobsInbox[0])

                delete bobsInbox[0].data.links[0].proof.invalid
                delete bobsInbox[0].data.links[0].proof.valid
                expect(bobsInbox[0].data).toEqual(message)
                
            })
        })
    }
}
