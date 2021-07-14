import path from "path"
import { Link, Perspective, LinkExpression, ExpressionProof, LinkQuery, PerspectiveHandle } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from "./sleep";


export default function neighbourhoodTests(testContext: TestContext) {
    return () => {
        describe('Neighbourhood', () => {
            let neighbourhoodUrl

            it('can publish and join locally @alice', async () => {
                const ad4mClient = testContext.alice!;

                const create = await ad4mClient!.perspective.add("publish-test");
                expect(create.name).toEqual("publish-test");

                //Create unique social-context to simulate real scenario
                const createUniqueLang = await ad4mClient.languages.cloneHolochainTemplate(path.join(__dirname, "../test-temp/social-context"), "social-context", "b98e53a8-5800-47b6-adb9-86d55a74871e");
                expect(createUniqueLang.name).toBe("social-context-channel");

                let link = new LinkExpression()
                link.author = "did:test";
                link.timestamp = new Date().toISOString();
                link.data = new Link({source: "src", target: "target", predicate: "pred"});
                link.proof = new ExpressionProof("sig", "key");
                const publishPerspective = await ad4mClient.neighbourhood.publishFromPerspective(create.uuid, createUniqueLang.address, 
                    new Perspective(
                        [link]
                    )
                );
                //Check that we got an ad4m url back
                expect(publishPerspective.split("://").length).toBe(2);

                const join = await ad4mClient.neighbourhood.joinFromUrl(publishPerspective );
                expect(join.sharedUrl).toBe(publishPerspective);
            })

            it('can be created by Alice and joined by Bob', async () => {
                const alice = testContext.alice
                const bob = testContext.bob

                const aliceP1 = await alice.perspective.add("friends")
                const createUniqueLang = await alice.languages.cloneHolochainTemplate(path.join(__dirname, "../test-temp/social-context"), "social-context", "b98e53a8-5800-47b6-adb9-alice-bob-friends");
                const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(aliceP1.uuid, createUniqueLang.address, new Perspective())

                let bobP1: PerspectiveHandle | null = null 
                let tries = 0
                while(!bobP1 && tries < 10) {
                    await sleep(10000)
                    tries++
                    try {
                        bobP1 = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl)
                    } catch(e) {
                        console.log(`Getting Neigbourhood error on try ${tries}:`, e)
                    }
                }
                
                expect(bobP1).toBeDefined()
                expect(bobP1!.name).toBeDefined()
                expect(bobP1!.sharedUrl).toEqual(neighbourhoodUrl)

                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})

                await sleep(5000)

                const bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                expect(bobLinks.length).toBe(1)
            })
        })
    }
}