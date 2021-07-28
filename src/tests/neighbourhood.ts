import path from "path"
import { Link, Perspective, LinkExpression, ExpressionProof, LinkQuery, PerspectiveHandle } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from "./sleep";

const LINK_LANGUAGE_PATH = path.join(__dirname, "../test-temp/languages/social-context")

export default function neighbourhoodTests(testContext: TestContext) {
    return () => {
        describe('Neighbourhood', () => {
            let neighbourhoodUrl

            it('can publish and join locally @alice', async () => {
                const ad4mClient = testContext.alice!;

                const create = await ad4mClient!.perspective.add("publish-test");
                expect(create.name).toEqual("publish-test");
                expect(create.neighbourhood).toBeNull();

                //Create unique social-context to simulate real scenario
                const createUniqueLang = await ad4mClient.languages.cloneHolochainTemplate(LINK_LANGUAGE_PATH, "social-context", "b98e53a8-5800-47b6-adb9-86d55a74871e");
                expect(createUniqueLang.name).toBe("social-context");

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

                const perspective = await ad4mClient.perspective.byUUID(create.uuid);
                expect(perspective?.neighbourhood).toBeDefined();

                const join = await ad4mClient.neighbourhood.joinFromUrl(publishPerspective );
                expect(join.sharedUrl).toBe(publishPerspective);
                expect(join.neighbourhood).toBeDefined();
            })

            it('can be created by Alice and joined by Bob', async () => {
                const alice = testContext.alice
                const bob = testContext.bob

                const aliceP1 = await alice.perspective.add("friends")
                const createUniqueLang = await alice.languages.cloneHolochainTemplate(LINK_LANGUAGE_PATH, "social-context", "b97e53a8-5800-47b6-adb9-alice-bob-friends");
                const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(aliceP1.uuid, createUniqueLang.address, new Perspective())

                let bobP1 = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);
                
                expect(bobP1).toBeTruthy()
                expect(bobP1!.name).toBeDefined()
                expect(bobP1!.sharedUrl).toEqual(neighbourhoodUrl)
                expect(bobP1!.neighbourhood).toBeDefined();

                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})

                await sleep(5000)
                let bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                let tries = 1

                while(bobLinks.length < 1 && tries < 20) {
                    await sleep(1000)
                    bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                    tries++
                }
                
                expect(bobLinks.length).toBe(1)
            })
        })
    }
}