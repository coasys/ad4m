import path from "path"
import { Link, Perspective, LinkExpression, ExpressionProof, LinkQuery, LanguageMetaInput } from "@perspect3vism/ad4m";
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
                const socialContext = await ad4mClient.languages.publish(
                    path.join(__dirname, "../test-temp/languages/social-context/build/bundle.js"), 
                    new LanguageMetaInput("Alice's social-context")
                )
                expect(socialContext.name).toBe("Alice's social-context");

                let link = new LinkExpression()
                link.author = "did:test";
                link.timestamp = new Date().toISOString();
                link.data = new Link({source: "src", target: "target", predicate: "pred"});
                link.proof = new ExpressionProof("sig", "key");
                const publishPerspective = await ad4mClient.neighbourhood.publishFromPerspective(create.uuid, socialContext.address, 
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
                expect(join.neighbourhood!.linkLanguage).toBe(socialContext.address);
                expect(join.neighbourhood!.meta.links.length).toBe(1);
            })

            it('can be created by Alice and joined by Bob', async () => {
                const alice = testContext.alice
                const bob = testContext.bob

                const aliceP1 = await alice.perspective.add("friends")
                const socialContext = await alice.languages.publish(
                    path.join(__dirname, "../test-temp/languages/social-context/build/bundle.js"), 
                    new LanguageMetaInput("Alice's neighbourhood with Bob", "")

                )
                expect(socialContext.name).toBe("Alice's neighbourhood with Bob");
                const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(aliceP1.uuid, socialContext.address, new Perspective())

                let bobP1 = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);

                await testContext.makeAllNodesKnown()
                
                expect(bobP1).toBeTruthy()
                expect(bobP1!.name).toBeDefined()
                expect(bobP1!.sharedUrl).toEqual(neighbourhoodUrl)
                expect(bobP1!.neighbourhood).toBeDefined();
                expect(bobP1!.neighbourhood!.linkLanguage).toBe(socialContext.address);
                expect(bobP1!.neighbourhood!.meta.links.length).toBe(0);

                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})

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