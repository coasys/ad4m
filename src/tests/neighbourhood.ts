import path from "path"
import { Link, Perspective, LinkExpression, ExpressionProof } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'


export default function neighbourhoodTests(testContext: TestContext) {
    return () => {
        describe('Neighbourhood', () => {
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
        })
    }
}