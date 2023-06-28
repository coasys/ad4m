import { Link, Perspective, LinkExpression, ExpressionProof, LinkQuery, PerspectiveState, NeighbourhoodProxy, PerspectiveUnsignedInput } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from "./sleep";
import fs from "fs";
import { v4 as uuidv4 } from 'uuid';
import { expect } from "chai";

const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/gun-perspective-diff-sync-hash").toString();

export default function gunDbTests(testContext: TestContext) {
    return () => {
        describe('Neighbourhood', () => {
            it('can install and query gun-db language', async () => {
                const ad4mClient = testContext.alice!;

                const create = await ad4mClient!.perspective.add("publish-test");
                expect(create.name).to.be.equal("publish-test");
                expect(create.neighbourhood).to.be.null;
                expect(create.state).to.be.equal(PerspectiveState.Private);

                //Create unique perspective-diff-sync to simulate real scenario
                const socialContext = await ad4mClient.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Alice's perspective-diff-sync"}));
                expect(socialContext.name).to.be.equal("Alice's perspective-diff-sync");

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
                expect(publishPerspective.split("://").length).to.be.equal(2);

                const perspective = await ad4mClient.perspective.byUUID(create.uuid);
                expect(perspective?.neighbourhood).not.to.be.undefined;
                expect(perspective?.neighbourhood!.linkLanguage).to.be.equal(socialContext.address);
                expect(perspective?.neighbourhood!.meta.links.length).to.be.equal(1);
                expect(perspective?.state).to.be.equal(PerspectiveState.Synced);

                const addLink = await perspective?.add({source: "src", target: "target", predicate: "pred"});
                console.log("addLink", addLink);

                await sleep(10000);
            })
        })
    }
}
