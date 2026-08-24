import { Link, Perspective, LinkExpression, ExpressionProof, LinkQuery, PerspectiveState, NeighbourhoodProxy, PerspectiveUnsignedInput, PerspectiveProxy, PerspectiveHandle } from "@coasys/ad4m";
import fs from "fs";
import { TestContext } from './test-context'
import { pollUntil } from '../utils/utils'
import { expect } from "chai";
import { v4 as uuidv4 } from 'uuid';

const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();

export default function tripleAgentTests(testContext: TestContext) {
    return () => {
        it("three agents can join and use a neighbourhood", async () => {
                const alice = testContext.alice
                const bob = testContext.bob
                const jim = testContext.jim

                const aliceP1 = await alice.perspective.add("three-agents")
                const socialContext = await alice.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Alice's neighbourhood with Bob"}));
                expect(socialContext.name).to.be.equal("Alice's neighbourhood with Bob");
                const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(aliceP1.uuid, socialContext.address, new Perspective())

                let bobP1 = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);
                let jimP1 = await jim.neighbourhood.joinFromUrl(neighbourhoodUrl);

                await testContext.makeAllThreeNodesKnown()
                
                expect(bobP1!.name).not.to.be.undefined;
                expect(bobP1!.sharedUrl).to.be.equal(neighbourhoodUrl)
                expect(bobP1!.neighbourhood).not.to.be.undefined;;
                expect(bobP1!.neighbourhood!.linkLanguage).to.be.equal(socialContext.address);
                expect(bobP1!.neighbourhood!.meta.links.length).to.be.equal(0);

                expect(jimP1!.name).not.to.be.undefined;
                expect(jimP1!.sharedUrl).to.be.equal(neighbourhoodUrl)
                expect(jimP1!.neighbourhood).not.to.be.undefined;;
                expect(jimP1!.neighbourhood!.linkLanguage).to.be.equal(socialContext.address);
                expect(jimP1!.neighbourhood!.meta.links.length).to.be.equal(0);

                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})

                let bobLinks: any[] = [];
                await pollUntil(async () => {
                    bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'ad4m://root'}));
                    console.log(`Bob has ${bobLinks.length}/10 links`);
                    return bobLinks.length >= 10;
                }, { timeoutMs: 20000, intervalMs: 1000, label: "bob receives 10 links from alice" });
                
                expect(bobLinks.length).to.be.equal(10)

                await bob.perspective.addLink(bobP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})

                let jimLinks: any[] = [];
                await pollUntil(async () => {
                    jimLinks = await jim.perspective.queryLinks(jimP1!.uuid, new LinkQuery({source: 'ad4m://root'}));
                    console.log(`Jim has ${jimLinks.length}/20 links`);
                    return jimLinks.length >= 20;
                }, { timeoutMs: 20000, intervalMs: 1000, label: "jim receives 20 links" });
                
                expect(jimLinks.length).to.be.equal(20)

                //Alice bob and jim all collectively add 10 links and then check can be received by all agents
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await jim.perspective.addLink(jimP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await jim.perspective.addLink(jimP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await jim.perspective.addLink(jimP1.uuid, {source: 'ad4m://root', target: 'test://test'})
                await jim.perspective.addLink(jimP1.uuid, {source: 'ad4m://root', target: 'test://test'})

                let aliceLinks: any[] = [];
                await pollUntil(async () => {
                    aliceLinks = await alice.perspective.queryLinks(aliceP1!.uuid, new LinkQuery({source: 'ad4m://root'}));
                    console.log(`Alice has ${aliceLinks.length}/30 links`);
                    return aliceLinks.length >= 30;
                }, { timeoutMs: 20000, intervalMs: 1000, label: "alice receives 30 links" });
                
                expect(aliceLinks.length).to.be.equal(30)




                await pollUntil(async () => {
                    bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'ad4m://root'}));
                    console.log(`Bob has ${bobLinks.length}/30 links`);
                    return bobLinks.length >= 30;
                }, { timeoutMs: 20000, intervalMs: 1000, label: "bob receives 30 links" });
                
                expect(bobLinks.length).to.be.equal(30)




                await pollUntil(async () => {
                    jimLinks = await jim.perspective.queryLinks(jimP1!.uuid, new LinkQuery({source: 'ad4m://root'}));
                    console.log(`Jim has ${jimLinks.length}/30 links`);
                    return jimLinks.length >= 30;
                }, { timeoutMs: 20000, intervalMs: 1000, label: "jim receives 30 links" });
                
                expect(jimLinks.length).to.be.equal(30)
                
        })
    }
}