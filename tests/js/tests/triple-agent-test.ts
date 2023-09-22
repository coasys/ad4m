import { Link, Perspective, LinkExpression, ExpressionProof, LinkQuery, PerspectiveState, NeighbourhoodProxy, PerspectiveUnsignedInput, PerspectiveProxy, PerspectiveHandle } from "@perspect3vism/ad4m";
import fs from "fs";
import { TestContext } from './integration.test'
import { sleep } from '../utils/utils'
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

                await sleep(1000)

                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})

                await sleep(1000)

                let bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                let tries = 1

                while(bobLinks.length < 10 && tries < 20) {
                    console.log("Bob retrying getting links...");
                    await sleep(1000)
                    bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                    tries++
                }
                
                expect(bobLinks.length).to.be.equal(10)

                await bob.perspective.addLink(bobP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})

                let jimLinks = await jim.perspective.queryLinks(jimP1!.uuid, new LinkQuery({source: 'root'}))
                let jimRetries = 1

                while(jimLinks.length < 20 && jimRetries < 20) {
                    console.log("Jim retrying getting links...");
                    await sleep(1000)
                    jimLinks = await jim.perspective.queryLinks(jimP1!.uuid, new LinkQuery({source: 'root'}))
                    jimRetries++
                }
                
                expect(jimLinks.length).to.be.equal(20)

                //Alice bob and jim all collectively add 10 links and then check can be received by all agents
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'root', target: 'test://test'})
                await jim.perspective.addLink(jimP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'root', target: 'test://test'})
                await jim.perspective.addLink(jimP1.uuid, {source: 'root', target: 'test://test'})
                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})
                await bob.perspective.addLink(bobP1.uuid, {source: 'root', target: 'test://test'})
                await jim.perspective.addLink(jimP1.uuid, {source: 'root', target: 'test://test'})
                await jim.perspective.addLink(jimP1.uuid, {source: 'root', target: 'test://test'})

                let aliceLinks = await alice.perspective.queryLinks(aliceP1!.uuid, new LinkQuery({source: 'root'}))
                tries = 1

                while(aliceLinks.length < 30 && tries < 20) {
                    console.log("Alice retrying getting links...");
                    await sleep(1000)
                    aliceLinks = await alice.perspective.queryLinks(aliceP1!.uuid, new LinkQuery({source: 'root'}))
                    tries++
                }
                
                expect(aliceLinks.length).to.be.equal(30)




                bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                tries = 1

                while(bobLinks.length < 30 && tries < 20) {
                    console.log("Bob retrying getting links...");
                    await sleep(1000)
                    bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                    tries++
                }
                
                expect(bobLinks.length).to.be.equal(30)




                jimLinks = await jim.perspective.queryLinks(jimP1!.uuid, new LinkQuery({source: 'root'}))
                tries = 1

                while(jimLinks.length < 30 && tries < 20) {
                    console.log("Jim retrying getting links...");
                    await sleep(1000)
                    jimLinks = await jim.perspective.queryLinks(jimP1!.uuid, new LinkQuery({source: 'root'}))
                    tries++
                }
                
                expect(jimLinks.length).to.be.equal(30)
                
        })
    }
}