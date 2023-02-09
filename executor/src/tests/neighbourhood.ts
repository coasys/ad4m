import { Link, Perspective, LinkExpression, ExpressionProof, LinkQuery, PerspectiveState, NeighbourhoodProxy } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from "./sleep";
import fs from "fs";
import { v4 as uuidv4 } from 'uuid';
import { expect } from "chai";

const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();

export default function neighbourhoodTests(testContext: TestContext) {
    return () => {
        describe('Neighbourhood', () => {
            it('can publish and join locally @alice', async () => {
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
            })

            it('can be created by Alice and joined by Bob', async () => {
                const alice = testContext.alice
                const bob = testContext.bob

                const aliceP1 = await alice.perspective.add("friends")
                const socialContext = await alice.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Alice's neighbourhood with Bob"}));
                expect(socialContext.name).to.be.equal("Alice's neighbourhood with Bob");
                const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(aliceP1.uuid, socialContext.address, new Perspective())

                let bobP1 = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);

                await testContext.makeAllNodesKnown()
                
                expect(bobP1!.name).not.to.be.undefined;
                expect(bobP1!.sharedUrl).to.be.equal(neighbourhoodUrl)
                expect(bobP1!.neighbourhood).not.to.be.undefined;;
                expect(bobP1!.neighbourhood!.linkLanguage).to.be.equal(socialContext.address);
                expect(bobP1!.neighbourhood!.meta.links.length).to.be.equal(0);
                expect(bobP1!.state).to.be.oneOf([PerspectiveState.LinkLanguageInstalledButNotSynced, PerspectiveState.Synced]);

                await sleep(5000)

                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})

                await sleep(5000)

                let bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                let tries = 1

                while(bobLinks.length < 1 && tries < 20) {
                    await sleep(1000)
                    bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                    tries++
                }
                
                expect(bobLinks.length).to.be.equal(1)
            })

            describe('with set up and joined NH for Telepresence', async () => {
                let aliceNH: NeighbourhoodProxy|undefined
                let bobNH: NeighbourhoodProxy|undefined
                let aliceDID: string|undefined
                let bobDID: string|undefined

                before(async () => {
                    const alice = testContext.alice
                    const bob = testContext.bob

                    const aliceP1 = await alice.perspective.add("telepresence")
                    const linkLang = await alice.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Alice's neighbourhood for Telepresence"}));
                    const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(aliceP1.uuid, linkLang.address, new Perspective())
                    const bobP1Handle = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);
                    const bobP1 = await bob.perspective.byUUID(bobP1Handle.uuid)
                    await testContext.makeAllNodesKnown()
                    
                    aliceNH = aliceP1.getNeighbourhoodProxy()
                    bobNH = bobP1!.getNeighbourhoodProxy()
                    aliceDID = (await alice.agent.me()).did
                    bobDID = (await bob.agent.me()).did
                })

                it('they see each other in `otherAgents`', async () => {
                    const aliceAgents = await aliceNH!.otherAgents()
                    const bobAgents = await bobNH!.otherAgents()
                    expect(aliceAgents.length).to.be.equal(1)
                    expect(aliceAgents[0]).to.be.equal(bobDID)
                    expect(bobAgents.length).to.be.equal(1)
                    expect(bobAgents[0]).to.be.equal(aliceDID)
                })

                it('they can set their online status and see each others online status in `onlineAgents`', async () => {
                    let link = new LinkExpression()
                    link.author = "did:test";
                    link.timestamp = new Date().toISOString();
                    link.data = new Link({source: "src", target: "target", predicate: "pred"});
                    link.proof = new ExpressionProof("sig", "key");
                    const testPerspective = new Perspective([link])
                    await aliceNH!.setOnlineStatus(testPerspective)
                    await bobNH!.setOnlineStatus(testPerspective)
                    const aliceOnline = await aliceNH!.onlineAgents()
                    const bobOnline = await bobNH!.onlineAgents()
                    expect(aliceOnline.length).to.be.equal(1)
                    expect(aliceOnline[0].did).to.be.equal(bobDID)
                    expect(aliceOnline[0].status).to.deep.equal(testPerspective)
                    
                    expect(bobOnline.length).to.be.equal(1)
                    expect(bobOnline[0].did).to.be.equal(aliceDID)
                    expect(bobOnline[0].status).to.deep.equal(testPerspective)
                })

                it('they can send signals via `sendSignal` and receive callbacks via `addSignalHandler`', async () => {
                    const aliceHandler = jest.fn()
                    aliceNH!.addSignalHandler(aliceHandler)
                    const bobHandler = jest.fn()
                    bobNH!.addSignalHandler(bobHandler)

                    let link = new LinkExpression()
                    link.author = aliceDID;
                    link.timestamp = new Date().toISOString();
                    link.data = new Link({source: "aliace", target: "bob", predicate: "signal"});
                    link.proof = new ExpressionProof("sig", "key");
                    const aliceSignal = new Perspective([link])

                    await aliceNH!.sendSignal(bobDID!, aliceSignal)

                    await sleep(1000)

                    expect(bobHandler.mock.calls.length).to.be.equal(1)
                    expect(aliceHandler.mock.calls.length).to.be.equal(0)
                    expect(bobHandler.mock.calls[0][0].data).to.deep.equal(aliceSignal)

                    let link2 = new LinkExpression()
                    link2.author = bobDID;
                    link2.timestamp = new Date().toISOString();
                    link2.data = new Link({source: "bob", target: "alice", predicate: "signal"});
                    link2.proof = new ExpressionProof("sig", "key");
                    const bobSignal = new Perspective([link2])

                    await bobNH!.sendBroadcast(bobSignal)

                    await sleep(1000)

                    expect(aliceHandler.mock.calls.length).to.be.equal(1)
                    expect(aliceHandler.mock.calls[0][0].data).to.deep.equal(bobSignal)
                })

        })
    }
}