import { Link, Perspective, LinkExpression, ExpressionProof, LinkQuery, PerspectiveState, NeighbourhoodProxy, PerspectiveUnsignedInput, PerspectiveProxy, PerspectiveHandle } from "@coasys/ad4m";
import { TestContext } from './integration.test'
import { sleep } from "../utils/utils";
import fs from "fs";
import { v4 as uuidv4 } from 'uuid';
import { expect } from "chai";

const DIFF_SYNC_OFFICIAL = fs.readFileSync("./scripts/perspective-diff-sync-hash").toString();
let aliceP1: null | PerspectiveProxy = null;
let bobP1: null | PerspectiveHandle = null;

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
                expect(perspective?.neighbourhood!.data.linkLanguage).to.be.equal(socialContext.address);
                expect(perspective?.neighbourhood!.data.meta.links.length).to.be.equal(1);
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
                expect(bobP1!.neighbourhood!.data.linkLanguage).to.be.equal(socialContext.address);
                expect(bobP1!.neighbourhood!.data.meta.links.length).to.be.equal(0);
            })

            it('shared link created by Alice received by Bob', async () => {
                const alice = testContext.alice
                const bob = testContext.bob

                const aliceP1 = await alice.perspective.add("friends")
                const socialContext = await alice.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Alice's neighbourhood with Bob test shared links"}));
                const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(aliceP1.uuid, socialContext.address, new Perspective())

                let bobP1 = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);

                await testContext.makeAllNodesKnown()
                expect(bobP1!.state).to.be.oneOf([PerspectiveState.LinkLanguageInstalledButNotSynced, PerspectiveState.Synced]);

                await sleep(1000)

                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})

                await sleep(1000)

                let bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                let tries = 1

                while(bobLinks.length < 1 && tries < 20) {
                    console.log("Bob retrying getting links...");
                    await sleep(1000)
                    bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                    tries++
                }

                expect(bobLinks.length).to.be.equal(1)
                expect(bobLinks[0].data.target).to.be.equal('test://test')
                expect(bobLinks[0].proof.valid).to.be.true;
            })


            it('local link created by Alice NOT received by Bob', async () => {
                const alice = testContext.alice
                const bob = testContext.bob

                aliceP1 = await alice.perspective.add("friends")
                const socialContext = await alice.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Alice's neighbourhood with Bob test local links"}));
                const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(aliceP1.uuid, socialContext.address, new Perspective())
                console.log("neighbourhoodUrl", neighbourhoodUrl);
                bobP1 = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);

                await testContext.makeAllNodesKnown()

                await sleep(1000)

                await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'}, 'local')

                await sleep(1000)

                let bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                let tries = 1

                while(bobLinks.length < 1 && tries < 5) {
                    console.log("Bob retrying getting NOT received links...");
                    await sleep(1000)
                    bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                    tries++
                }

                expect(bobLinks.length).to.be.equal(0)
            })

            it('stress test - Bob receives 1500 links created rapidly by Alice', async () => {
                const alice = testContext.alice
                const bob = testContext.bob

                aliceP1 = await alice.perspective.add("friends")
                const socialContext = await alice.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Alice's neighbourhood with Bob stress test"}));
                const neighbourhoodUrl = await alice.neighbourhood.publishFromPerspective(aliceP1.uuid, socialContext.address, new Perspective())
                console.log("neighbourhoodUrl", neighbourhoodUrl);
                bobP1 = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);

                await testContext.makeAllNodesKnown()

                await sleep(1000)

                // Create 1500 links as fast as possible
                //const linkPromises = []
                for(let i = 0; i < 1500; i++) {
                    console.log("Alice adding link ", i)
                    const link = await alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: `test://test/${i}`})
                    console.log("Link expression:", link)
                }
                //await Promise.all(linkPromises)

                console.log("wait 10s")
                await sleep(10000)

                let bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                let tries = 1
                const maxTries = 120 // 2 minutes with 1 second sleep

                while(bobLinks.length < 1500 && tries < maxTries) {
                    console.log(`Bob retrying getting links... Got ${bobLinks.length}/1500`);
                    await sleep(1000)
                    bobLinks = await bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
                    tries++
                }

                expect(bobLinks.length).to.be.equal(1500)
                // Verify a few random links to ensure data integrity
                expect(bobLinks.some(link => link.data.target === 'test://test/0')).to.be.true
                expect(bobLinks.some(link => link.data.target === 'test://test/749')).to.be.true
                expect(bobLinks.some(link => link.data.target === 'test://test/1499')).to.be.true
                bobLinks.forEach(link => {
                    expect(link.proof.valid).to.be.true
                })
            })

            it('can delete neighbourhood', async () => {
                const alice = testContext.alice;
                const bob = testContext.bob;

                const deleteNeighbourhood = await alice.perspective.remove(aliceP1!.uuid);
                expect(deleteNeighbourhood.perspectiveRemove).to.be.true;

                const bobDeleteNeighbourhood = await bob.perspective.remove(bobP1!.uuid);
                expect(bobDeleteNeighbourhood.perspectiveRemove).to.be.true;

                const perspectives = await alice.perspective.all();
            })

            // it('can get the correct state change signals', async () => {
            //     const aliceP1 = await testContext.alice.perspective.add("state-changes")
            //     expect(aliceP1.state).to.be.equal(PerspectiveState.Private);

            //     const socialContext = await testContext.alice.languages.applyTemplateAndPublish(DIFF_SYNC_OFFICIAL, JSON.stringify({uid: uuidv4(), name: "Alice's neighbourhood with Bob"}));
            //     expect(socialContext.name).to.be.equal("Alice's neighbourhood with Bob");
            //     const neighbourhoodUrl = await testContext.alice.neighbourhood.publishFromPerspective(aliceP1.uuid, socialContext.address, new Perspective())

            //     let aliceSyncChangeCalls = 0;
            //     let aliceSyncChangeData = null;
            //     const aliceSyncChangeHandler = (payload: PerspectiveState) => {
            //         aliceSyncChangeCalls += 1;
            //         //@ts-ignore
            //         aliceSyncChangeData = payload;
            //         return null;
            //     };

            //     aliceP1.addSyncStateChangeListener(aliceSyncChangeHandler);

            //     await testContext.alice.perspective.addLink(aliceP1.uuid, {source: 'root', target: 'test://test'})

            //     let bobSyncChangeCalls = 0;
            //     let bobSyncChangeData = null;
            //     const bobSyncChangeHandler = (payload: PerspectiveState) => {
            //         console.log("bob got new state", payload);
            //         bobSyncChangeCalls += 1;
            //         //@ts-ignore
            //         bobSyncChangeData = payload;
            //         return null;
            //     };

            //     let bobHandler = await testContext.bob.neighbourhood.joinFromUrl(neighbourhoodUrl);
            //     let bobP1 = await testContext.bob.perspective.byUUID(bobHandler.uuid);
            //     expect(bobP1?.state).to.be.equal(PerspectiveState.LinkLanguageInstalledButNotSynced);

            //     await bobP1!.addSyncStateChangeListener(bobSyncChangeHandler);

            //     //These next assertions are flaky since they depend on holochain not syncing right away, which most of the time is the case

            //     let bobLinks = await testContext.bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
            //     let tries = 1

            //     while(bobLinks.length < 1 && tries < 300) {
            //         await sleep(1000)
            //         bobLinks = await testContext.bob.perspective.queryLinks(bobP1!.uuid, new LinkQuery({source: 'root'}))
            //         tries++
            //     }

            //     expect(bobLinks.length).to.be.equal(1)

            //     await sleep(5000);

            //     // expect(aliceSyncChangeCalls).to.be.equal(2);
            //     // expect(aliceSyncChangeData).to.be.equal(PerspectiveState.Synced);

            //     expect(bobSyncChangeCalls).to.be.equal(1);
            //     expect(bobSyncChangeData).to.be.equal(PerspectiveState.Synced);
            // })


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
                    await sleep(5000)
                    const bobP1Handle = await bob.neighbourhood.joinFromUrl(neighbourhoodUrl);
                    const bobP1 = await bob.perspective.byUUID(bobP1Handle.uuid)
                    await testContext.makeAllNodesKnown()

                    aliceNH = aliceP1.getNeighbourhoodProxy()
                    bobNH = bobP1!.getNeighbourhoodProxy()
                    aliceDID = (await alice.agent.me()).did
                    bobDID = (await bob.agent.me()).did
                    await sleep(5000)
                })

                it('they see each other in `otherAgents`', async () => {
                    await sleep(10000);
                    const aliceAgents = await aliceNH!.otherAgents()
                    console.log("alice agents", aliceAgents);
                    const bobAgents = await bobNH!.otherAgents()
                    console.log("bob agents", bobAgents);
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
                    link.proof.invalid = true;
                    link.proof.valid = false;
                    const testPerspective = new Perspective([link])
                    await aliceNH!.setOnlineStatus(testPerspective)
                    await bobNH!.setOnlineStatus(testPerspective)

                    const aliceOnline = await aliceNH!.onlineAgents()
                    const bobOnline = await bobNH!.onlineAgents()
                    expect(aliceOnline.length).to.be.equal(1)
                    expect(aliceOnline[0].did).to.be.equal(bobDID)
                    console.log(aliceOnline[0].status);
                    expect(aliceOnline[0].status.data.links).to.deep.equal(testPerspective.links)

                    expect(bobOnline.length).to.be.equal(1)
                    expect(bobOnline[0].did).to.be.equal(aliceDID)
                    expect(bobOnline[0].status.data.links).to.deep.equal(testPerspective.links)


                    await aliceNH!.setOnlineStatusU(PerspectiveUnsignedInput.fromLink(new Link({
                        source: "test://source",
                        target: "test://target"
                    })))

                    const bobOnline2 = await bobNH!.onlineAgents()

                    expect(bobOnline2.length).to.be.equal(1)
                    expect(bobOnline2[0].did).to.be.equal(aliceDID)
                    expect(bobOnline2[0].status.data.links[0].data.source).to.equal("test://source")
                    expect(bobOnline2[0].status.data.links[0].data.target).to.equal("test://target")
                    expect(bobOnline2[0].status.data.links[0].proof.valid).to.be.true
                    // TODO: Signature check for the whole perspective is broken
                    // Got to fix that and add back this assertion
                    //expect(bobOnline2[0].status.proof.valid).to.be.true

                })

                it('they can send signals via `sendSignal` and receive callbacks via `addSignalHandler`', async () => {
                    let aliceCalls = 0;
                    let aliceData = null;
                    const aliceHandler = async (payload: Perspective) => {
                        aliceCalls += 1;
                        //@ts-ignore
                        aliceData = payload;
                    };
                    aliceNH!.addSignalHandler(aliceHandler)

                    let bobCalls = 0;
                    let bobData = null;
                    const bobHandler = async (payload: Perspective) => {
                        bobCalls += 1;
                        //@ts-ignore
                        bobData = payload;
                    };
                    bobNH!.addSignalHandler(bobHandler)

                    let link = new LinkExpression()
                    link.author = aliceDID;
                    link.timestamp = new Date().toISOString();
                    link.data = new Link({source: "alice", target: "bob", predicate: "signal"});
                    link.proof = new ExpressionProof("sig", "key");
                    const aliceSignal = new Perspective([link])

                    await aliceNH!.sendSignal(bobDID!, aliceSignal)

                    await sleep(1000)

                    expect(bobCalls).to.be.equal(1)
                    expect(aliceCalls).to.be.equal(0)

                    link.proof.invalid = true;
                    link.proof.valid = false;
                    //@ts-ignore
                    expect(bobData.data.links).to.deep.equal(aliceSignal.links)


                    let link2 = new Link({source: "bob", target: "alice", predicate: "signal"});
                    const bobSignal = new PerspectiveUnsignedInput([link2])

                    await bobNH!.sendBroadcastU(bobSignal)

                    await sleep(1000)

                    expect(aliceCalls).to.be.equal(1)

                    //@ts-ignore
                    expect(aliceData.data.links[0].data).to.deep.equal(link2)
                })
            })
        })
    }
}
