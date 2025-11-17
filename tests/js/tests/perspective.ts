import { Ad4mClient, Link, LinkQuery, PerspectiveProxy, PerspectiveState } from "@coasys/ad4m";
import { TestContext } from './integration.test'
import { expect } from "chai";
import * as sinon from "sinon";
import { sleep } from "../utils/utils";

export default function perspectiveTests(testContext: TestContext) {
    return  () => {
        describe('Perspectives', () => {
            it('can create, get & delete perspective', async () => {
                const ad4mClient = testContext.ad4mClient!

                let perspectiveCount = (await ad4mClient.perspective.all()).length

                const create = await ad4mClient.perspective.add("test");
                expect(create.name).to.equal("test");

                const get = await ad4mClient!.perspective.byUUID(create.uuid);
                expect(get!.name).to.equal("test");

                const update = await ad4mClient!.perspective.update(create.uuid, "updated-test");
                expect(update.name).to.equal("updated-test");

                const getUpdated = await ad4mClient!.perspective.byUUID(update.uuid );
                expect(getUpdated!.name).to.equal("updated-test");

                const perspectives = await ad4mClient.perspective.all();
                expect(perspectives.length).to.equal(perspectiveCount + 1);

                const perspectiveSnaphot = await ad4mClient.perspective.snapshotByUUID(update.uuid );
                expect(perspectiveSnaphot!.links.length).to.equal(0);

                const deletePerspective = await ad4mClient!.perspective.remove(update.uuid );
                expect(deletePerspective.perspectiveRemove).to.be.true;

                const getDeleted = await ad4mClient!.perspective.byUUID(update.uuid );
                expect(getDeleted).to.be.null;
            })

            it('can CRUD local perspective links', async () => {
                const ad4mClient = testContext.ad4mClient!;

                const create = await ad4mClient.perspective.add("test-crud");
                expect(create.name).to.equal("test-crud");

                const linkAdd = await create.add(new Link({
                    source: "test://test-source",
                    predicate: "test://test-predicate",
                    target: "test://test-target"
                }));

                const links = await create.get({} as LinkQuery);
                expect(links.length).to.equal(1);

                await create.remove(linkAdd);

                const linksPostDelete = await create.get({} as LinkQuery);
                expect(linksPostDelete.length).to.equal(0);

                const snapshot = await create.snapshot();
                expect(snapshot.links.length).to.equal(0);
            })

            it('can CRUD local perspective links with local link method', async () => {
                const ad4mClient = testContext.ad4mClient!;

                const create = await ad4mClient.perspective.add("test-crud");
                expect(create.name).to.equal("test-crud");

                const linkAdd = await create.add(new Link({
                    source: "test://test-source",
                    predicate: "test://test-predicate",
                    target: "test://test-target"
                }), 'local');

                const links = await create.get({} as LinkQuery);
                expect(links.length).to.equal(1);
                expect(links[0].status).to.equal('LOCAL')

                await create.remove(linkAdd);

                const linksPostDelete = await create.get({} as LinkQuery);
                expect(linksPostDelete.length).to.equal(0);

                const snapshot = await create.snapshot();
                expect(snapshot.links.length).to.equal(0);
            })

            it('can make mutations using perspective addLinks(), removeLinks() & linkMutations()', async () => {
                const ad4mClient = testContext.ad4mClient!;

                const create = await ad4mClient.perspective.add("test-mutations");
                expect(create.name).to.equal("test-mutations");

                const links = [
                    new Link({
                        source: "test://test-source",
                        predicate: "test://test-predicate",
                        target: "test://test-target"
                    }),
                    new Link({
                        source: "test://test-source2",
                        predicate: "test://test-predicate2",
                        target: "test://test-target2"
                    })
                ];
                const linkAdds = await create.addLinks(links);
                expect(linkAdds.length).to.equal(2);

                const linksPostAdd = await create.get({} as LinkQuery);
                expect(linksPostAdd.length).to.equal(2);

                const linkRemoves = await create.removeLinks(linkAdds);
                expect(linkRemoves.length).to.equal(2);

                const linksPostRemove = await create.get({} as LinkQuery);
                expect(linksPostRemove.length).to.equal(0);

                const addTwoMore = await create.addLinks(links);

                const linkMutation = {
                    additions: links,
                    removals: addTwoMore
                };
                const linkMutations = await create.linkMutations(linkMutation);
                expect(linkMutations.additions.length).to.equal(2);
                expect(linkMutations.removals.length).to.equal(2);

                const linksPostMutation = await create.get({} as LinkQuery);
                expect(linksPostMutation.length).to.equal(2);
            })

            it(`doesn't error when duplicate entries passed to removeLinks`, async () => {
                const ad4mClient = testContext.ad4mClient!;
                const perspective = await ad4mClient.perspective.add('test-duplicate-link-removal');
                expect(perspective.name).to.equal('test-duplicate-link-removal');

                // create link
                const link = { source: 'root', predicate: 'p', target: 'abc' };
                const addLink = await perspective.add(link);
                expect(addLink.data.target).to.equal("abc");

                // get link expression
                const linkExpression = (await perspective.get(new LinkQuery(link)))[0];
                expect(linkExpression.data.target).to.equal("abc");

                // attempt to remove link twice (currently errors and prevents further execution of code)
                await perspective.removeLinks([linkExpression, linkExpression])

                // check link is removed
                const links = await perspective.get(new LinkQuery(link));
                expect(links.length).to.equal(0);
            })

            it('test local perspective links - time query', async () => {
                const ad4mClient = testContext.ad4mClient!

                const create = await ad4mClient!.perspective.add("test-links-time");
                expect(create.name).to.equal("test-links-time");

                let addLink = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target", predicate: "lang://predicate"}));
                await sleep(10);
                let addLink2 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target2", predicate: "lang://predicate"}));
                await sleep(10);
                let addLink3 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target3", predicate: "lang://predicate"}));
                await sleep(10);
                let addLink4 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target4", predicate: "lang://predicate"}));
                await sleep(10);
                let addLink5 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target5", predicate: "lang://predicate"}));

                // Get all the links
                let queryLinksAll = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(new Date(addLink.timestamp).getTime()), untilDate: new Date()}));
                expect(queryLinksAll.length).to.equal(5);


                // Get 3 of the links in descending order
                let queryLinksAsc = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(), untilDate: new Date("August 19, 1975 23:15:30"), limit: 3}));
                expect(queryLinksAsc.length).to.equal(3);
                expect(queryLinksAsc[0].data.target).to.equal(addLink5.data.target)
                expect(queryLinksAsc[1].data.target).to.equal(addLink4.data.target)
                expect(queryLinksAsc[2].data.target).to.equal(addLink3.data.target)

                // Get 3 of the links in descending order
                let queryLinksDesc = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date("August 19, 1975 23:15:30"), untilDate: new Date(), limit: 3}));
                expect(queryLinksDesc.length).to.equal(3);
                expect(queryLinksDesc[0].data.target).to.equal(addLink.data.target)
                expect(queryLinksDesc[1].data.target).to.equal(addLink2.data.target)
                expect(queryLinksDesc[2].data.target).to.equal(addLink3.data.target)


                //Test can get all links but first by querying from second timestamp
                let queryLinks = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(new Date(addLink2.timestamp).getTime() - 1), untilDate: new Date()}));
                expect(queryLinks.length).to.equal(4);

                //Test can get links limited
                let queryLinksLimited = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(new Date(addLink2.timestamp).getTime() - 1), untilDate: new Date(), limit: 3}));
                expect(queryLinksLimited.length).to.equal(3);

                //Test can get only the first link
                let queryLinksFirst = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({
                    source: "lang://test", fromDate: new Date(addLink.timestamp),
                    untilDate: new Date(new Date(addLink2.timestamp).getTime() - 1)
                }));
                expect(queryLinksFirst.length).to.equal(1);
                expect(queryLinksFirst[0].data.target).to.equal("lang://test-target");
            })

            it('test local perspective links', async () => {
                const ad4mClient = testContext.ad4mClient!

                const create = await ad4mClient!.perspective.add("test-links");
                expect(create.name).to.equal("test-links");

                let addLink = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target", predicate: "lang://predicate"}));
                expect(addLink.data.target).to.equal("lang://test-target");
                expect(addLink.data.source).to.equal("lang://test");

                //Test can get by source, target, predicate
                let queryLinks = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test"}));
                expect(queryLinks.length).to.equal(1);
                expect(queryLinks[0].data.target).to.equal("lang://test-target");
                expect(queryLinks[0].data.source).to.equal("lang://test");

                let queryLinksTarget = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({target: "lang://test-target"}));
                expect(queryLinksTarget.length).to.equal(1);
                expect(queryLinksTarget[0].data.target).to.equal("lang://test-target");
                expect(queryLinksTarget[0].data.source).to.equal("lang://test");

                let queryLinksPredicate = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({predicate: "lang://predicate"}));
                expect(queryLinksPredicate.length).to.equal(1);
                expect(queryLinksPredicate[0].data.target).to.equal("lang://test-target");
                expect(queryLinksPredicate[0].data.source).to.equal("lang://test");

                const perspectiveSnaphot = await ad4mClient.perspective.snapshotByUUID(create.uuid );
                expect(perspectiveSnaphot!.links.length).to.equal(1);

                //Update the link to new link
                const updateLink = await ad4mClient.perspective.updateLink(create.uuid, addLink,
                    new Link({source: "lang://test2", target: "lang://test-target2", predicate: "lang://predicate2"}));
                expect(updateLink.data.target).to.equal("lang://test-target2");
                expect(updateLink.data.source).to.equal("lang://test2");

                const perspectiveSnaphotLinkUpdate = await ad4mClient.perspective.snapshotByUUID(create.uuid );
                expect(perspectiveSnaphotLinkUpdate!.links.length).to.equal(1);

                //Test cannot get old link
                let queryLinksOld = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test"}));
                expect(queryLinksOld.length).to.equal(0);

                //Test can get new link
                let queryLinksUpdated = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test2"}));
                expect(queryLinksUpdated.length).to.equal(1);

                const deleteLink = await ad4mClient!.perspective.removeLink(create.uuid, updateLink);
                expect(deleteLink).to.equal(true);

                let queryLinksDeleted = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test2"}));
                expect(queryLinksDeleted.length).to.equal(0);
            })

            it('subscriptions', async () => {
                const ad4mClient: Ad4mClient = testContext.ad4mClient!

                const perspectiveAdded = sinon.fake()
                ad4mClient.perspective.addPerspectiveAddedListener(perspectiveAdded)
                const perspectiveUpdated = sinon.fake()
                ad4mClient.perspective.addPerspectiveUpdatedListener(perspectiveUpdated)
                const perspectiveRemoved = sinon.fake()
                ad4mClient.perspective.addPerspectiveRemovedListener(perspectiveRemoved)

                const name = "Subscription Test Perspective"
                const p = await ad4mClient.perspective.add(name)
                await sleep(1000)
                expect(perspectiveAdded.calledOnce).to.be.true;
                const pSeenInAddCB = perspectiveAdded.getCall(0).args[0];
                expect(pSeenInAddCB.uuid).to.equal(p.uuid)
                expect(pSeenInAddCB.name).to.equal(p.name)

                const p1 = await ad4mClient.perspective.update(p.uuid , "New Name")
                await sleep(1000)
                expect(perspectiveUpdated.calledOnce).to.be.true;
                const pSeenInUpdateCB = perspectiveUpdated.getCall(0).args[0];
                expect(pSeenInUpdateCB.uuid).to.equal(p1.uuid)
                expect(pSeenInUpdateCB.name).to.equal(p1.name)
                expect(pSeenInUpdateCB.state).to.equal(PerspectiveState.Private)

                const linkAdded = sinon.fake()
                await ad4mClient.perspective.addPerspectiveLinkAddedListener(p1.uuid, [linkAdded])
                const linkRemoved = sinon.fake()
                await ad4mClient.perspective.addPerspectiveLinkRemovedListener(p1.uuid, [linkRemoved])
                const linkUpdated = sinon.fake()
                await ad4mClient.perspective.addPerspectiveLinkUpdatedListener(p1.uuid, [linkUpdated])

                const linkExpression = await ad4mClient.perspective.addLink(p1.uuid , {source: 'root', target: 'lang://123'})
                await sleep(1000)
                expect(linkAdded.called).to.be.true;
                expect(linkAdded.getCall(0).args[0]).to.eql(linkExpression)

                const updatedLinkExpression = await ad4mClient.perspective.updateLink(p1.uuid , linkExpression, {source: 'root', target: 'lang://456'})
                await sleep(1000)
                expect(linkUpdated.called).to.be.true;
                expect(linkUpdated.getCall(0).args[0].newLink).to.eql(updatedLinkExpression)

                const copiedUpdatedLinkExpression = {...updatedLinkExpression}

                await ad4mClient.perspective.removeLink(p1.uuid , updatedLinkExpression)
                await sleep(1000)
                expect(linkRemoved.called).to.be.true;
                //expect(linkRemoved.getCall(0).args[0]).to.eql(copiedUpdatedLinkExpression)
            })

            it('shares subscription between identical prolog queries', async () => {
                const ad4mClient: Ad4mClient = testContext.ad4mClient!
                const p = await ad4mClient.perspective.add("Subscription test")

                const query = 'triple(X, _, "test://target").'
                
                // Create first subscription
                const sub1 = await p.subscribeInfer(query)
                const sub1Id = sub1.id
                const callback1 = sinon.fake()
                sub1.onResult(callback1)

                // Create second subscription with same query
                const sub2 = await p.subscribeInfer(query) 
                const sub2Id = sub2.id
                const callback2 = sinon.fake()
                sub2.onResult(callback2)

                // Assert they got same subscription ID
                expect(sub1Id).to.equal(sub2Id)

                // Wait for the subscriptions to be established
                // it's sending the initial result a couple of times
                // to allow clients to wait and ensure for the subscription to be established
                await sleep(1000)

                // Add a link that matches the query
                await p.add(new Link({
                    source: "test://source",
                    target: "test://target"
                }))

                await sleep(1000)

                // Verify both callbacks were called
                expect(callback1.called).to.be.true
                expect(callback2.called).to.be.true

                // Verify both got same result
                const result1 = callback1.getCall(callback1.callCount - 1).args[0]
                const result2 = callback2.getCall(callback2.callCount - 1).args[0]
                console.log("result1", result1)
                expect(result1).to.deep.equal(result2)
                expect(result1[0].X).to.equal("test://source")
            })

            it('can run Prolog queries', async () => {
                const ad4mClient: Ad4mClient = testContext.ad4mClient!
                const p = await ad4mClient.perspective.add("Prolog test")
                await p.add(new Link({
                    source: "ad4m://root",
                    target: "note-ipfs://Qm123"
                }))
                await p.add(new Link({
                    source: "note-ipfs://Qm123",
                    target: "todo-ontology://is-todo"
                }))

                const result = await p.infer('triple(X, _, "todo-ontology://is-todo").')
                expect(result).not.to.be.false;
                expect(result.length).to.equal(1)
                expect(result[0].X).to.equal('note-ipfs://Qm123')

                expect(await p.infer('reachable("ad4m://root", "todo-ontology://is-todo")')).to.be.true;

                const linkResult = await p.infer('link(X, _, "todo-ontology://is-todo", Timestamp, Author).')
                expect(linkResult).not.to.be.false;
                expect(linkResult.length).to.equal(1)
                expect(linkResult[0].X).to.equal('note-ipfs://Qm123');
                expect(linkResult[0].Timestamp).not.to.be.null;
                expect(linkResult[0].Author).not.to.be.null;
            })
        })

        describe.skip('Batch Operations', () => {
            let proxy: PerspectiveProxy
            let ad4mClient: Ad4mClient
            
            beforeEach(async () => {
                ad4mClient = testContext.ad4mClient!
                proxy = await ad4mClient.perspective.add("batch test");
            })

            it('can create and commit empty batch', async () => {
                const batchId = await proxy.createBatch()
                expect(batchId).to.be.a('string')
                
                const result = await proxy.commitBatch(batchId)
                expect(result.additions).to.be.an('array')
                expect(result.additions.length).to.equal(0)
                expect(result.removals).to.be.an('array')
                expect(result.removals.length).to.equal(0)
            })

            it('can add links in batch', async () => {
                const batchId = await proxy.createBatch()
                
                const link1 = new Link({
                    source: 'test://source1',
                    predicate: 'test://predicate1',
                    target: 'test://target1'
                })
                const link2 = new Link({
                    source: 'test://source2',
                    predicate: 'test://predicate2',
                    target: 'test://target2'
                })

                // Add links to batch
                await proxy.add(link1, 'shared', batchId)
                await proxy.add(link2, 'shared', batchId)

                // Links should not be visible before commit
                let links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(0)

                // Commit batch
                const result = await proxy.commitBatch(batchId)
                expect(result.additions.length).to.equal(2)
                expect(result.removals.length).to.equal(0)

                // Verify links are now visible
                links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(2)
                expect(links.map(l => l.data.target)).to.include('test://target1')
                expect(links.map(l => l.data.target)).to.include('test://target2')
            })

            it('can remove links in batch', async () => {
                // Add some initial links
                const link1 = new Link({
                    source: 'test://source1',
                    predicate: 'test://predicate1',
                    target: 'test://target1'
                })
                const link2 = new Link({
                    source: 'test://source2',
                    predicate: 'test://predicate2',
                    target: 'test://target2'
                })

                const expr1 = await proxy.add(link1)
                const expr2 = await proxy.add(link2)

                // Create batch for removals
                const batchId = await proxy.createBatch()
                
                // Remove links in batch
                await proxy.remove(expr1, batchId)
                await proxy.remove(expr2, batchId)

                // Links should still be visible before commit
                let links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(2)

                // Commit batch
                const result = await proxy.commitBatch(batchId)
                expect(result.additions.length).to.equal(0)
                expect(result.removals.length).to.equal(2)

                // Verify links are now removed
                links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(0)
            })

            it('can mix additions and removals in batch', async () => {
                // Add an initial link
                const link1 = new Link({
                    source: 'test://source1',
                    predicate: 'test://predicate1',
                    target: 'test://target1'
                })
                const expr1 = await proxy.add(link1)

                // Create batch
                const batchId = await proxy.createBatch()

                // Remove existing link and add new one in batch
                await proxy.remove(expr1, batchId)
                const link2 = new Link({
                    source: 'test://source2',
                    predicate: 'test://predicate2',
                    target: 'test://target2'
                })
                await proxy.add(link2, 'shared', batchId)

                // Original state should be unchanged before commit
                let links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(1)
                expect(links[0].data.target).to.equal('test://target1')

                // Commit batch
                const result = await proxy.commitBatch(batchId)
                expect(result.additions.length).to.equal(1)
                expect(result.removals.length).to.equal(1)

                // Verify final state
                links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(1)
                expect(links[0].data.target).to.equal('test://target2')
            })

            it('can update links in batch', async () => {
                // Add an initial link
                const link1 = new Link({
                    source: 'test://source1',
                    predicate: 'test://predicate1',
                    target: 'test://target1'
                })
                const expr1 = await proxy.add(link1)

                // Create batch
                const batchId = await proxy.createBatch()

                // Update link in batch
                const newLink = new Link({
                    source: 'test://source1',
                    predicate: 'test://predicate1',
                    target: 'test://updated-target'
                })
                await proxy.update(expr1, newLink, batchId)

                // Original state should be unchanged before commit
                let links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(1)
                expect(links[0].data.target).to.equal('test://target1')

                // Commit batch
                const result = await proxy.commitBatch(batchId)
                expect(result.additions.length).to.equal(1)
                expect(result.removals.length).to.equal(1)

                // Verify final state
                links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(1)
                expect(links[0].data.target).to.equal('test://updated-target')
            })

            it('can handle multiple batches concurrently', async () => {
                // Create two batches
                const batchId1 = await proxy.createBatch()
                const batchId2 = await proxy.createBatch()

                // Add different links to each batch
                const link1 = new Link({
                    source: 'test://source1',
                    predicate: 'test://predicate1',
                    target: 'test://target1'
                })
                const link2 = new Link({
                    source: 'test://source2',
                    predicate: 'test://predicate2',
                    target: 'test://target2'
                })

                await proxy.add(link1, 'shared', batchId1)
                await proxy.add(link2, 'shared', batchId2)

                // Verify no links are visible yet
                let links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(0)

                // Commit first batch
                const result1 = await proxy.commitBatch(batchId1)
                expect(result1.additions.length).to.equal(1)
                expect(result1.removals.length).to.equal(0)

                // Verify only first link is visible
                links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(1)
                expect(links[0].data.target).to.equal('test://target1')

                // Commit second batch
                const result2 = await proxy.commitBatch(batchId2)
                expect(result2.additions.length).to.equal(1)
                expect(result2.removals.length).to.equal(0)

                // Verify both links are now visible
                links = await proxy.get({} as LinkQuery)
                expect(links.length).to.equal(2)
                expect(links.map(l => l.data.target)).to.include('test://target1')
                expect(links.map(l => l.data.target)).to.include('test://target2')
            })

            it('handles batch operations with addLinks and removeLinks', async () => {
                const batchId = await proxy.createBatch()

                // Add multiple links in one call
                const links = [
                    new Link({
                        source: 'test://source1',
                        predicate: 'test://predicate1',
                        target: 'test://target1'
                    }),
                    new Link({
                        source: 'test://source2',
                        predicate: 'test://predicate2',
                        target: 'test://target2'
                    })
                ]

                await proxy.addLinks(links, 'shared', batchId)

                // Verify links are not visible yet
                let currentLinks = await proxy.get({} as LinkQuery)
                expect(currentLinks.length).to.equal(0)

                // Commit batch
                const result = await proxy.commitBatch(batchId)
                expect(result.additions.length).to.equal(2)
                expect(result.removals.length).to.equal(0)

                // Verify links are now visible
                currentLinks = await proxy.get({} as LinkQuery)
                expect(currentLinks.length).to.equal(2)

                // Create new batch for removal
                const removeBatchId = await proxy.createBatch()

                // Remove multiple links in one call
                await proxy.removeLinks(currentLinks, removeBatchId)

                // Verify links are still visible before commit
                currentLinks = await proxy.get({} as LinkQuery)
                expect(currentLinks.length).to.equal(2)

                // Commit removal batch
                const removeResult = await proxy.commitBatch(removeBatchId)
                expect(removeResult.additions.length).to.equal(0)
                expect(removeResult.removals.length).to.equal(2)

                // Verify all links are removed
                currentLinks = await proxy.get({} as LinkQuery)
                expect(currentLinks.length).to.equal(0)
            })

            it("should support batch operations with executeCommands", async () => {
                const perspective = await ad4mClient.perspective.add("test-batch-execute-commands");
                const batchId = await perspective.createBatch();

                // Execute commands in batch
                await perspective.executeAction(
                    [
                        {
                            action: "addLink",
                            source: "test://source1",
                            predicate: "test://predicate1",
                            target: "test://target1"
                        },
                        {
                            action: "addLink",
                            source: "test://source2",
                            predicate: "test://predicate2",
                            target: "test://target2"
                        }
                    ],
                    "test://expression",
                    [],
                    batchId
                );

                // Verify links are not visible before commit
                let links = await perspective.get(new LinkQuery({}));
                expect(links.length).to.equal(0);

                // Commit batch and verify links are now visible
                const diff = await perspective.commitBatch(batchId);
                expect(diff.additions.length).to.equal(2);
                expect(diff.removals.length).to.equal(0);

                links = await perspective.get({ isMatch: () => true });
                expect(links.length).to.equal(2);

                // Verify link contents
                const link1 = links.find(l => l.data.source === "test://source1");
                if (!link1) throw new Error("Expected to find link1");
                expect(link1.data.predicate).to.equal("test://predicate1");
                expect(link1.data.target).to.equal("test://target1");

                const link2 = links.find(l => l.data.source === "test://source2");
                if (!link2) throw new Error("Expected to find link2");
                expect(link2.data.predicate).to.equal("test://predicate2");
                expect(link2.data.target).to.equal("test://target2");
            });

            it("should support batch operations with multiple commands", async () => {
                const perspective = await ad4mClient.perspective.add("test-batch-multiple-commands");
                
                // Add a link outside the batch first
                await perspective.executeAction(
                    [{
                        action: "addLink",
                        source: "test://source0",
                        predicate: "test://predicate0",
                        target: "test://target0"
                    }],
                    "test://expression",
                    []
                );

                const batchId = await perspective.createBatch();

                // Execute multiple commands in batch including a remove
                await perspective.executeAction(
                    [
                        {
                            action: "removeLink",
                            source: "test://source0",
                            predicate: "test://predicate0",
                            target: "test://target0"
                        },
                        {
                            action: "addLink",
                            source: "test://source1",
                            predicate: "test://predicate1",
                            target: "test://target1"
                        },
                        {
                            action: "setSingleTarget",
                            source: "test://source2",
                            predicate: "test://predicate2",
                            target: "test://target2"
                        }
                    ],
                    "test://expression",
                    [],
                    batchId
                );

                // Verify state before commit
                let links = await perspective.get(new LinkQuery({}));
                expect(links.length).to.equal(1); // Only the initial link
                expect(links[0].data.source).to.equal("test://source0");

                // Commit batch and verify final state
                const diff = await perspective.commitBatch(batchId);
                expect(diff.additions.length).to.equal(2); // New links
                expect(diff.removals.length).to.equal(1); // Removed initial link

                links = await perspective.get(new LinkQuery({}));
                expect(links.length).to.equal(2); // Two new links

                // Verify final link contents
                const link1 = links.find(l => l.data.source === "test://source1");
                if (!link1) throw new Error("Expected to find link1");
                expect(link1.data.predicate).to.equal("test://predicate1");
                expect(link1.data.target).to.equal("test://target1");

                const link2 = links.find(l => l.data.source === "test://source2");
                if (!link2) throw new Error("Expected to find link2");
                expect(link2.data.predicate).to.equal("test://predicate2");
                expect(link2.data.target).to.equal("test://target2");

                // Verify removed link is gone
                const removedLink = links.find(l => l.data.source === "test://source0");
                expect(removedLink).to.be.undefined;
            });
        })

        describe('PerspectiveProxy', () => {
            let proxy: PerspectiveProxy
            let ad4mClient: Ad4mClient
            before(async () => {
                ad4mClient = testContext.ad4mClient!
                proxy = await ad4mClient.perspective.add("proxy test");
            })

            it('can do link CRUD', async () => {
                const all = new LinkQuery({})
                const testLink = new Link({
                    source: 'test://source',
                    predicate: 'test://predicate',
                    target: 'test://target'
                })

                expect(await proxy.get(all)).to.eql([])

                await proxy.add(testLink)
                let links = await proxy.get(all)
                expect(links.length).to.equal(1)

                let link = new Link(links[0].data)
                expect(link).to.eql(testLink)

                const updatedLink = new Link({
                    source: link.source,
                    predicate: link.predicate,
                    target: 'test://new_target'
                })
                await proxy.update(links[0], updatedLink)

                links = await proxy.get(all)
                expect(links.length).to.equal(1)
                link = new Link(links[0].data)
                expect(link).to.eql(updatedLink)

                await proxy.remove(links[0])
                expect(await proxy.get(all)).to.eql([])
            })

            it('can do singleTarget operations', async () => {
                const all = new LinkQuery({})

                expect(await proxy.get(all)).to.eql([])
                const link1 = new Link({
                    source: 'test://source',
                    predicate: 'test://predicate',
                    target: 'target1'
                })

                await proxy.setSingleTarget(link1)
                const result1 = (await proxy.get(all))[0].data
                expect(result1.source).to.equal(link1.source)
                expect(result1.predicate).to.equal(link1.predicate)
                expect(result1.target).to.equal(link1.target)
                expect(await proxy.getSingleTarget(new LinkQuery(link1))).to.equal('target1')

                const link2 = new Link({
                    source: 'test://source',
                    predicate: 'test://predicate',
                    target: 'target2'
                })

                await proxy.setSingleTarget(link2)

                const result2 = (await proxy.get(all))[0].data
                expect(result2.source).to.equal(link2.source)
                expect(result2.predicate).to.equal(link2.predicate)
                expect(result2.target).to.equal(link2.target)
                expect(await proxy.getSingleTarget(new LinkQuery(link1))).to.equal('target2')
            })

            it('can subscribe to Prolog query results', async () => {
                // Add some test data
                await proxy.add(new Link({
                    source: "ad4m://root",
                    target: "note-ipfs://Qm123"
                }))
                await proxy.add(new Link({
                    source: "note-ipfs://Qm123",
                    target: "todo-ontology://is-todo"
                }))

                // Create subscription
                const subscription = await (proxy as any).subscribeInfer('triple(X, _, "todo-ontology://is-todo").')
                
                // Check initial result
                const initialResult = subscription.result
                expect(initialResult).to.be.an('array')
                expect(initialResult.length).to.equal(1)
                expect(initialResult[0].X).to.equal('note-ipfs://Qm123')

                // Set up callback for updates
                const updates: any[] = []
                const unsubscribe = subscription.onResult((result: any) => {
                    updates.push(result)
                })

                // Add another link that should trigger an update
                await proxy.add(new Link({
                    source: "note-ipfs://Qm456",
                    target: "todo-ontology://is-todo"
                }))

                // Wait for subscription update
                await sleep(1000)

                // Verify we got an update
                expect(updates.length).to.be.greaterThan(0)
                const latestResult = updates[updates.length - 1]
                expect(latestResult).to.be.an('array')
                expect(latestResult.length).to.equal(2)
                expect(latestResult.map((r: any) => r.X)).to.include('note-ipfs://Qm123')
                expect(latestResult.map((r: any) => r.X)).to.include('note-ipfs://Qm456')

                // Clean up subscription
                unsubscribe()
                subscription.dispose()
            })

        })

        describe('SurrealDB Queries', () => {
            it('should execute basic SurrealQL SELECT query', async () => {
                const ad4mClient = testContext.ad4mClient!
                const perspective = await ad4mClient.perspective.add("test-surrealdb-basic")
                
                // Add sample links
                const link1 = new Link({
                    source: "test://source1",
                    predicate: "test://follows",
                    target: "test://target1"
                })
                const link2 = new Link({
                    source: "test://source2",
                    predicate: "test://likes",
                    target: "test://target2"
                })
                
                await perspective.addLinks([link1, link2])
                
                // Wait a bit for SurrealDB to index
                await sleep(500)
                
                // Execute SurrealQL query
                const result = await perspective.querySurrealDB('SELECT * FROM link')
                
                // Verify results
                expect(result).to.be.an('array')
                expect(result.length).to.be.greaterThanOrEqual(2)
                
                // Check that our links are present
                const sources = result.map((r: any) => r.source)
                expect(sources).to.include('test://source1')
                expect(sources).to.include('test://source2')
                
                // Clean up
                await ad4mClient.perspective.remove(perspective.uuid)
            })

            it('should handle SurrealQL query with WHERE clause', async () => {
                const ad4mClient = testContext.ad4mClient!
                const perspective = await ad4mClient.perspective.add("test-surrealdb-where")
                
                // Add links with different predicates
                const followsLink = new Link({
                    source: "test://alice",
                    predicate: "test://follows",
                    target: "test://bob"
                })
                const likesLink = new Link({
                    source: "test://alice",
                    predicate: "test://likes",
                    target: "test://post123"
                })
                
                await perspective.addLinks([followsLink, likesLink])
                
                // Wait for indexing
                await sleep(500)
                
                // Query with WHERE clause
                const result = await perspective.querySurrealDB(
                    "SELECT * FROM link WHERE predicate = 'test://follows'"
                )
                
                // Verify filtered results
                expect(result).to.be.an('array')
                expect(result.length).to.be.greaterThanOrEqual(1)
                
                // All results should have the 'follows' predicate
                result.forEach((link: any) => {
                    if (link.source === 'test://alice') {
                        expect(link.predicate).to.equal('test://follows')
                    }
                })
                
                // Clean up
                await ad4mClient.perspective.remove(perspective.uuid)
            })

            it('should return empty array for query with no matches', async () => {
                const ad4mClient = testContext.ad4mClient!
                const perspective = await ad4mClient.perspective.add("test-surrealdb-empty")
                
                // Add a link
                await perspective.add(new Link({
                    source: "test://source",
                    predicate: "test://predicate",
                    target: "test://target"
                }))
                
                // Wait for indexing
                await sleep(500)
                
                // Query for something that doesn't exist
                const result = await perspective.querySurrealDB(
                    "SELECT * FROM link WHERE predicate = 'test://nonexistent'"
                )
                
                // Should return empty array
                expect(result).to.be.an('array')
                expect(result.length).to.equal(0)
                
                // Clean up
                await ad4mClient.perspective.remove(perspective.uuid)
            })

            it('should integrate with link mutations', async () => {
                const ad4mClient = testContext.ad4mClient!
                const perspective = await ad4mClient.perspective.add("test-surrealdb-mutations")
                
                // Add a link
                const link = new Link({
                    source: "test://mutation-source",
                    predicate: "test://mutation-predicate",
                    target: "test://mutation-target"
                })
                
                const addedLink = await perspective.add(link)
                
                // Wait for indexing
                await sleep(500)
                
                // Query to verify it's there
                let result = await perspective.querySurrealDB(
                    "SELECT * FROM link WHERE source = 'test://mutation-source'"
                )
                
                expect(result).to.be.an('array')
                expect(result.length).to.be.greaterThanOrEqual(1)
                
                // Remove the link
                await perspective.removeLinks([addedLink])
                
                // Wait for indexing
                await sleep(500)
                
                // Query again and verify it's gone
                result = await perspective.querySurrealDB(
                    "SELECT * FROM link WHERE source = 'test://mutation-source'"
                )
                
                expect(result).to.be.an('array')
                expect(result.length).to.equal(0)
                
                // Clean up
                await ad4mClient.perspective.remove(perspective.uuid)
            })
        })
    }
}