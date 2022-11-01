import { Ad4mClient, Link, LinkQuery, PerspectiveProxy } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import { expect } from "chai";
import * as sinon from "sinon";

export default function perspectiveTests(testContext: TestContext) {
    return  () => {
        describe('Perspectives', () => {
            it('can create, get & delete perspective', async () => {
                const ad4mClient = testContext.ad4mClient!

                const create = await ad4mClient.perspective.add("test");
                expect(create.name).to.equal("test");

                const get = await ad4mClient!.perspective.byUUID(create.uuid);
                expect(get!.name).to.equal("test");

                const update = await ad4mClient!.perspective.update(create.uuid, "updated-test");
                expect(update.name).to.equal("updated-test");

                const getUpdated = await ad4mClient!.perspective.byUUID(update.uuid );
                expect(getUpdated!.name).to.equal("updated-test");

                const perspectives = await ad4mClient.perspective.all();
                expect(perspectives.length).to.equal(1);

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

            it('test local perspective links - time query', async () => {
                const ad4mClient = testContext.ad4mClient!

                const create = await ad4mClient!.perspective.add("test-links-time");
                expect(create.name).to.equal("test-links-time");

                let addLink = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target", predicate: "lang://predicate"}));
                let addLink2 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target2", predicate: "lang://predicate"}));
                let addLink3 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target3", predicate: "lang://predicate"}));
                let addLink4 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target4", predicate: "lang://predicate"}));
                let addLink5 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target5", predicate: "lang://predicate"}));

                // Get all the links
                let queryLinksAll = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(new Date(addLink.timestamp).getTime()), untilDate: new Date()}));
                expect(queryLinksAll.length).to.equal(5);


                // Get all the links in ascending order
                let queryLinksAsc = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(), untilDate: new Date("August 19, 1975 23:15:30"), limit: 3}));
                expect(queryLinksAsc.length).to.equal(3);
                expect(queryLinksAsc[0].data.target).to.equal(addLink3.data.target)
                expect(queryLinksAsc[1].data.target).to.equal(addLink4.data.target)
                expect(queryLinksAsc[2].data.target).to.equal(addLink5.data.target)

                // Get all the links in ascending order
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
                expect(deleteLink.perspectiveRemoveLink).to.equal(true);

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
                expect(perspectiveAdded.calledOnce).to.be.true;
                const pSeenInAddCB = perspectiveAdded.getCall(0).args[0];
                expect(pSeenInAddCB.uuid).to.equal(p.uuid)
                expect(pSeenInAddCB.name).to.equal(p.name)

                const p1 = await ad4mClient.perspective.update(p.uuid , "New Name")
                expect(perspectiveUpdated.calledOnce).to.be.true;
                const pSeenInUpdateCB = perspectiveUpdated.getCall(0).args[0];
                expect(pSeenInUpdateCB.uuid).to.equal(p1.uuid)
                expect(pSeenInUpdateCB.name).to.equal(p1.name)

                const linkAdded = sinon.fake()
                await ad4mClient.perspective.addPerspectiveLinkAddedListener(p1.uuid, [linkAdded])
                const linkRemoved = sinon.fake()
                await ad4mClient.perspective.addPerspectiveLinkRemovedListener(p1.uuid, [linkRemoved])

                const linkExpression = await ad4mClient.perspective.addLink(p1.uuid , {source: 'root', target: 'lang://123'})
                expect(linkAdded.calledOnce).to.be.true;
                expect(linkAdded.getCall(0).args[0]).to.eql(linkExpression)

                const updatedLinkExpression = await ad4mClient.perspective.updateLink(p1.uuid , linkExpression, {source: 'root', target: 'lang://456'})
                expect(linkAdded.calledTwice).to.be.true;
                expect(linkAdded.getCall(1).args[0]).to.eql(updatedLinkExpression)

                expect(linkRemoved.calledOnce).to.be.true;
                expect(linkRemoved.getCall(0).args[0]).to.eql(linkExpression)

                await ad4mClient.perspective.removeLink(p1.uuid , updatedLinkExpression)
                expect(linkRemoved.calledTwice).to.be.true;
                expect(linkRemoved.getCall(1).args[0]).to.eql(updatedLinkExpression)
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
                console.warn("got prolog link result", linkResult);
                expect(linkResult).not.to.be.false;
                expect(linkResult.length).to.equal(1)
                expect(linkResult[0].X).to.equal('note-ipfs://Qm123');
                expect(linkResult[0].Timestamp).not.to.be.null;
                expect(linkResult[0].Author).not.to.be.null;
            })
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
            
        })
    }
}