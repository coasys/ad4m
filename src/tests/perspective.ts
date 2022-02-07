import { isReference } from "@apollo/client";
import { Ad4mClient, Link, LinkQuery, PerspectiveProxy, LinkExpression, ExpressionProof } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'
import sleep from "./sleep";

export default function perspectiveTests(testContext: TestContext) {
    return  () => {
        describe('Perspectives', () => {
            it('can create, get & delete perspective', async () => {
                const ad4mClient = testContext.ad4mClient!

                const create = await ad4mClient.perspective.add("test");
                expect(create.name).toEqual("test");

                const get = await ad4mClient!.perspective.byUUID(create.uuid);
                expect(get!.name).toEqual("test");

                const update = await ad4mClient!.perspective.update(create.uuid, "updated-test");
                expect(update.name).toEqual("updated-test");

                const getUpdated = await ad4mClient!.perspective.byUUID(update.uuid );
                expect(getUpdated!.name).toEqual("updated-test");

                const perspectives = await ad4mClient.perspective.all();
                expect(perspectives.length).toBe(1);

                const perspectiveSnaphot = await ad4mClient.perspective.snapshotByUUID(update.uuid );
                expect(perspectiveSnaphot!.links.length).toBe(0);

                const deletePerspective = await ad4mClient!.perspective.remove(update.uuid );
                expect(deletePerspective.perspectiveRemove).toBe(true);

                const getDeleted = await ad4mClient!.perspective.byUUID(update.uuid );
                expect(getDeleted).toEqual(null);
            })

            it('test local perspective links - time query', async () => {
                const ad4mClient = testContext.ad4mClient!

                const create = await ad4mClient!.perspective.add("test-links-time");
                expect(create.name).toEqual("test-links-time");

                let addLink = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target", predicate: "lang://predicate"}));
                let addLink2 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target2", predicate: "lang://predicate"}));
                let addLink3 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target3", predicate: "lang://predicate"}));
                let addLink4 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target4", predicate: "lang://predicate"}));
                let addLink5 = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target5", predicate: "lang://predicate"}));

                // Get all the links
                let queryLinksAll = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(new Date(addLink.timestamp).getTime()), untilDate: new Date()}));
                expect(queryLinksAll.length).toEqual(5);


                // Get all the links in ascending order
                let queryLinksAsc = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(), untilDate: new Date("August 19, 1975 23:15:30"), limit: 3}));
                expect(queryLinksAsc.length).toEqual(3);
                expect(queryLinksAsc[0].data.target).toBe(addLink3.data.target)
                expect(queryLinksAsc[1].data.target).toBe(addLink4.data.target)
                expect(queryLinksAsc[2].data.target).toBe(addLink5.data.target)

                // Get all the links in ascending order
                let queryLinksDesc = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date("August 19, 1975 23:15:30"), untilDate: new Date(), limit: 3}));
                expect(queryLinksDesc.length).toEqual(3);
                expect(queryLinksDesc[0].data.target).toBe(addLink.data.target)
                expect(queryLinksDesc[1].data.target).toBe(addLink2.data.target)
                expect(queryLinksDesc[2].data.target).toBe(addLink3.data.target)


                //Test can get all links but first by querying from second timestamp
                let queryLinks = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(new Date(addLink2.timestamp).getTime() - 1), untilDate: new Date()}));
                expect(queryLinks.length).toEqual(4);

                console.warn("QUERYING FOR LINKS\n\n\n\n");
                //Test can get links limited
                let queryLinksLimited = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test", fromDate: new Date(new Date(addLink2.timestamp).getTime() - 1), untilDate: new Date(), limit: 3}));
                expect(queryLinksLimited.length).toEqual(3);

                //Test can get only the first link
                let queryLinksFirst = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({
                    source: "lang://test", fromDate: new Date(addLink.timestamp), 
                    untilDate: new Date(new Date(addLink2.timestamp).getTime() - 1)
                }));
                expect(queryLinksFirst.length).toEqual(1);
                expect(queryLinksFirst[0].data.target).toEqual("lang://test-target");
            })

            it('test local perspective links', async () => {
                const ad4mClient = testContext.ad4mClient!

                const create = await ad4mClient!.perspective.add("test-links");
                expect(create.name).toEqual("test-links");

                let addLink = await ad4mClient!.perspective.addLink(create.uuid, new Link({source: "lang://test", target: "lang://test-target", predicate: "lang://predicate"}));
                expect(addLink.data.target).toEqual("lang://test-target");
                expect(addLink.data.source).toEqual("lang://test");

                //Test can get by source, target, predicate
                let queryLinks = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test"}));
                expect(queryLinks.length).toEqual(1);
                expect(queryLinks[0].data.target).toEqual("lang://test-target");
                expect(queryLinks[0].data.source).toEqual("lang://test");

                let queryLinksTarget = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({target: "lang://test-target"}));
                expect(queryLinksTarget.length).toEqual(1);
                expect(queryLinksTarget[0].data.target).toEqual("lang://test-target");
                expect(queryLinksTarget[0].data.source).toEqual("lang://test");

                let queryLinksPredicate = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({predicate: "lang://predicate"}));
                expect(queryLinksPredicate.length).toEqual(1);
                expect(queryLinksPredicate[0].data.target).toEqual("lang://test-target");
                expect(queryLinksPredicate[0].data.source).toEqual("lang://test");

                const perspectiveSnaphot = await ad4mClient.perspective.snapshotByUUID(create.uuid );
                expect(perspectiveSnaphot!.links.length).toBe(1);

                //Update the link to new link
                const updateLink = await ad4mClient.perspective.updateLink(create.uuid, addLink, 
                    new Link({source: "lang://test2", target: "lang://test-target2", predicate: "lang://predicate2"}));
                expect(updateLink.data.target).toEqual("lang://test-target2");
                expect(updateLink.data.source).toEqual("lang://test2");

                const perspectiveSnaphotLinkUpdate = await ad4mClient.perspective.snapshotByUUID(create.uuid );
                expect(perspectiveSnaphotLinkUpdate!.links.length).toBe(1);

                //Test cannot get old link
                let queryLinksOld = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test"}));
                expect(queryLinksOld.length).toEqual(0);

                //Test can get new link
                let queryLinksUpdated = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test2"}));
                expect(queryLinksUpdated.length).toEqual(1);

                const deleteLink = await ad4mClient!.perspective.removeLink(create.uuid, updateLink);
                expect(deleteLink.perspectiveRemoveLink).toEqual(true);

                let queryLinksDeleted = await ad4mClient!.perspective.queryLinks(create.uuid, new LinkQuery({source: "lang://test2"}));
                expect(queryLinksDeleted.length).toEqual(0);
            })

            it('subscriptions', async () => {
                const ad4mClient: Ad4mClient = testContext.ad4mClient!

                const perspectiveAdded = jest.fn()
                ad4mClient.perspective.addPerspectiveAddedListener(perspectiveAdded)
                const perspectiveUpdated = jest.fn()
                ad4mClient.perspective.addPerspectiveUpdatedListener(perspectiveUpdated)
                const perspectiveRemoved = jest.fn()
                ad4mClient.perspective.addPerspectiveRemovedListener(perspectiveRemoved)

                const name = "Subscription Test Perspective"
                const p = await ad4mClient.perspective.add(name)
                expect(perspectiveAdded.mock.calls.length).toBe(1)
                const pSeenInAddCB = perspectiveAdded.mock.calls[0][0]
                expect(pSeenInAddCB.uuid).toStrictEqual(p.uuid)
                expect(pSeenInAddCB.name).toStrictEqual(p.name)

                const p1 = await ad4mClient.perspective.update(p.uuid , "New Name")
                expect(perspectiveUpdated.mock.calls.length).toBe(1)
                const pSeenInUpdateCB = perspectiveUpdated.mock.calls[0][0]
                expect(pSeenInUpdateCB.uuid).toStrictEqual(p1.uuid)
                expect(pSeenInUpdateCB.name).toStrictEqual(p1.name)

                // const linkAdded = jest.fn()
                // // TODO: @fayeed update this
                // await ad4mClient.perspective.addPerspectiveLinkListener(p1.uuid, 'link-added', linkAdded)
                // const linkRemoved = jest.fn()
                // await ad4mClient.perspective.addPerspectiveLinkListener(p1.uuid, 'link-removed', linkRemoved)

                // const linkExpression = await ad4mClient.perspective.addLink(p1.uuid , {source: 'root', target: 'lang://123'})
                // expect(linkAdded.mock.calls.length).toBe(1)
                // expect(linkAdded.mock.calls[0][0]).toEqual(linkExpression)

                // const updatedLinkExpression = await ad4mClient.perspective.updateLink(p1.uuid , linkExpression, {source: 'root', target: 'lang://456'})
                // expect(linkAdded.mock.calls.length).toBe(2)
                // expect(linkAdded.mock.calls[1][0]).toEqual(updatedLinkExpression)
                // expect(linkRemoved.mock.calls.length).toBe(1)
                // expect(linkRemoved.mock.calls[0][0]).toEqual(linkExpression)

                // await ad4mClient.perspective.removeLink(p1.uuid , updatedLinkExpression)
                // expect(linkRemoved.mock.calls.length).toBe(2)
                // expect(linkRemoved.mock.calls[1][0]).toEqual(updatedLinkExpression)
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
                expect(result).toBeTruthy()
                expect(result.length).toBe(1)
                expect(result[0].X).toBe('note-ipfs://Qm123')

                expect(await p.infer('reachable("ad4m://root", "todo-ontology://is-todo")')).toBeTruthy()
            })
        })

        describe('PerspectiveProxy', () => {
            let proxy: PerspectiveProxy
            let ad4mClient: Ad4mClient
            beforeAll(async () => {
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

                expect(await proxy.get(all)).toStrictEqual([])

                await proxy.add(testLink)
                let links = await proxy.get(all)
                expect(links.length).toBe(1)

                let link = new Link(links[0].data)
                expect(link).toStrictEqual(testLink)

                const updatedLink = new Link({
                    source: link.source,
                    predicate: link.predicate,
                    target: 'test://new_target'
                })
                await proxy.update(links[0], updatedLink)

                links = await proxy.get(all)
                expect(links.length).toBe(1)
                link = new Link(links[0].data)
                expect(link).toStrictEqual(updatedLink)

                await proxy.remove(links[0])
                expect(await proxy.get(all)).toStrictEqual([])
            })

            it('can do singleTarget operations', async () => {
                const all = new LinkQuery({})

                expect(await proxy.get(all)).toStrictEqual([])
                const link1 = new Link({
                    source: 'test://source',
                    predicate: 'test://predicate',
                    target: 'target1'
                })

                await proxy.setSingleTarget(link1)
                const result1 = (await proxy.get(all))[0].data
                expect(result1.source).toStrictEqual(link1.source)
                expect(result1.predicate).toStrictEqual(link1.predicate)
                expect(result1.target).toStrictEqual(link1.target)
                expect(await proxy.getSingleTarget(new LinkQuery(link1))).toBe('target1')

                const link2 = new Link({
                    source: 'test://source',
                    predicate: 'test://predicate',
                    target: 'target2'
                })

                await proxy.setSingleTarget(link2)

                const result2 = (await proxy.get(all))[0].data
                expect(result2.source).toStrictEqual(link2.source)
                expect(result2.predicate).toStrictEqual(link2.predicate)
                expect(result2.target).toStrictEqual(link2.target)
                expect(await proxy.getSingleTarget(new LinkQuery(link1))).toBe('target2')
            })
            
        })
    }
}