import { Ad4mClient, Link, LinkQuery } from "@perspect3vism/ad4m";
import { TestContext } from './integration.test'

export default function tests(testContext: TestContext) {
    return  () => {
        describe('create, update, get, delete perspective', () => {
            it('can create perspective', async () => {
                const ad4mClient = testContext.ad4mClient

                const create = await ad4mClient.perspective.add("test");
                expect(create.name).toEqual("test");

                const get = await ad4mClient!.perspective.byUUID(create.uuid);
                expect(get.name).toEqual("test");

                const update = await ad4mClient!.perspective.update(create.uuid, "updated-test");
                expect(update.name).toEqual("updated-test");

                const getUpdated = await ad4mClient!.perspective.byUUID(update.uuid);
                expect(getUpdated.name).toEqual("updated-test");

                const deletePerspective = await ad4mClient!.perspective.remove(update.uuid);
                console.log(deletePerspective);

                const getDeleted = await ad4mClient!.perspective.byUUID(update.uuid);
                expect(getDeleted).toEqual(null);
            })

            it('test local perspective links', async () => {
                const ad4mClient = testContext.ad4mClient

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

                //Update the link to new link
                const updateLink = await ad4mClient.perspective.updateLink(create.uuid, addLink, 
                    new Link({source: "lang://test2", target: "lang://test-target2", predicate: "lang://predicate2"}));
                expect(updateLink.data.target).toEqual("lang://test-target2");
                expect(updateLink.data.source).toEqual("lang://test2");

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
        })
    }
}