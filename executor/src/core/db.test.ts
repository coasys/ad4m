import { Ad4mDb } from './db'
import { v4 as uuidv4 } from 'uuid';
import { expect } from "chai";
import { LinkExpressionInput, PerspectiveDiff } from '@coasys/ad4m';
import faker from 'faker'

function constructDummyLinkExpression(): LinkExpressionInput {
    // @ts-ignore
    return {
        data: {
            source: faker.internet.url(),
            target: faker.internet.url(),
            predicate: faker.internet.url(),
        },
        proof: {
            signature: 'signature',
            key: 'key',
            date: 'date',
        },
        author: "did:test:key",
        timestamp: new Date().toISOString(),
    } as LinkExpressionInput
}

describe('Ad4mDb', () => {
    let db: Ad4mDb | undefined
    let pUUID: string | undefined

    beforeEach(() => {
        db = new Ad4mDb();
        pUUID = uuidv4()
    })

    it('can store and retrieve links', async () => {
        const link = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link);
        link.status = 'shared'

        const result = await db!.getLink(pUUID!, link);
        expect(result).to.be.deep.equal(link);
    })

    it('can store and get link with missing predicate', async () => {
        const link = constructDummyLinkExpression();
        delete link.data.predicate
        await db!.addLink(pUUID!, link);

        const result = await db!.getLink(pUUID!, link);
        expect(result).to.be.deep.equal(link);
    })

    it('can call getLink() multiple times', async () => {
        const link = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link);

        for(let i=0; i<3; i++) {
            expect(await db!.getLink(pUUID!, link)).to.be.deep.equal(link);
        }
    })

    it('can getAllLinks', async () => {
        const link1 = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link1);

        link1.status = 'shared'

        const link2 = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link2);

        link2.status = 'shared'

        const allLinks = await db!.getAllLinks(pUUID!)

        expect(allLinks).to.be.deep.equal([
            link1,
            link2
        ])
    })

    it('can call getAllLinks() multiple times', async () => {
        const link1 = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link1);

        link1.status = 'shared'

        for(let i=0; i<3; i++) {
            expect(await db!.getAllLinks(pUUID!)).to.be.deep.equal([
                link1
            ])
        }
    })

    it('can getLinksBySource', async () => {
        const link1 = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link1);

        link1.status = 'shared';

        const link2 = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link2);

        const result = await db!.getLinksBySource(pUUID!, link1.data.source);

        expect(result).to.be.deep.equal([link1]);
    })

    it('can getLinksByTarget', async () => {
        const link1 = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link1);

        link1.status = 'shared';

        const link2 = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link2);

        const result = await db!.getLinksByTarget(pUUID!, link1.data.target);

        expect(result).to.be.deep.equal([link1]);
    })

    it('can updateLink', async () => {
        const link1 = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link1);

        expect(await db!.getLink(pUUID!, link1)).to.be.deep.equal(link1);

        const link2 = constructDummyLinkExpression();
        await db!.updateLink(pUUID!, link1, link2);
        expect(await db!.getLink(pUUID!, link1)).to.be.undefined;
        expect(await db!.getLink(pUUID!, link2)).to.be.deep.equal(link2);
    })

    it('can remove()', async () => {
        const link1 = constructDummyLinkExpression();
        await db!.addLink(pUUID!, link1);

        expect(await db!.getLink(pUUID!, link1)).to.be.deep.equal(link1)
        await db!.removeLink(pUUID!, link1);
        expect(await db!.getLink(pUUID!, link1)).to.be.undefined;
    })

    it('can get & remove pendingDiffs()', async () => {
        const addition = constructDummyLinkExpression();
        const removal = constructDummyLinkExpression();
        await db!.addPendingDiff(pUUID!, {additions: [
            addition,
        ], removals: [
            removal,
        ]} as PerspectiveDiff);

        const get = await db!.getPendingDiffs(pUUID!);

        expect(get?.additions.length).to.be.equal(1);
        expect(get?.removals.length).to.be.equal(1);

        expect(get).to.be.deep.equal({additions: [
            addition,
        ], removals: [
            removal,
        ]} as PerspectiveDiff);

        await db!.clearPendingDiffs(pUUID!)
        const get2 = await db?.getPendingDiffs(pUUID!);

        expect(get2?.additions.length).to.be.equal(0);
    })
})