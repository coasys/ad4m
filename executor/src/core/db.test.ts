import { PerspectivismDb } from './db'
import Memory from 'lowdb/adapters/Memory'
import { v4 as uuidv4 } from 'uuid';

describe('PerspectivismDb', () => {
    let db: PerspectivismDb | undefined
    let pUUID: string | undefined

    beforeEach(() => {
        db = new PerspectivismDb(new Memory(""))
        pUUID = uuidv4()
    })

    it('can store and retrieve objects by name', () => {
        const obj = { test: 'object' }
        const name = 'linkName'

        db!.storeLink(pUUID!, obj, name)
        const result = db!.getLink(pUUID!, name)

        expect(result).toEqual(obj)
    })

    it('can call getLink() multiple times', () => {
        const obj = { test: 'object' }
        const name = 'linkName'
        db!.storeLink(pUUID!, obj, name)

        for(let i=0; i<3; i++) {
            expect(db!.getLink(pUUID!, name)).toEqual(obj)
        }

    })

    it('can getAllLinks', () => {
        const obj1 = { test: 'object1' }
        const name1 = 'linkName1'
        db!.storeLink(pUUID!, obj1, name1)

        const obj2 = { test: 'object2' }
        const name2 = 'linkName2'
        db!.storeLink(pUUID!, obj2, name2)

        const allLinks = db!.getAllLinks(pUUID!)

        expect(allLinks).toEqual([
            {
                link: obj1,
                name: name1,
            },
            {
                link: obj2,
                name: name2,
            }
        ])
    })

    it('can getAllLinks with only one link (attached)', () => {
        const obj1 = { test: 'object1' }
        const name1 = 'linkName1'
        db!.storeLink(pUUID!, obj1, name1)
        db!.attachSource(pUUID!, 'root', name1)
        db!.attachTarget(pUUID!, 'link-url', name1)

        const allLinks = db!.getAllLinks(pUUID!)

        expect(allLinks).toEqual([
            {
                link: obj1,
                name: name1,
            }
        ])
    })

    it('can call getAllLinks() multiple times', () => {
        const obj1 = { test: 'object1' }
        const name1 = 'linkName1'
        db!.storeLink(pUUID!, obj1, name1)

        for(let i=0; i<3; i++) {
            expect(db!.getAllLinks(pUUID!)).toEqual([
                {
                    link: obj1,
                    name: name1,
                }
            ])
        }
    })

    it('can getLinksBySource', () => {
        const obj1 = { test: 'object1' }
        const name1 = 'linkName1'
        db!.storeLink(pUUID!, obj1, name1)

        const obj2 = { test: 'object2' }
        const name2 = 'linkName2'
        db!.storeLink(pUUID!, obj2, name2)

        db!.attachSource(pUUID!, name1, name2)

        const result = db!.getLinksBySource(pUUID!, name1)

        expect(result).toEqual([{
            link: obj2,
            name: name2
        }])
    })

    it('can getLinksByTarget', () => {
        const obj1 = { test: 'object1' }
        const name1 = 'linkName1'
        db!.storeLink(pUUID!, obj1, name1)

        const obj2 = { test: 'object2' }
        const name2 = 'linkName2'
        db!.storeLink(pUUID!, obj2, name2)

        db!.attachTarget(pUUID!, name1, name2)

        const result = db!.getLinksByTarget(pUUID!, name1)

        expect(result).toEqual([{
            link: obj2,
            name: name2
        }])
    })

    it('can updateLink', () => {
        const obj1 = { test: 'object1' }
        const name1 = 'linkName1'
        db!.storeLink(pUUID!, obj1, name1)

        expect(db!.getLink(pUUID!, name1)).toEqual(obj1)

        const obj2 = { test: 'object2' }
        db!.updateLink(pUUID!, obj2, name1)
        expect(db!.getLink(pUUID!, name1)).toEqual(obj2)
    })

    it('can remove()', () => {
        db!.storeLink(pUUID!, { test: 'object1' }, '1')
        expect(db!.getLink(pUUID!, '1')).toEqual({ test: 'object1' })
        db!.remove
    })

    it('can removeTarget()', () => {
        db!.storeLink(pUUID!, { test: 'object1' }, '1')
        db!.storeLink(pUUID!, { test: 'object2' }, '2')

        db!.attachTarget(pUUID!, '1', '2')

        const result1 = db!.getLinksByTarget(pUUID!, '1')

        expect(result1).toEqual([{
            link: { test: 'object2' },
            name: '2'
        }])

        db!.removeTarget(pUUID!, '1', '2')

        const result2 = db!.getLinksByTarget(pUUID!, '1')

        expect(result2).toEqual([])
    })
})