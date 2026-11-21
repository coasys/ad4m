import { LinkQuery }  from '../perspectives/LinkQuery'
import { LinkExpression } from './Links'

const query = new LinkQuery({
    source: 'a'
})

describe('LinkQuery', () => {
    it('can match positively', () => {
        expect(query.isMatch({
            source: 'a',
            predicate: 'b',
            target: 'c'
        })).toBe(true)

        expect(query.isMatch({
            source: 'a',
            target: 'c'
        })).toBe(true)

        expect(query.isMatch({
            source: 'a',
            predicate: 'b',
            target: 'c'
        })).toBe(true)
    })

    it('can match negatively', () => {
        expect(query.isMatch({
            source: 'c',
            predicate: 'b',
            target: 'c'
        })).toBe(false)

        expect(query.isMatch({
            source: 'c',
            target: 'b'
        })).toBe(false)

    })
})

describe('LinkExpression.fromWire', () => {
    const wire = {
        author: 'did:key:z6Mkbot',
        timestamp: '2026-11-08T00:00:00.000Z',
        data: { source: 'a', predicate: 'b', target: 'c' },
        proof: { key: 'did:key:z6Mkbot#z6Mkbot', signature: 'ab', valid: true, invalid: false },
        status: 'SHARED' as const,
        verdict: 'valid' as const,
        person: 'did:scid:coasys:1:owner',
    }

    it("keeps the bot mark of a bot's link with its owner as the person", () => {
        const link = LinkExpression.fromWire({ ...wire, byBot: true })
        expect(link.person).toBe('did:scid:coasys:1:owner')
        expect(link.byBot).toBe(true)
    })

    it("leaves the bot mark out of a device's link", () => {
        const link = LinkExpression.fromWire(wire)
        expect(link.person).toBe('did:scid:coasys:1:owner')
        expect(link.byBot).toBeUndefined()
    })
})