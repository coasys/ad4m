import { Perspective } from "./Perspective"
import { Link, LinkExpression } from "../links/Links"
import { LinkQuery } from "./LinkQuery"

const link = new LinkExpression()
link.author = 'did:method:12345'
link.timestamp = new Date().toString()
link.data = new Link({source: 'root', target: 'perspective://Qm34589a3ccc0'})
link.proof = { signature: 'asdfasdf', key: 'asdfasdf' }

describe('Perspective', () => {
    it('implements get()', () => {
        let p = new Perspective([link])
        let result = p.get(new LinkQuery({source: 'root'}))
        expect(result).toEqual([link])
    })
    
    it('implements getSingleTarget()', () => {
        let p = new Perspective([link])
        let result = p.getSingleTarget(new LinkQuery({source: 'root'}))
        expect(result).toEqual(link.data.target)
    })
})