//@ts-ignore
import swipl from 'swipl-stdio'
import { expect } from "chai";

describe('swipl', () => {
    it('can call Prolog predicate', async () => {
        const engine = new swipl.Engine();
        const query = await engine.createQuery('member(X, [1,2,3,4])');
        let allMatches = []
        try {
            let ret = null;
            while (ret = await query.next()) {
                //@ts-ignore
                allMatches.push(ret.X)
            }
        } finally {
            await query.close()
        }
        
        expect(allMatches).to.be.eql([1,2,3,4])
    })
})
