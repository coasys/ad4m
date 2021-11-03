//@ts-ignore
import swipl from 'swipl-stdio'

describe('swipl', () => {
    it('can call Prolog predicate', async () => {
        const engine = new swipl.Engine();
        const query = await engine.createQuery('member(X, [1,2,3,4])');
        let allMatches = []
        try {
            let ret = null;
            while (ret = await query.next()) {
                allMatches.push(ret.X)
            }
        } finally {
            await query.close()
        }
        
        expect(allMatches).toEqual([1,2,3,4])
    })
})
