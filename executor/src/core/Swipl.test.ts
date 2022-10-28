//@ts-ignore
import swipl from 'swipl-stdio'
import { expect } from "chai";
import path from 'path';
import { fileURLToPath } from 'url';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

describe('swipl', () => {
    it('can call Prolog predicate', async () => {
        const engine = new swipl.Engine(path.join(__dirname, '../test-temp', 'swipl'));
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
        
        expect(allMatches).to.be.deep.equal([1,2,3,4])
    })
})
