import { expect } from "chai";

import PrologInstance from "./PrologInstance"

const linksProgram = `
linkFact(1,2).
linkFact(2,3).
link(A,B):-linkFact(A,B).
link(A,B):-
    linkFact(A,X),
    link(X,B).`

const config = {
    resourcePath: ''
}

describe('PrologInstance', () => {
    it('smoke test', async () => {
        // @ts-ignore
        const instance = new PrologInstance(config)
    })

    it('runs Prolog', async () => {
        // @ts-ignore
        const instance = new PrologInstance(config)

        await instance.consult(linksProgram)

        expect(await instance.query('link(1,2).')).to.be.true;
        expect(await instance.query('link(1,3).')).to.be.true;
        expect(await instance.query('link(1,4).')).to.be.false;
    })

    it('can destructure query results', async () => {
        // @ts-ignore
        const instance = new PrologInstance(config)
        await instance.consult(linksProgram)

        expect(await instance.query('link(1,X).')).to.be.eql([
            {'X': 2},
            {'X': 3}
        ])

        expect(await instance.query('link(Y,3).')).to.be.eql([
            {'Y': 2},
            {'Y': 1}
        ])
        expect(await instance.query('link(Y,4).')).to.be.false;
    })

})