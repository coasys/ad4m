import PrologInstance from "./PrologInstance"

const linksProgram = `
linkFact(1,2).
linkFact(2,3).
link(A,B):-linkFact(A,B).
link(A,B):-
    linkFact(A,X),
    link(X,B).`

describe('PrologInstance', () => {
    it('smoke test', async () => {
        const instance = new PrologInstance()
    })

    it('runs Prolog', async () => {
        const instance = new PrologInstance()

        await instance.consult(linksProgram)

        expect(await instance.query('link(1,2).')).toEqual(true)
        expect(await instance.query('link(1,3).')).toEqual(true)
        expect(await instance.query('link(1,4).')).toEqual(false)
    })

    it('can destructure query results', async () => {
        const instance = new PrologInstance()
        await instance.consult(linksProgram)
        
        expect(await instance.query('link(1,X).')).toEqual([
            {'X': 2}, 
            {'X': 3}
        ])

        expect(await instance.query('link(Y,3).')).toEqual([
            {'Y': 2}, 
            {'Y': 1}
        ])
        expect(await instance.query('link(Y,4).')).toEqual(false)
    })

})