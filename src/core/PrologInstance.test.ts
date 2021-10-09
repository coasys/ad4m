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
        await instance.initialized()
    })

    it('has working virtual filesystem', async () => {
        const instance = new PrologInstance()
        await instance.initialized()
        const content = "some test string"
        instance.writeFile('wurst', content)
        const read = instance.readFile('wurst')
        expect(read).toEqual(content)
    })

    it('runs Prolog', async () => {
        const instance = new PrologInstance()
        await instance.initialized()

        expect(instance.consult(linksProgram)).toEqual('true.')

        expect(instance.query('link(1,2).')).toEqual('true')
        expect(instance.query('link(1,3).')).toEqual('true')
        expect(instance.query('link(1,4).')).toEqual('false.')
    })

    it('can destructure query results', async () => {
        const instance = new PrologInstance()
        await instance.initialized()
        expect(instance.consult(linksProgram)).toEqual('true.')

        
    })

})