import PrologInstance from "./PrologInstance"

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
        const program = `
link(1,2).
link(2,3).
link(A,B):-link(A,X),link(X,B).
`
        instance.setProgram(program)
        expect(instance.getStdout()).toEqual('true.')
    })

})