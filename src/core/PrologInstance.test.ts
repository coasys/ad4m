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
        const consultResult = instance.consult(`
        link(1,2).
        link(2,3).
        link(A,B):-link(A,X),link(X,B).
        `)
        expect(consultResult).toEqual('true.')

        expect(instance.query('link(1,2).')).toEqual('true')
        expect(instance.query('link(1,3).')).toEqual('true')
        //expect(instance.query('link(1,4).')).toEqual('false')
    })

})