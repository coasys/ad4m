import sleep from "../tests/sleep"
import PrologInstance from "./PrologInstance"

describe('PrologInstance', () => {
    it('smoke test', async () => {
        const instance = new PrologInstance()
        await instance.initialized()
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
    })

})