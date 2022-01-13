import fs from "fs-extra";
//@ts-ignore
import swipl from 'swipl-stdio'
//@ts-ignore
import tmp from 'tmp'

export default class PrologInstance {
    #engine

    constructor() {
        this.#engine = new swipl.Engine()
    }

    async query(input: string) {
        const query = await this.#engine.createQuery(input);
        let allMatches = []
        try {
            let ret = null;
            while (ret = await query.next()) {
                allMatches.push(ret)
            }
        } finally {
            await query.close()
        }

        if(JSON.stringify(allMatches) === JSON.stringify([{}]))
            return true
        if(JSON.stringify(allMatches) === JSON.stringify([]))
            return false

        return allMatches
    }

    async call(query: string) {
        return await this.#engine.call(query)
    };

    async consult(program: string) {
        const tmpobj = tmp.fileSync()
        //@ts-ignore
        fs.writeFileSync(tmpobj.name, program);
        const result = await this.call(`consult('${tmpobj.name}').`)
        tmpobj.removeCallback()
        return result
    }

    close() {
        this.#engine.close()
    }
}