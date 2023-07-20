import { randomUUID } from "crypto";

export default class PrologInstance {
    prologService = PROLOG;
    name: string;

    constructor() {
        this.name = randomUUID()
    }

    async start() {
        return await this.prologService.spawnEngine(this.name)
    }

    async query(input: string) {
        return await this.prologService.runQuery(this.name, input)

        //TODO; add parsing of the result

        // const query = await this.#engine.createQuery(input);
        // let allMatches = []
        // try {
        //     let ret = null;
        //     while (ret = await query.next()) {
        //         allMatches.push(ret)
        //     }
        // } finally {
        //     await query.close()
        // }

        // if(JSON.stringify(allMatches) === JSON.stringify([{}]))
        //     return true
        // if(JSON.stringify(allMatches) === JSON.stringify([]))
        //     return false

        // return allMatches
    }

    async call(query: string) {
        return await this.prologService.runQuery(this.name, query)

        //TODO; add parsing of the result

        // return await this.#engine.call(query)
    };

    async consult(program: string, moduleName?: string) {
        return await this.prologService.loadModuleString(this.name, program, moduleName || "main")
        // const tmpobj = tmp.fileSync()
        // //@ts-ignore
        // fs.writeFileSync(tmpobj.name, program);
        // const result = await this.call(`consult('${tmpobj.name}').`)
        // tmpobj.removeCallback()
        // return result
    }
}