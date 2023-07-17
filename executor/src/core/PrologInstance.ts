export default class PrologInstance {
    prologService = PROLOG;

    constructor() {
    }

    async start() {
        return await this.prologService.startPrologService()
    }

    async query(input: string) {
        return await this.prologService.runQuery(input)

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
        return await this.prologService.runQuery(query)

        //TODO; add parsing of the result

        // return await this.#engine.call(query)
    };

    async consult(program: string, moduleName?: string) {
        return await this.prologService.loadModuleString(program, moduleName || "main")
        // const tmpobj = tmp.fileSync()
        // //@ts-ignore
        // fs.writeFileSync(tmpobj.name, program);
        // const result = await this.call(`consult('${tmpobj.name}').`)
        // tmpobj.removeCallback()
        // return result
    }
}