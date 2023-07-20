import Perspective from "./Perspective";
import { randomUUID } from "crypto";

export default class PrologInstance {
    //@ts-ignore
    prologService = PROLOG_SERVICE;
    name: string;

    constructor(perspective: Perspective) {
        this.name = perspective.uuid || randomUUID()
    }

    async start() {
        //console.log("Starting prolog instance", this.name)
        return await this.prologService.spawnEngine(this.name)
    }

    async query(input: string) {
        //console.log("Querying prolog instance", this.name, input)
        let result = await this.prologService.runQuery(this.name, input)
        //console.log("Got Prolog result", result)
        return result

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
        //console.log("Calling prolog instance", this.name, query)
        return await this.prologService.runQuery(this.name, query)

        //TODO; add parsing of the result

        // return await this.#engine.call(query)
    };

    async consult(program_lines: string[]) {
        //console.log("PrologInstance.consult", this.name, program, moduleName)
        return await this.prologService.loadModuleString(this.name, "main.pl", program_lines)
        // const tmpobj = tmp.fileSync()
        // //@ts-ignore
        // fs.writeFileSync(tmpobj.name, program);
        // const result = await this.call(`consult('${tmpobj.name}').`)
        // tmpobj.removeCallback()
        // return result
    }
}