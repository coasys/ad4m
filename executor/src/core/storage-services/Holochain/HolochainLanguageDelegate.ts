import type { Dna } from '@perspect3vism/ad4m'
import type HolochainService from './HolochainService'
import type { AppSignalCb } from '@holochain/client'
import { AsyncQueue } from './Queue'

export default class HolochainLanguageDelegate {
    #languageHash
    #holochainService
    #queue;

    constructor(languageHash: string, holochainService: HolochainService, queue: AsyncQueue) {
        this.#languageHash = languageHash
        this.#holochainService = holochainService
        this.#queue = queue
    }

    async registerDNAs(dnas: Dna[], holochainSignalCallback?: AppSignalCb): Promise<void> {
        const _cells = await this.#holochainService.ensureInstallDNAforLanguage(this.#languageHash, dnas, holochainSignalCallback);
        return;
    }

    async call(dnaNick: string, zomeName: string, fnName: string, params: object|string): Promise<any> {
        return await this.#queue.add(async () => {
            return await this.#holochainService.callZomeFunction(this.#languageHash, dnaNick, zomeName, fnName, params)
        })
    }

    async callAsync(calls: {dnaNick: string, zomeName: string, fnName: string, params: object|string}[], timeoutMs?: number): Promise<any[]> {
        const promises = [];
        for (const call of calls) {
            promises.push(new Promise(async (resolve, reject) => {
                if (timeoutMs) {
                    setTimeout(() => setTimeout(() => reject(Error(`NH: ZomeCall hit timeout... rejecting`)), timeoutMs));
                }
                let res = await this.#holochainService.callZomeFunction(this.#languageHash, call.dnaNick, call.zomeName, call.fnName, call.params)
                resolve(res);
            }));
        };
        return await Promise.allSettled(promises).then((results) => {
            const succeeds = [];
            for (const result of results) {
                if (result.status === "fulfilled") {
                    succeeds.push(result.value);
                }
            };
            return succeeds;
        });
    }
}