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
        return await this.#holochainService.ensureInstallDNAforLanguage(this.#languageHash, dnas, holochainSignalCallback)
    }

    async call(dnaNick: string, zomeName: string, fnName: string, params: object|string): Promise<any> {
        return await this.#queue.add(async () => {
            return await this.#holochainService.callZomeFunction(this.#languageHash, dnaNick, zomeName, fnName, params)
        })
    }

    async callAsync(calls: {dnaNick: string, zomeName: string, fnName: string, params: object|string}[]): Promise<any[]> {
        const promises = [];
        for (const call of calls) {
            promises.push(this.#holochainService.callZomeFunction(this.#languageHash, call.dnaNick, call.zomeName, call.fnName, call.params));
        };
        return await Promise.all(promises);
    }
}