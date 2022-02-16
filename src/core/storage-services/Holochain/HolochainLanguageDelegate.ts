import type { Dna } from '@perspect3vism/ad4m'
import type HolochainService from './HolochainService'
import type { AppSignalCb } from '@holochain/client'
import { AsyncQueue } from './Queue'

export default class HolochainLanguageDelegate {
    #languageHash
    #holochainService
    #queue;

    constructor(languageHash: string, holochainService: HolochainService) {
        this.#languageHash = languageHash
        this.#holochainService = holochainService
        this.#queue = new AsyncQueue();
    }

    async registerDNAs(dnas: Dna[], holochainSignalCallback?: AppSignalCb) {
        return await this.#holochainService.ensureInstallDNAforLanguage(this.#languageHash, dnas, holochainSignalCallback)
    }

    async call(dnaNick: string, zomeName: string, fnName: string, params: object|string): Promise<any> {
        return await this.#queue.add(async () => {
            return await this.#holochainService.callZomeFunction(this.#languageHash, dnaNick, zomeName, fnName, params)
        })
    }
}