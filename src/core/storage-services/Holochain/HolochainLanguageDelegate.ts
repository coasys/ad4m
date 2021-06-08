import type { Dna } from '@perspect3vism/ad4m/LanguageContext'
import type HolochainService from './HolochainService'
import type { AppSignalCb } from '@holochain/conductor-api'

export default class HolochainLanguageDelegate {
    #languageHash
    #holochainService

    constructor(languageHash: string, holochainService: HolochainService) {
        this.#languageHash = languageHash
        this.#holochainService = holochainService
    }

    async registerDNAs(dnas: Dna[], holochainSignalCallback?: AppSignalCb) {
        return await this.#holochainService.ensureInstallDNAforLanguage(this.#languageHash, dnas, holochainSignalCallback)
    }

    async call(dnaNick: string, zomeName: string, fnName: string, params: object|string): Promise<any> {
        return this.#holochainService.callZomeFunction(this.#languageHash, dnaNick, zomeName, fnName, params)
    }
}