import type Dna from './dna'
import type HolochainService from './HolochainService'

export default class HolochainLanguageDelegate {
    #languageHash
    #holochainService

    constructor(languageHash: string, holochainService: HolochainService) {
        this.#languageHash = languageHash
        this.#holochainService = holochainService
    }

    async registerDNAs(dnas: Dna[]) {
        return this.#holochainService.ensureInstallDNAforLanguage(this.#languageHash, dnas)
    }

    async call(dnaNick: string, zomeName: string, fnName: string, params: object|string): Promise<any> {
        return this.#holochainService.callZomeFunction(this.#languageHash, dnaNick, zomeName, fnName, params)
    }
}