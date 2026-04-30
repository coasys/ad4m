import { RestClient } from "../restClient"
import { LanguageHandle } from "./LanguageHandle"
import { LanguageMeta, LanguageMetaInput } from "./LanguageMeta"
import { LanguageRef } from "./LanguageRef"
import type { ApplyTemplateRequest, PublishLanguageRequest, WriteSettingsRequest } from "../generated/rest"

export class LanguageClient {
    #restClient: RestClient

    constructor(baseUrl: string, token?: string, sharedRestClient?: RestClient) {
        this.#restClient = sharedRestClient || new RestClient(baseUrl, token)
    }

    async byAddress(address: string): Promise<LanguageHandle> {
        return this.#restClient.call<LanguageHandle>('language.get', { address })
    }

    async byFilter(filter: string): Promise<LanguageHandle[]> {
        return this.#restClient.call<LanguageHandle[]>('language.all', { filter })
    }

    async all(): Promise<LanguageHandle[]> {
        return this.#restClient.call<LanguageHandle[]>('language.all')
    }

    async writeSettings(languageAddress: string, settings: string): Promise<Boolean> {
        return this.#restClient.call<Boolean>('language.writeSettings', { address: languageAddress, settings })
    }

    async applyTemplateAndPublish(sourceLanguageHash: string, templateData: string): Promise<LanguageRef> {
        return this.#restClient.call<LanguageRef>('language.applyTemplate', { sourceLanguageHash, templateData })
    }

    async publish(languagePath: string, languageMeta: LanguageMetaInput): Promise<LanguageMeta> {
        return this.#restClient.call<LanguageMeta>('language.publish', { languagePath, languageMeta })
    }

    async meta(address: string): Promise<LanguageMeta> {
        return this.#restClient.call<LanguageMeta>('language.meta', { address })
    }

    async source(address: string): Promise<string> {
        return this.#restClient.call<string>('language.source', { address })
    }

    async remove(address: string): Promise<Boolean> {
        return this.#restClient.call<Boolean>('language.remove', { address })
    }
}
