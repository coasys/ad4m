import { ApiClient, CallOptions, longCall } from "../apiClient"
import { LanguageHandle } from "./LanguageHandle"
import { LanguageMeta, LanguageMetaInput } from "./LanguageMeta"
import { LanguageRef } from "./LanguageRef"
import type { ApplyTemplateRequest, PublishLanguageRequest, WriteSettingsRequest } from "../generated/api"

export class LanguageClient {
    #apiClient: ApiClient

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
    }

    async byAddress(address: string): Promise<LanguageHandle> {
        return this.#apiClient.call<LanguageHandle>('language.get', { address })
    }

    async byFilter(filter: string): Promise<LanguageHandle[]> {
        return this.#apiClient.call<LanguageHandle[]>('language.all', { filter })
    }

    async all(): Promise<LanguageHandle[]> {
        return this.#apiClient.call<LanguageHandle[]>('language.all')
    }

    async writeSettings(languageAddress: string, settings: string): Promise<Boolean> {
        return this.#apiClient.call<Boolean>('language.writeSettings', { address: languageAddress, settings })
    }

    async applyTemplateAndPublish(sourceLanguageHash: string, templateData: string, options?: CallOptions): Promise<LanguageRef> {
        return this.#apiClient.call<LanguageRef>('language.applyTemplate', { sourceLanguageHash, templateData }, longCall(options))
    }

    async publish(languagePath: string, languageMeta: LanguageMetaInput, options?: CallOptions): Promise<LanguageMeta> {
        return this.#apiClient.call<LanguageMeta>('language.publish', { languagePath, languageMeta }, longCall(options))
    }

    async meta(address: string): Promise<LanguageMeta> {
        return this.#apiClient.call<LanguageMeta>('language.meta', { address })
    }

    async source(address: string): Promise<string> {
        return this.#apiClient.call<string>('language.source', { address })
    }

    async remove(address: string): Promise<Boolean> {
        return this.#apiClient.call<Boolean>('language.remove', { address })
    }
}
