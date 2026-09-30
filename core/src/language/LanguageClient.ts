import {ApiClient, CallOptions } from '../apiClient'
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
        return this.#apiClient.call('language.get', { address })
    }

    async byFilter(filter: string): Promise<LanguageHandle[]> {
        return this.#apiClient.call('language.all', { filter })
    }

    async all(): Promise<LanguageHandle[]> {
        return this.#apiClient.call('language.all', {})
    }

    async writeSettings(languageAddress: string, settings: string): Promise<Boolean> {
        return this.#apiClient.call('language.writeSettings', { address: languageAddress, settings })
    }

    async applyTemplateAndPublish(sourceLanguageHash: string, templateData: string, options?: CallOptions): Promise<LanguageRef> {
        return this.#apiClient.call('language.applyTemplate', { sourceLanguageHash, templateData }, options)
    }

    async publish(languagePath: string, languageMeta: LanguageMetaInput, options?: CallOptions): Promise<LanguageMeta> {
        return this.#apiClient.call('language.publish', { languagePath, languageMeta }, options)
    }

    async meta(address: string): Promise<LanguageMeta> {
        return this.#apiClient.call('language.meta', { address })
    }

    async source(address: string): Promise<string> {
        return this.#apiClient.call('language.source', { address })
    }

    async remove(address: string): Promise<Boolean> {
        return this.#apiClient.call('language.remove', { address })
    }
}
