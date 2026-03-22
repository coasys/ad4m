import { RestClient } from "../restClient"
import { LanguageHandle } from "./LanguageHandle"
import { LanguageMeta, LanguageMetaInput } from "./LanguageMeta"
import { LanguageRef } from "./LanguageRef"

export class LanguageClient {
    #restClient: RestClient

    constructor(baseUrl: string, token?: string) {
        this.#restClient = new RestClient(baseUrl, token)
    }

    async byAddress(address: string): Promise<LanguageHandle> {
        return this.#restClient.get<LanguageHandle>(`/api/v1/languages/${encodeURIComponent(address)}`)
    }

    async byFilter(filter: string): Promise<LanguageHandle[]> {
        return this.#restClient.get<LanguageHandle[]>(`/api/v1/languages?filter=${encodeURIComponent(filter)}`)
    }

    async all(): Promise<LanguageHandle[]> {
        return this.byFilter('')
    }

    async writeSettings(languageAddress: string, settings: string): Promise<Boolean> {
        return this.#restClient.put<Boolean>(`/api/v1/languages/${encodeURIComponent(languageAddress)}/settings`, { settings })
    }

    async applyTemplateAndPublish(sourceLanguageHash: string, templateData: string): Promise<LanguageRef> {
        return this.#restClient.post<LanguageRef>('/api/v1/languages/apply-template', { sourceLanguageHash, templateData })
    }

    async publish(languagePath: string, languageMeta: LanguageMetaInput): Promise<LanguageMeta> {
        return this.#restClient.post<LanguageMeta>('/api/v1/languages/publish', { languagePath, languageMeta })
    }

    async meta(address: string): Promise<LanguageMeta> {
        return this.#restClient.get<LanguageMeta>(`/api/v1/languages/${encodeURIComponent(address)}/meta`)
    }

    async source(address: string): Promise<string> {
        return this.#restClient.get<string>(`/api/v1/languages/${encodeURIComponent(address)}/source`)
    }

    async remove(address: string): Promise<Boolean> {
        return this.#restClient.delete<Boolean>(`/api/v1/languages/${encodeURIComponent(address)}`)
    }
}
