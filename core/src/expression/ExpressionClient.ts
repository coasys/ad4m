import { RestClient } from "../restClient";
import { InteractionCall, InteractionMeta } from "../language/Language";
import { ExpressionRendered } from "./Expression";
import { Literal } from "../Literal";

export class ExpressionClient {
    #restClient: RestClient

    constructor(baseUrl: string, token?: string) {
        this.#restClient = new RestClient(baseUrl, token)
    }

    async get(url: string, alwaysGet: boolean = false): Promise<ExpressionRendered> {
        if(!alwaysGet){
            try {
                let literalValue = Literal.fromUrl(url).get();
                if (typeof literalValue === 'object' && literalValue !== null) {
                    if ('author' in literalValue && 'timestamp' in literalValue && 'data' in literalValue && 'proof' in literalValue) {
                        return literalValue;
                    }
                }
            } catch(e) {}
        }

        return this.#restClient.get<ExpressionRendered>(`/api/v1/expressions?url=${encodeURIComponent(url)}`)
    }

    async getMany(urls: string[]): Promise<ExpressionRendered[]> {
        const params = urls.map(u => `urls=${encodeURIComponent(u)}`).join('&')
        return this.#restClient.get<ExpressionRendered[]>(`/api/v1/expressions/many?${params}`)
    }

    async getRaw(url: string): Promise<string> {
        return this.#restClient.get<string>(`/api/v1/expressions/raw?url=${encodeURIComponent(url)}`)
    }

    async create(content: any, languageAddress: string): Promise<string> {
        content = JSON.stringify(content)
        return this.#restClient.post<string>('/api/v1/expressions', { content, languageAddress })
    }

    async interactions(url: string): Promise<InteractionMeta[]> {
        return this.#restClient.get<InteractionMeta[]>(`/api/v1/expressions/interactions?url=${encodeURIComponent(url)}`)
    }

    async interact(url: string, interactionCall: InteractionCall): Promise<string|null> {
        return this.#restClient.post<string|null>(`/api/v1/expressions/interact`, { url, interactionCall })
    }
}
