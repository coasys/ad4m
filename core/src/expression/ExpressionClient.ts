import { RestClient } from "../restClient";
import { InteractionCall, InteractionMeta } from "../language/Language";
import { ExpressionRendered } from "./Expression";
import { Literal } from "../Literal";
import type { CreateExpressionRequest, ExpressionManyRequest } from "../generated/rest";

export class ExpressionClient {
    #restClient: RestClient

    constructor(baseUrl: string, token?: string, sharedRestClient?: RestClient) {
        this.#restClient = sharedRestClient || new RestClient(baseUrl, token)
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

        return this.#restClient.get<ExpressionRendered>(`/api/v1/expressions/${encodeURIComponent(url)}`)
    }

    async getMany(urls: string[]): Promise<ExpressionRendered[]> {
        const body: ExpressionManyRequest = { urls }
        return this.#restClient.post<ExpressionRendered[]>('/api/v1/expressions/many', body)
    }

    async getRaw(url: string): Promise<string> {
        return this.#restClient.get<string>(`/api/v1/expressions/${encodeURIComponent(url)}?raw=true`)
    }

    async create(content: unknown, languageAddress: string): Promise<string> {
        const serialized = JSON.stringify(content)
        const body: CreateExpressionRequest = { content: serialized, languageAddress }
        return this.#restClient.post<string>('/api/v1/expressions', body)
    }

    async interactions(url: string): Promise<InteractionMeta[]> {
        return this.#restClient.get<InteractionMeta[]>(`/api/v1/expressions/${encodeURIComponent(url)}/interactions`)
    }

    async interact(url: string, interactionCall: InteractionCall): Promise<string|null> {
        return this.#restClient.post<string|null>(`/api/v1/expressions/${encodeURIComponent(url)}/interact`, { interactionCall })
    }
}
