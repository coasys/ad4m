import { ApiClient } from "../apiClient";
import { InteractionCall, InteractionMeta } from "../language/Language";
import { ExpressionRendered } from "./Expression";
import { Literal } from "../Literal";
import type { CreateExpressionRequest, ExpressionManyRequest } from "../generated/api";

export class ExpressionClient {
    #apiClient: ApiClient

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token)
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

        return this.#apiClient.call<ExpressionRendered>('expression.get', { url })
    }

    async getMany(urls: string[]): Promise<ExpressionRendered[]> {
        return this.#apiClient.call<ExpressionRendered[]>('expression.getMany', { urls })
    }

    async getRaw(url: string): Promise<string> {
        return this.#apiClient.call<string>('expression.get', { url, raw: true })
    }

    async create(content: unknown, languageAddress: string): Promise<string> {
        const serialized = JSON.stringify(content)
        return this.#apiClient.call<string>('expression.create', { content: serialized, languageAddress })
    }

    async interactions(url: string): Promise<InteractionMeta[]> {
        return this.#apiClient.call<InteractionMeta[]>('expression.interactions', { url })
    }

    async interact(url: string, interactionCall: InteractionCall): Promise<string|null> {
        return this.#apiClient.call<string|null>('expression.interact', { url, interactionCall })
    }
}
