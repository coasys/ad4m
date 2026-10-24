import { AIModelLoadingStatus, AITask, AITaskInput } from "./Tasks";

export class ModelApi {
    baseUrl: string;
    /** Whether the model has a provider key. The executor never returns the key itself. */
    hasApiKey: boolean;
    model: string;
    apiType: String;
    /** Optional ceiling for the provider's context window (tokens).
     *  Read by the Ollama provider to bound num_ctx / KV-cache VRAM. */
    maxNumCtx?: number;
}

export class TokenizerSource {
    repo: string;
    revision: string;
    fileName: string;
}

export class LocalModel {
    fileName: string;
    tokenizerSource?: TokenizerSource;
    huggingfaceRepo?: string;
    revision?: string;
}

export type ModelType = "LLM" | "EMBEDDING" | "TRANSCRIPTION";

export class Model {
    id: string;
    name: string;
    api?: ModelApi;
    local?: LocalModel;
    modelType: ModelType;
}

export class ModelApiInput {
    baseUrl: string;
    /** The provider key. Empty or left out on `addModel`: no key. Left out on
     *  `updateModel`: the stored key stays, and the update is refused if `baseUrl`
     *  changes too. Empty on `updateModel`: the stored key is removed. */
    apiKey?: string;
    model: string;
    apiType: string;
    /** Optional ceiling for the provider's context window (tokens). */
    maxNumCtx?: number;
}

export class TokenizerSourceInput {
    repo: string;
    revision: string;
    fileName: string;
}

export class LocalModelInput {
    fileName: string;
    tokenizerSource?: TokenizerSourceInput;
    huggingfaceRepo?: string;
    revision?: string;
}

export class ModelInput {
    name: string;
    api?: ModelApiInput;
    local?: LocalModelInput;
    modelType: ModelType;
}

export class VoiceActivityParamsInput {
    startThreshold?: number;
    startWindow?: number;
    endThreshold?: number;
    endWindow?: number;
    timeBeforeSpeech?: number;
}
