import { AIModelLoadingStatus, AITask, AITaskInput } from "./Tasks";

export class ModelApi {
    baseUrl: string;
    apiKey: string;
    model: string;
    apiType: String;
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
    apiKey: string;
    model: string;
    apiType: string;
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
