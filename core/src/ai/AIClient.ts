import { RestClient } from "../restClient";
import base64js from 'base64-js';
import pako from 'pako'
import { AIModelLoadingStatus, AITask, AITaskInput } from "./Tasks";
import { ModelInput, Model, ModelType } from "./AITypes"
import type { PromptRequest, EmbedRequest, SetDefaultModelRequest } from "../generated/rest";

export class AIClient {
    #restClient: RestClient;
    #transcriptionUnsubscribers: Map<string, () => void> = new Map();

    constructor(baseUrl: string, token?: string, subscribe: boolean = true, sharedRestClient?: RestClient) {
        this.#restClient = sharedRestClient || new RestClient(baseUrl, token);
    }

    private serializeModelInput(model: ModelInput): Record<string, unknown> {
        const payload: Record<string, unknown> = { ...model };
        if ('modelType' in payload) {
            payload.type = payload.modelType;
            delete payload.modelType;
        }
        return payload;
    }

    async getModels(): Promise<Model[]> {
        return this.#restClient.call<Model[]>('ai.models');
    }

    async addModel(model: ModelInput): Promise<string> {
        return this.#restClient.call<string>('ai.addModel', { model: this.serializeModelInput(model) });
    }

    async updateModel(modelId: string, model: ModelInput): Promise<boolean> {
        return this.#restClient.call<boolean>('ai.updateModel', { id: modelId, model: this.serializeModelInput(model) });
    }

    async removeModel(modelId: string): Promise<boolean> {
        return this.#restClient.call<boolean>('ai.removeModel', { id: modelId });
    }

    async setDefaultModel(modelType: ModelType, modelId: string): Promise<boolean> {
        return this.#restClient.call<boolean>('ai.setDefaultModel', { id: modelId, modelType });
    }

    async getDefaultModel(modelType: ModelType): Promise<Model> {
        return this.#restClient.call<Model>('ai.getDefaultModel', { modelType });
    }

    async tasks(): Promise<AITask[]> {
        return this.#restClient.call<AITask[]>('ai.tasks');
    }

    async addTask(name: string, modelId: string, systemPrompt: string, promptExamples: { input: string, output: string }[], metaData?: string): Promise<AITask> {
        const task = new AITaskInput(name, modelId, systemPrompt, promptExamples, metaData);
        return this.#restClient.call<AITask>('ai.addTask', { task });
    }

    async removeTask(taskId: string): Promise<AITask> {
        return this.#restClient.call<AITask>('ai.removeTask', { id: taskId });
    }

    async updateTask(taskId: string, task: AITask): Promise<AITask> {
        return this.#restClient.call<AITask>('ai.updateTask', {
            id: taskId,
            task: {
                name: task.name,
                modelId: task.modelId,
                systemPrompt: task.systemPrompt,
                promptExamples: task.promptExamples
            }
        });
    }

    async modelLoadingStatus(model: string): Promise<AIModelLoadingStatus> {
        return this.#restClient.call<AIModelLoadingStatus>('ai.modelLoadingStatus', { model });
    }

    async prompt(taskId: string, prompt: string): Promise<string> {
        return this.#restClient.call<string>('ai.prompt', { taskId, prompt });
    }

    async embed(modelId: string, text: string): Promise<Array<number>> {
        const aiEmbed = await this.#restClient.call<string>('ai.embed', { modelId, text });

        const compressed = base64js.toByteArray(aiEmbed);
        const decompressed = JSON.parse(pako.inflate(compressed, { to: 'string' }));

        return decompressed;
    }

    async openTranscriptionStream(
        modelId: string,
        streamCallback: (text: string) => void,
        params?: {
            startThreshold?: number;
            startWindow?: number;
            endThreshold?: number;
            endWindow?: number;
            timeBeforeSpeech?: number;
        }
    ): Promise<string> {
        const streamId = await this.#restClient.call<string>('ai.transcriptionOpen', { modelId, params });

        const unsub = this.#restClient.subscribe(
            (data) => {
                if (data.type === 'transcription-text' && data.streamId === streamId && data.text) {
                    streamCallback(data.text as string);
                }
            }
        );

        this.#transcriptionUnsubscribers.set(streamId, unsub);

        return streamId;
    }

    async closeTranscriptionStream(streamId: string): Promise<void> {
        this.#pendingStreamIds.delete(streamId);
        await this.#restClient.call<void>('ai.transcriptionClose', { streamId });

        const unsub = this.#transcriptionUnsubscribers.get(streamId);
        if (unsub) {
            unsub();
            this.#transcriptionUnsubscribers.delete(streamId);
        }
    }

    #pendingStreamIds: Set<string> = new Set();

    /**
     * Feed an audio utterance to one or more transcription streams.
     * Sends raw binary Float32Array as application/octet-stream.
     * NOTE: This method still uses HTTP fetch because binary audio data
     * cannot be efficiently sent over the JSON-based WebSocket RPC protocol.
     * Transcription results are delivered via the WS event channel.
     */
    async feedTranscriptionStream(streamIds: string | string[], audio: Float32Array | number[]): Promise<void> {
        const ids = Array.isArray(streamIds) ? streamIds : [streamIds];

        // Ensure we have a typed array for binary transport
        const typedAudio = audio instanceof Float32Array
            ? audio
            : new Float32Array(audio);

        if (ids.length === 0 || typedAudio.length === 0) {
            return;
        }

        const baseUrl = this.#restClient.getBaseUrl();
        const token = this.#restClient.getToken();

        // Use slice to get only the relevant portion of the underlying ArrayBuffer
        // (Float32Array may be a view over a larger buffer)
        const bodyBuffer = typedAudio.buffer.slice(
            typedAudio.byteOffset,
            typedAudio.byteOffset + typedAudio.byteLength
        );

        const response = await fetch(`${baseUrl}/api/v1/ai/transcription/feed`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/octet-stream',
                'X-Stream-Ids': ids.join(','),
                ...(token ? { 'Authorization': `Bearer ${token}` } : {}),
            },
            body: bodyBuffer,
        });

        if (!response.ok) {
            const text = await response.text().catch(() => '');
            throw new Error(`[AIClient] feed failed: ${response.status} ${response.statusText} ${text}`);
        }
    }

}
