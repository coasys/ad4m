import {ApiClient, CallOptions } from '../apiClient';
import base64js from 'base64-js';
import pako from 'pako'
import { AIModelLoadingStatus, AITask, AITaskInput } from "./Tasks";
import { ModelInput, Model, ModelType } from "./AITypes"
import type { ModelInput as ModelInputData } from "../generated/api/ModelInput";

export class AIClient {
    #apiClient: ApiClient;
    #transcriptionUnsubscribers: Map<string, () => void> = new Map();

    constructor(baseUrl: string, token?: string, sharedApiClient?: ApiClient) {
        this.#apiClient = sharedApiClient || new ApiClient(baseUrl, token);
    }

    /** The executor names the model type `type`. */
    private serializeModelInput({ modelType, ...model }: ModelInput): ModelInputData {
        return { ...model, type: modelType };
    }

    async getModels(): Promise<Model[]> {
        return this.#apiClient.call('ai.models', {});
    }

    /**
     * Ask a remote endpoint which models it serves, before adding one.
     *
     * Takes the credentials of a model that does not exist yet — a settings
     * form is being filled in and wants the list to pick from. Rejects when
     * the endpoint is unreachable or the key is refused, which makes this the
     * credential check too: without it a bad key surfaces later as a failed
     * completion carrying an error from a different layer.
     *
     * `apiType` defaults to the OpenAI shape, which is what every endpoint
     * that is not Anthropic speaks.
     */
    async discoverModels(baseUrl: string, apiKey?: string, apiType?: string): Promise<string[]> {
        return this.#apiClient.call('ai.discoverModels', { baseUrl, apiKey, apiType });
    }

    async addModel(model: ModelInput, options?: CallOptions): Promise<string> {
        return this.#apiClient.call('ai.addModel', { model: this.serializeModelInput(model) }, options);
    }

    async updateModel(modelId: string, model: ModelInput): Promise<boolean> {
        return this.#apiClient.call('ai.updateModel', { id: modelId, model: this.serializeModelInput(model) });
    }

    async removeModel(modelId: string): Promise<boolean> {
        return this.#apiClient.call('ai.removeModel', { id: modelId });
    }

    async setDefaultModel(modelType: ModelType, modelId: string): Promise<boolean> {
        return this.#apiClient.call('ai.setDefaultModel', { id: modelId, modelType });
    }

    async getDefaultModel(modelType: ModelType): Promise<Model> {
        return this.#apiClient.call('ai.getDefaultModel', { modelType });
    }

    async tasks(): Promise<AITask[]> {
        return this.#apiClient.call('ai.tasks', {});
    }

    async addTask(name: string, modelId: string, systemPrompt: string, promptExamples: { input: string, output: string }[], metaData?: string): Promise<AITask> {
        const task = new AITaskInput(name, modelId, systemPrompt, promptExamples, metaData);
        return this.#apiClient.call('ai.addTask', { task });
    }

    async removeTask(taskId: string): Promise<boolean> {
        return this.#apiClient.call('ai.removeTask', { id: taskId });
    }

    async updateTask(taskId: string, task: AITask): Promise<AITask> {
        return this.#apiClient.call('ai.updateTask', {
            task: {
                taskId,
                name: task.name,
                modelId: task.modelId,
                systemPrompt: task.systemPrompt,
                promptExamples: task.promptExamples,
                metaData: task.metaData,
                createdAt: task.createdAt,
                updatedAt: task.updatedAt,
            }
        });
    }

    async modelLoadingStatus(model: string): Promise<AIModelLoadingStatus> {
        return this.#apiClient.call('ai.modelLoadingStatus', { model });
    }

    async prompt(taskId: string, prompt: string, options?: CallOptions): Promise<string> {
        return this.#apiClient.call('ai.prompt', { taskId, prompt }, options);
    }

    async embed(modelId: string, text: string, options?: CallOptions): Promise<Array<number>> {
        const aiEmbed = await this.#apiClient.call('ai.embed', { modelId, text }, options);

        const compressed = base64js.toByteArray(aiEmbed);
        // NB: pako v1 accepts `{ to: 'string' }`, pako v2 wants `{ toText: true }`,
        // pako v3 also drops that overload from its bundled types. Call the
        // version-agnostic form (returns Uint8Array) and decode explicitly so
        // this works regardless of which pako major the lockfile pins.
        const inflated = pako.inflate(compressed);
        const decompressed = JSON.parse(new TextDecoder('utf-8').decode(inflated));

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
        const streamId = await this.#apiClient.call('ai.transcriptionOpen', { modelId, params });

        const unsub = this.#apiClient.on('transcription-text', (event) => {
            if (event.streamId === streamId && event.text) streamCallback(event.text);
        });

        this.#transcriptionUnsubscribers.set(streamId, unsub);

        return streamId;
    }

    async closeTranscriptionStream(streamId: string): Promise<void> {
        this.#pendingStreamIds.delete(streamId);
        try {
            await this.#apiClient.call('ai.transcriptionClose', { streamId });
        } finally {
            this.#transcriptionUnsubscribers.get(streamId)?.();
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
     *
     * Rejects if any stream did not take the audio. When only some failed, the message names them
     * and the others were fed, so retry only the failed ids.
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

        const baseUrl = this.#apiClient.getBaseUrl();
        const token = this.#apiClient.getToken();

        // Use slice to get only the relevant portion of the underlying ArrayBuffer
        // (Float32Array may be a view over a larger buffer)
        const bodyBuffer = typedAudio.buffer.slice(
            typedAudio.byteOffset,
            typedAudio.byteOffset + typedAudio.byteLength
        );

        const response = await this.#apiClient.doFetch(`${baseUrl}/api/v1/ai/transcription/feed`, {
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
