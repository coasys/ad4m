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
        return this.#restClient.get<Model[]>('/api/v1/ai/models');
    }

    async addModel(model: ModelInput): Promise<string> {
        return this.#restClient.post<string>('/api/v1/ai/models', { model: this.serializeModelInput(model) });
    }

    async updateModel(modelId: string, model: ModelInput): Promise<boolean> {
        return this.#restClient.put<boolean>(`/api/v1/ai/models/${encodeURIComponent(modelId)}`, { model: this.serializeModelInput(model) });
    }

    async removeModel(modelId: string): Promise<boolean> {
        return this.#restClient.delete<boolean>(`/api/v1/ai/models/${encodeURIComponent(modelId)}`);
    }

    async setDefaultModel(modelType: ModelType, modelId: string): Promise<boolean> {
        return this.#restClient.put<boolean>(`/api/v1/ai/models/${encodeURIComponent(modelId)}/default`, { modelType });
    }

    async getDefaultModel(modelType: ModelType): Promise<Model> {
        return this.#restClient.get<Model>(`/api/v1/ai/models/default?modelType=${encodeURIComponent(modelType)}`);
    }

    async tasks(): Promise<AITask[]> {
        return this.#restClient.get<AITask[]>('/api/v1/ai/tasks');
    }

    async addTask(name: string, modelId: string, systemPrompt: string, promptExamples: { input: string, output: string }[], metaData?: string): Promise<AITask> {
        const task = new AITaskInput(name, modelId, systemPrompt, promptExamples, metaData);
        return this.#restClient.post<AITask>('/api/v1/ai/tasks', { task });
    }

    async removeTask(taskId: string): Promise<AITask> {
        return this.#restClient.delete<AITask>(`/api/v1/ai/tasks/${encodeURIComponent(taskId)}`);
    }

    async updateTask(taskId: string, task: AITask): Promise<AITask> {
        return this.#restClient.put<AITask>(`/api/v1/ai/tasks/${encodeURIComponent(taskId)}`, {
            task: {
                name: task.name,
                modelId: task.modelId,
                systemPrompt: task.systemPrompt,
                promptExamples: task.promptExamples
            }
        });
    }

    async modelLoadingStatus(model: string): Promise<AIModelLoadingStatus> {
        return this.#restClient.get<AIModelLoadingStatus>(`/api/v1/ai/model-loading-status?model=${encodeURIComponent(model)}`);
    }

    async prompt(taskId: string, prompt: string): Promise<string> {
        return this.#restClient.post<string>('/api/v1/ai/prompt', { taskId, prompt });
    }

    async embed(modelId: string, text: string): Promise<Array<number>> {
        const aiEmbed = await this.#restClient.post<string>('/api/v1/ai/embed', { modelId, text });

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
        console.log('[AIClient] openTranscriptionStream:', modelId, params);
        const streamId = await this.#restClient.post<string>('/api/v1/ai/transcription/open', { modelId, params });
        console.log('[AIClient] transcription stream opened, streamId:', streamId);

        const unsub = this.#restClient.subscribe(
            `/api/v1/events/ai`,
            (data) => {
                console.log('[AIClient] SSE event received:', data);
                if (data.type === 'transcription-text' && data.streamId === streamId && data.text) {
                    console.log('[AIClient] transcription text for stream', streamId, ':', data.text);
                    streamCallback(data.text as string);
                }
            }
        );

        this.#transcriptionUnsubscribers.set(streamId, unsub);

        return streamId;
    }

    async closeTranscriptionStream(streamId: string): Promise<void> {
        this.#pendingStreamIds.delete(streamId);
        await this.#restClient.post<void>('/api/v1/ai/transcription/close', { streamId });

        const unsub = this.#transcriptionUnsubscribers.get(streamId);
        if (unsub) {
            unsub();
            this.#transcriptionUnsubscribers.delete(streamId);
        }
    }

    private _feedCount = 0;
    #pendingStreamIds: Set<string> = new Set();

    /**
     * Feed an audio utterance to one or more transcription streams.
     * Sends raw binary Float32Array as application/octet-stream.
     * Transcription results are delivered via the SSE channel registered
     * in openTranscriptionStream (/events/ai).
     */
    async feedTranscriptionStream(streamIds: string | string[], audio: Float32Array | number[]): Promise<void> {
        const ids = Array.isArray(streamIds) ? streamIds : [streamIds];

        // Ensure we have a typed array for binary transport
        const typedAudio = audio instanceof Float32Array
            ? audio
            : new Float32Array(audio);

        this._feedCount++;
        if (this._feedCount % 50 === 1) {
            console.log(`[AIClient] feedTranscriptionStream binary (frame #${this._feedCount}, ${typedAudio.length} samples)`);
        }

        const baseUrl = this.#restClient.getBaseUrl();
        const token = this.#restClient.getToken();
        const response = await fetch(`${baseUrl}/api/v1/ai/transcription/feed`, {
            method: 'POST',
            headers: {
                'Content-Type': 'application/octet-stream',
                'X-Stream-Ids': ids.join(','),
                ...(token ? { 'Authorization': `Bearer ${token}` } : {}),
            },
            body: typedAudio.buffer as ArrayBuffer,
        });

        if (!response.ok) {
            console.error(`[AIClient] feed failed: ${response.status} ${response.statusText}`);
        }
    }

}
