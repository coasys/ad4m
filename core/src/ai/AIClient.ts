import { RestClient } from "../restClient";
import base64js from 'base64-js';
import pako from 'pako'
import { AIModelLoadingStatus, AITask, AITaskInput } from "./Tasks";
import { ModelInput, Model, ModelType } from "./AITypes"
import type { PromptRequest, EmbedRequest, SetDefaultModelRequest } from "../generated/rest";

export class AIClient {
    #restClient: RestClient;
    #transcriptionUnsubscribers: Map<string, () => void> = new Map();
    #audioWs: WebSocket | null = null;

    constructor(baseUrl: string, token?: string, subscribe: boolean = true, sharedRestClient?: RestClient) {
        this.#restClient = sharedRestClient || new RestClient(baseUrl, token);
    }

    async getModels(): Promise<Model[]> {
        return this.#restClient.get<Model[]>('/api/v1/ai/models');
    }

    async addModel(model: ModelInput): Promise<string> {
        return this.#restClient.post<string>('/api/v1/ai/models', { model });
    }

    async updateModel(modelId: string, model: ModelInput): Promise<boolean> {
        return this.#restClient.put<boolean>(`/api/v1/ai/models/${encodeURIComponent(modelId)}`, { model });
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
        const streamId = await this.#restClient.post<string>('/api/v1/ai/transcription/open', { modelId, params });

        // Subscribe to AI events and filter for this stream's transcription text
        const unsub = this.#restClient.subscribe(
            `/api/v1/events/ai`,
            (data) => {
                if (data.type === 'transcription-text' && data.streamId === streamId && data.text) {
                    streamCallback(data.text);
                }
            }
        );

        this.#transcriptionUnsubscribers.set(streamId, unsub);

        // Connect binary WebSocket for efficient audio transport
        this.connectAudioWs([streamId]);

        return streamId;
    }

    async closeTranscriptionStream(streamId: string): Promise<void> {
        this.disconnectAudioWs();
        await this.#restClient.post<void>('/api/v1/ai/transcription/close', { streamId });

        const unsub = this.#transcriptionUnsubscribers.get(streamId);
        if (unsub) {
            unsub();
            this.#transcriptionUnsubscribers.delete(streamId);
        }
    }

    async feedTranscriptionStream(streamIds: string | string[], audio: Float32Array): Promise<void> {
        const ids = Array.isArray(streamIds) ? streamIds : [streamIds];

        // Use WebSocket if connected (binary, efficient — no JSON serialisation)
        if (this.#audioWs && this.#audioWs.readyState === WebSocket.OPEN) {
            this.#audioWs.send(audio.buffer);
            return;
        }

        // Fallback to REST POST (JSON, less efficient)
        return this.#restClient.post<void>('/api/v1/ai/transcription/feed', {
            streamIds: ids,
            audio: Array.from(audio)
        });
    }

    private connectAudioWs(streamIds: string[]): void {
        const baseUrl = this.#restClient.getBaseUrl().replace(/^http/, 'ws');
        const token = this.#restClient.getToken();
        if (!token) return;

        const idsParam = encodeURIComponent(streamIds.join(','));
        const tokenParam = encodeURIComponent(token);

        this.#audioWs = new WebSocket(
            `${baseUrl}/api/v1/ws/audio?token=${tokenParam}&stream_ids=${idsParam}`
        );
        this.#audioWs.binaryType = 'arraybuffer';

        this.#audioWs.onerror = () => {
            this.#audioWs = null;
        };

        this.#audioWs.onclose = () => {
            this.#audioWs = null;
        };
    }

    private disconnectAudioWs(): void {
        if (this.#audioWs) {
            this.#audioWs.close();
            this.#audioWs = null;
        }
    }
}
