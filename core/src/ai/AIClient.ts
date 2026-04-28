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

    private _feedCount = 0;

    async feedTranscriptionStream(streamIds: string | string[], audio: Float32Array): Promise<void> {
        const ids = Array.isArray(streamIds) ? streamIds : [streamIds];

        if (this.#audioWs && this.#audioWs.readyState === WebSocket.OPEN) {
            this._feedCount++;
            if (this._feedCount % 50 === 1) {
                console.log(`[AIClient] feedTranscriptionStream via WebSocket (frame #${this._feedCount}, ${audio.length} samples)`);
            }
            this.#audioWs.send(audio.buffer);
            return;
        }

        this._feedCount++;
        if (this._feedCount % 50 === 1) {
            console.log(`[AIClient] feedTranscriptionStream via REST fallback (frame #${this._feedCount}, ${audio.length} samples, ws state: ${this.#audioWs?.readyState})`);
        }
        return this.#restClient.post<void>('/api/v1/ai/transcription/feed', {
            streamIds: ids,
            audio: Array.from(audio)
        });
    }

    private connectAudioWs(streamIds: string[]): void {
        const baseUrl = this.#restClient.getBaseUrl().replace(/^http/, 'ws');
        const token = this.#restClient.getToken();
        if (!token) {
            console.warn('[AIClient] connectAudioWs: no token, skipping WebSocket');
            return;
        }

        const idsParam = encodeURIComponent(streamIds.join(','));
        const tokenParam = encodeURIComponent(token);

        const wsUrl = `${baseUrl}/api/v1/ws/audio?token=${tokenParam}&stream_ids=${idsParam}`;
        console.log('[AIClient] connecting audio WebSocket:', wsUrl.replace(/token=[^&]+/, 'token=***'));

        this.#audioWs = new WebSocket(wsUrl);
        this.#audioWs.binaryType = 'arraybuffer';

        this.#audioWs.onopen = () => {
            console.log('[AIClient] audio WebSocket connected');
        };

        this.#audioWs.onerror = (e) => {
            console.error('[AIClient] audio WebSocket error:', e);
            this.#audioWs = null;
        };

        this.#audioWs.onclose = (e) => {
            console.log('[AIClient] audio WebSocket closed:', e.code, e.reason);
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
