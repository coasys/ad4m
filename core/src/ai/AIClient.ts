import { ApolloClient, gql } from "@apollo/client";
import unwrapApolloResult from "../unwrapApolloResult";
import base64js from 'base64-js';
import pako from 'pako'
import { AIModelLoadingStatus, AITask, AITaskInput } from "./Tasks";
import { ModelInput, Model } from "./AIResolver"

export class AIClient {
    #apolloClient: ApolloClient<any>;
    #transcriptionSubscriptions: Map<string, any> = new Map();

    constructor(apolloClient: ApolloClient<any>, subscribe: boolean = true) {
        this.#apolloClient = apolloClient;
    }

    async getModels(): Promise<Model[]> {
        const result = await this.#apolloClient.query({
            query: gql`
                query {
                    runtimeGetModels {
                        name
                        api {
                            baseUrl
                            apiKey
                            apiType
                        }
                        local {
                            fileName
                            tokenizerSource
                            modelParameters
                        }
                        type
                    }
                }
            `
        });
        return unwrapApolloResult(result).runtimeGetModels;
    }

    async addModel(model: ModelInput): Promise<boolean> {
        const result = await this.#apolloClient.mutate({
            mutation: gql`
                mutation($model: ModelInput!) {
                    runtimeAddModel(model: $model)
                }
            `,
            variables: { model }
        });
        return unwrapApolloResult(result).runtimeAddModel;
    }

    async removeModel(name: string): Promise<boolean> {
        const result = await this.#apolloClient.mutate({
            mutation: gql`
                mutation($name: String!) {
                    runtimeRemoveModel(name: $name)
                }
            `,
            variables: { name }
        });
        return unwrapApolloResult(result).runtimeRemoveModel;
    }

    async tasks(): Promise<AITask[]> {
        const { aiTasks } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`
                query {
                    aiTasks {
                        name
                        modelId
                        taskId
                        systemPrompt
                        promptExamples {
                            input
                            output
                        }
                        metaData
                        createdAt
                        updatedAt
                    }
                }
            `
        }));

        return aiTasks;
    }

    async addTask(name: string, modelId: string, systemPrompt: string, promptExamples: { input: string, output: string }[], metaData?: string): Promise<AITask> {
        const task = new AITaskInput(name, modelId, systemPrompt, promptExamples, metaData);
        const { aiAddTask } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiAddTask($task: AITaskInput!) {
                    aiAddTask(task: $task) {
                        name
                        modelId
                        taskId
                        systemPrompt
                        promptExamples {
                            input
                            output
                        }
                        metaData
                        createdAt
                        updatedAt
                    }
                }
            `,
            variables: {
                task
            }
        }));

        return aiAddTask;
    }

    async removeTask(taskId: string): Promise<AITask> {
        const { aiRemoveTask } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiRemoveTask($taskId: String!) {
                    aiRemoveTask(taskId: $taskId) {
                        name
                        modelId
                        taskId
                        systemPrompt
                        promptExamples {
                            input
                            output
                        }
                        metaData
                        createdAt
                        updatedAt
                    }
                }
            `,
            variables: {
                taskId
            }
        }));

        return aiRemoveTask;
    }

    async updateTask(taskId: string, task: AITask): Promise<AITask> {
        const { aiUpdateTask } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiUpdateTask($taskId: String!, $task: AITaskInput!) {
                    aiUpdateTask(taskId: $taskId, task: $task) {
                        name
                        modelId
                        taskId
                        systemPrompt
                        promptExamples {
                            input
                            output
                        }
                        metaData
                        createdAt
                        updatedAt
                    }
                }
            `,
            variables: {
                taskId,
                task: {
                    name: task.name,
                    modelId: task.modelId,
                    systemPrompt: task.systemPrompt,
                    promptExamples: task.promptExamples
                }
            }
        }));

        return aiUpdateTask;
    }

    async modelLoadingStatus(model: string): Promise<AIModelLoadingStatus> {
        const { aiModelLoadingStatus } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`
                query AiModelLoadingStatus($model: String!) {
                    aiModelLoadingStatus(model: $model) {
                        model
                        status
                        progress
                        loaded
                        downloaded
                    }
                }
            `,
            variables: {
                model
            }
        }));

        return aiModelLoadingStatus
    }

    async prompt(taskId: string, prompt: string): Promise<string> {
        const { aiPrompt } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiPrompt($taskId: String!, $prompt: String!) {
                    aiPrompt(taskId: $taskId, prompt: $prompt)
                }
            `,
            variables: {
                taskId,
                prompt
            }
        }));

        return aiPrompt;
    }

    async embed(modelId: string, text: string): Promise<Array<number>> {
        const { aiEmbed } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation aiEmbed($modelId: String!, $text: String!) {
                    aiEmbed(modelId: $modelId, text: $text)
                }
            `,
            variables: {
                modelId,
                text
            }
        }));

        const compressed = base64js.toByteArray(aiEmbed);

        const decompressed = JSON.parse(pako.inflate(compressed, { to: 'string' }));

        return decompressed;
    }

    async openTranscriptionStream(modelId: string, streamCallback: (text: string) => void): Promise<string> {
        const { aiOpenTranscriptionStream } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiOpenTranscriptionStream($modelId: String!) {
                    aiOpenTranscriptionStream(modelId: $modelId)
                }
            `,
            variables: {
                modelId
            }
        }));

        const subscription = this.#apolloClient.subscribe({
            query: gql` subscription {
                aiTranscriptionText(streamId: "${aiOpenTranscriptionStream}")
            }`
        }).subscribe({
            next(data) {
                streamCallback(data.data.aiTranscriptionText);

                return data.data.aiTranscriptionText;
            },
            error(err) {
                console.error(err);
            }
        });

        this.#transcriptionSubscriptions.set(aiOpenTranscriptionStream, subscription);

        return aiOpenTranscriptionStream;
    }

    async closeTranscriptionStream(streamId: string): Promise<void> {
        const { aiCloseTranscriptionStream } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation aiCloseTranscriptionStream($streamId: String!) {
                    aiCloseTranscriptionStream(streamId: $streamId)
                }
            `,
            variables: {
                streamId
            }
        }));

        const subscription = this.#transcriptionSubscriptions.get(streamId);

        if (!subscription.closed) {
            subscription.unsubscribe();
        }

        return aiCloseTranscriptionStream;
    }

    async feedTranscriptionStream(streamId: string, audio: Float32Array): Promise<void> {
        const { feedTranscriptionStream } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiFeedTranscriptionStream($streamId: String!, $audio: [Float!]!) {
                    aiFeedTranscriptionStream(streamId: $streamId, audio: $audio)
                }
            `,
            variables: {
                streamId,
                audio: audio
            }
        }));

        return feedTranscriptionStream;
    }
}