import { ApolloClient, gql } from "@apollo/client";
import unwrapApolloResult from "../unwrapApolloResult";
import { Task } from "./Tasks";
import base64js from 'base64js';
import pako from 'pako'

export class AIClient {
    #apolloClient: ApolloClient<any>;
    #transcriptionSubscriptions: Map<string, any> = new Map();

    constructor(apolloClient: ApolloClient<any>, subscribe: boolean = true) {
        this.#apolloClient = apolloClient;
    }

    async tasks(): Promise<Task[]> {
        const { tasks } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`
                query {
                    aiTasks {
                        modelId
                        taskId
                        systemPrompt
                        promptExamples {
                            input
                            output
                        }
                    }
                }
            `
        }));

        return tasks;
    }

    async addTask(modelId: string, systemPrompt: string, promptExamples: { input: string, output: string }[]): Promise<Task> {
        const { addTask } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiAddTask($modelId: String!, $systemPrompt: String!, $promptExamples: [PromptExamplesInput!]!) {
                    aiAddTask(modelId: $modelId, systemPrompt: $systemPrompt, promptExamples: $promptExamples) {
                        modelId
                        taskId
                        systemPrompt
                        promptExamples {
                            input
                            output
                        }
                    }
                }
            `,
            variables: {
                modelId,
                systemPrompt,
                promptExamples
            }
        }));

        return addTask;
    }

    async removeTask(taskId: string): Promise<Task> {
        const { removeTask } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiRemoveTask($taskId: String!) {
                    aiRemoveTask(taskId: $taskId) {
                        modelId
                        taskId
                        systemPrompt
                        promptExamples {
                            input
                            output
                        }
                    }
                }
            `,
            variables: {
                taskId
            }
        }));

        return removeTask;
    }

    async updateTask(taskId: string, task: Task): Promise<Task> {
        const { updateTask } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiUpdateTask($taskId: String!, $task: TaskInput!) {
                    aiUpdateTask(taskId: $taskId, task: $task) {
                        modelId
                        taskId
                        systemPrompt
                        promptExamples {
                            input
                            output
                        }
                    }
                }
            `,
            variables: {
                taskId,
                task
            }
        }));

        return updateTask;
    }

    async prompt(taskId: string, prompt: string): Promise<string> {
        const { prompt: output } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiPrompt($taskId: String!, $prompt: String!) {
                    aiPrompt(taskId: $taskId, prompt: $prompt) {
                        result
                    }
                }
            `,
            variables: {
                taskId,
                prompt
            }
        }));

        return output.result;
    }

    async embed(modelId: string, text: string): Promise<Array<number>> {
        const { embed } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiEmbed($modelId: String!, $text: String!) {
                    aiEmbed(modelId: $modelId, text: $text)
                }
            `,
            variables: {
                modelId,
                text
            }
        }));

        const compressed = base64js.toByteArray(embed);

        const decompressed = JSON.parse(pako.inflate(compressed));

        return decompressed;
    }

    async openTranscriptionStream(modelId: string, streamCallback: (text: string) => void): Promise<string> {
        const { openTranscriptionStream } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiOpenTranscriptionStream {
                    aiOpenTranscriptionStream(modelId: $modelId)
                }
            `,
            variables: {
                modelId
            }
        }));

        const subscription = await this.#apolloClient.subscribe({
            query: gql`
                subscription AiTranscriptionText($streamId: String!) {
                    aiTranscriptionText(streamId: $streamId)
                }
            `,
            variables: {
                streamId: openTranscriptionStream.stream_id
            }
        }).subscribe({
            next(data) {
                streamCallback(data.data.transcriptionText);

                return data.data.transcriptionText;
            },
            error(err) {
                console.error(err);
            }
        });

        this.#transcriptionSubscriptions.set(openTranscriptionStream.stream_id, () => subscription);

        return openTranscriptionStream.stream_id;
    }

    async closeTranscriptionStream(streamId: string): Promise<void> {
        const { closeTranscriptionStream } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiCloseTranscriptionStream($streamId: String!) {
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

        return closeTranscriptionStream;
    }

    async feedTranscriptionStream(streamId: string, audio: Float32Array): Promise<void> {
        const { feedTranscriptionStream } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation AiFeedTranscriptionStream($streamId: String!, $audio: [Float!]) {
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