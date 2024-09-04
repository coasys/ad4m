import { ApolloClient, gql } from "@apollo/client";
import unwrapApolloResult from "../unwrapApolloResult";
import { Task } from "./Tasks";
import base64js from 'base64js';
import pako from 'pako'

export class AIClient {
    #apolloClient: ApolloClient<any>;

    constructor(apolloClient: ApolloClient<any>, subscribe: boolean = true) {
        this.#apolloClient = apolloClient;
    }

    async tasks(): Promise<Task[]> {
        const { tasks } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`
                query {
                    tasks {
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
                mutation AddTask($modelId: String!, $systemPrompt: String!, $promptExamples: [PromptExamplesInput!]!) {
                    addTask(modelId: $modelId, systemPrompt: $systemPrompt, promptExamples: $promptExamples) {
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
                mutation RemoveTask($taskId: String!) {
                    removeTask(taskId: $taskId) {
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
                mutation UpdateTask($taskId: String!, $task: TaskInput!) {
                    updateTask(taskId: $taskId, task: $task) {
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
                mutation Prompt($taskId: String!, $prompt: String!) {
                    prompt(taskId: $taskId, prompt: $prompt) {
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
                mutation Embed($modelId: String!, $text: String!) {
                    embed(modelId: $modelId, text: $text)
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

    async openTranscriptionStream(modelId: string): Promise<string> {
        const { openTranscriptionStream } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation OpenTranscriptionStream {
                    openTranscriptionStream(modelId: $modelId)
                }
            `,
            variables: {
                modelId
            }
        }));

        return openTranscriptionStream.stream_id;
    }

    async closeTranscriptionStream(streamId: string): Promise<void> {
        const { closeTranscriptionStream } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation CloseTranscriptionStream($streamId: String!) {
                    closeTranscriptionStream(streamId: $streamId)
                }
            `,
            variables: {
                streamId
            }
        }));

        return closeTranscriptionStream;
    }

    async feedTranscriptionStream(streamId: string, audio: Float32Array): Promise<void> {
        const { feedTranscriptionStream } = unwrapApolloResult(await this.#apolloClient.mutate({
            mutation: gql`
                mutation FeedTranscriptionStream($streamId: String!, $audio: [Float!]) {
                    feedTranscriptionStream(streamId: $streamId, audio: $audio)
                }
            `,
            variables: {
                streamId,
                audio: audio
            }
        }));

        return feedTranscriptionStream;
    }

    async transcriptionText(streamId: string): Promise<string> {
        const { transcriptionText } = unwrapApolloResult(await this.#apolloClient.query({
            query: gql`
                subscription TranscriptionText($streamId: String!) {
                    transcriptionText(streamId: $streamId)
                }
            `,
            variables: {
                streamId
            }
        }));

        return transcriptionText;
    } 
}