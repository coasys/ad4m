import { ApolloClient, gql } from "@apollo/client";
import unwrapApolloResult from "../unwrapApolloResult";
import { Task } from "./Tasks";

export class AIClient {
    #apolloClient: ApolloClient<any>;

    constructor(apolloClient: ApolloClient<any>) {
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
}