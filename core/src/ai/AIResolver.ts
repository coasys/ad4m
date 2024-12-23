import { Query, Resolver, Mutation, Arg, InputType, Field, Subscription, Float, PubSub, ObjectType} from "type-graphql";
import { AIModelLoadingStatus, AITask, AITaskInput } from "./Tasks";
import pako from "pako";
import base64js from 'base64-js';
import { AI_TRANSCRIPTION_TEXT_TOPIC } from "../PubSub";

let createdAt = Date.now().toString();
let updatedAt = Date.now().toString();


@ObjectType()
export class ModelApi {
    @Field()
    baseUrl: string;

    @Field()
    apiKey: string;

    @Field()
    model: string;

    @Field()
    apiType: String;
}

@ObjectType()
export class LocalModel {
    @Field()
    fileName: string;

    @Field()
    tokenizerSource: string;

    @Field()
    modelParameters: string;
}

export type ModelType = "LLM" | "EMBEDDING" | "TRANSCRIPTION";

@ObjectType()
export class Model {
    @Field()
    id: string;

    @Field()
    name: string;

    @Field(type => ModelApi, { nullable: true })
    api?: ModelApi;

    @Field(type => LocalModel, { nullable: true })
    local?: LocalModel;

    @Field()
    modelType: ModelType;
}

@InputType()
export class ModelApiInput {
    @Field()
    baseUrl: string;

    @Field()
    apiKey: string;

    @Field()
    model: string;

    @Field()
    apiType: string;
}

@InputType()
export class LocalModelInput {
    @Field()
    fileName: string;

    @Field()
    tokenizerSource: string;

    @Field()
    modelParameters: string;
}

@InputType()
export class ModelInput {
    @Field()
    name: string;

    @Field(type => ModelApiInput, { nullable: true })
    api?: ModelApiInput;

    @Field(type => LocalModelInput, { nullable: true })
    local?: LocalModelInput;

    @Field()
    modelType: ModelType;
}

@Resolver()
export default class AIResolver {
    @Query(returns => [Model])
    aiGetModels(): Model[] {
        return [
            {
                id: "test-id",
                name: "Test Model",
                api: {
                    baseUrl: "https://api.example.com",
                    apiKey: "test-api-key",
                    model: "gpt4o",
                    apiType: "OpenAi"
                },
                local: {
                    fileName: "test-model.bin",
                    tokenizerSource: "test-tokenizer",
                    modelParameters: "{}"
                },
                modelType: "LLM"
            }
        ]
    }

    @Mutation(returns => String)
    aiAddModel(@Arg("model", type => ModelInput) model: ModelInput): string {
        // In a real implementation, this would add the model to storage
        return "new-model-id"
    }

    @Mutation(returns => Boolean)
    aiUpdateModel(
        @Arg("modelId", type => String) modelId: string,
        @Arg("model", type => ModelInput) model: ModelInput
    ): boolean {
        // In a real implementation, this would update the model in storage
        return true
    }

    @Mutation(returns => Boolean)
    aiRemoveModel(@Arg("modelId", type => String) modelId: string): boolean {
        // In a real implementation, this would remove the model from storage
        return true
    }

    @Mutation(returns => Boolean)
    aiSetDefaultModel(
        @Arg("modelType", type => String) modelType: ModelType,
        @Arg("modelId", type => String) modelId: string,
    ): boolean {
        // In a real implementation, this would set the default model
        return true
    }

    @Query(returns => Model)
    aiGetDefaultModel(@Arg("modelType", type => String) modelType: ModelType): Model {
        // In a real implementation, this would get the default model for the given type
        return {
            id: "default-test-id",
            name: "Default Test Model",
            api: {
                baseUrl: "https://api.example.com", 
                apiKey: "test-api-key",
                model: "gpt4o",
                apiType: "OpenAi"
            },
            local: {
                fileName: "test-model.bin",
                tokenizerSource: "test-tokenizer", 
                modelParameters: "{}"
            },
            modelType: modelType
        }
    }

    @Query(returns => [AITask])
    aiTasks(): AITask[] {
        return [new AITask(
            "task1",
            "modelId",
            "task_id",
            "systemPrompt",
            [],
            undefined,
            createdAt,
            updatedAt
        ),
        new AITask(
            "task2",
            "modelId",
            "task_id",
            "systemPrompt",
            [],
            undefined,
            createdAt,
            updatedAt
        )]
    }

    @Mutation(returns => AITask)
    aiAddTask(
        @Arg("task") task: AITaskInput,
    ): AITask {
        return new AITask(
            "task_name",
            task.modelId,
            "task_id",
            task.systemPrompt,
            task.promptExamples,
            undefined,
            createdAt,
            updatedAt
        )
    }

    @Mutation(() => AITask)
    aiRemoveTask(
        @Arg("taskId") taskId: string
    ): AITask {
        return new AITask(
            "task_name",
            "model_id",
            taskId,
            "system prompt",
            [],
            undefined,
            createdAt,
            updatedAt
        )
    }

    @Mutation(() => AITask)
    aiUpdateTask(
        @Arg("taskId") taskId: string,
        @Arg("task") task:AITaskInput,
    ): AITask {
        return new AITask(
            task.name,
            task.modelId,
            taskId,
            task.systemPrompt,
            task.promptExamples,
            undefined,
            createdAt,
            updatedAt
        )
    }

    @Query(() => AIModelLoadingStatus)
    aiModelLoadingStatus(@Arg("model") model: string): AIModelLoadingStatus {
        return new AIModelLoadingStatus(
            model,
            "loaded",
            100.0,
            true,
            true
        )
    }

    @Mutation(() => String)
    aiPrompt(
        @Arg("taskId") taskId: string,
        @Arg("prompt") input: string
    ): string {
        return "output"
    }

    @Mutation(() => String)
    aiEmbed(
        @Arg("modelId") modelId: string,
        @Arg("text") text: string
    ): string {
        const vec = [0, 10, 20, 30];
        const vecString = JSON.stringify(vec);
        const compressed = pako.deflate(vecString);
        const compressedString = base64js.fromByteArray(compressed);

        return compressedString;
    }

    @Mutation(() => String)
    aiOpenTranscriptionStream(
        @Arg("modelId") modelId: string
    ): string {
        return "streamId"
    }

    @Mutation(() => String)
    aiCloseTranscriptionStream(
        @Arg("streamId") streamId: string
    ): boolean {
        return true
    }

    @Mutation(() => String)
    aiFeedTranscriptionStream(
        @Arg("streamId") streamId: string,
        @Arg("audio", () => [Float]) audio: number[],
        @PubSub() pubSub: any
    ): boolean {
        pubSub.publish(AI_TRANSCRIPTION_TEXT_TOPIC, { streamId });
        return true
    }

    @Subscription({ topics: AI_TRANSCRIPTION_TEXT_TOPIC, nullable: false })
    aiTranscriptionText(
        @Arg("streamId") streamId: string
    ): string {
        return "transcription"
    }
}