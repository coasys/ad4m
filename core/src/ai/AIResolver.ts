import { Query, Resolver, Mutation, Arg, InputType, Field, Subscription, Float} from "type-graphql";
import { PromptExamplesInput, PromptOutput, Task } from "./Tasks";
import pako from "pako";
import base64js from "base64js";
import { AI_TRANSCRIPTION_TEXT_TOPIC } from "../PubSub";

@InputType()
export class TaskInput {
    @Field()
    modelId: string

    @Field()
    systemPrompt: string

    @Field(type => [PromptExamplesInput])
    promptExamples: PromptExamplesInput[]
}


@Resolver()
export default class AIResolver {
    @Query(() => [Task])
    tasks(): Task[] {
        return []
    }

    @Mutation(() => Task)
    aiAddTask(
        @Arg("task") task: TaskInput,
    ): Task {
        return new Task(
            task.modelId,
            "task_id",
            task.systemPrompt,
            task.promptExamples
        )
    }

    @Mutation(() => Task)
    aiRemoveTask(
        @Arg("task_id") task_id: string
    ): Task {
        return new Task(
            "modelId",
            task_id,
            "systemPrompt",
            []
        )
    }

    @Mutation(() => Task)
    aiUpdateTask(
        @Arg("task_id") task_id: string,
        @Arg("task") task: TaskInput,
    ): Task {
        return new Task(
            task.modelId,
            task_id,
            task.systemPrompt,
            task.promptExamples
        )
    }

    @Mutation(() => Task)
    aiPrompt(
        @Arg("task_id") task_id: string,
        @Arg("prompt") input: string
    ): PromptOutput {
        return new PromptOutput(
            "output"
        )
    }

    @Mutation(() => String)
    aiEmbed(
        @Arg("model_id") model_id: string,
        @Arg("text") text: string
    ): [number] {
        const vec = [0, 10, 20, 30];
        const vecString = JSON.stringify(vec);
        const compressed = pako.deflate(vecString);
        const compressedString = base64js.fromByteArray(compressed);

        const vecString1 = base64js.toByteArray(compressedString);
        const decompressed = pako.inflate(vecString1);

        return decompressed;
    }

    @Mutation(() => String)
    aiOpenTranscriptionStream(
        @Arg("model_id") model_id: string
    ): string {
        return "stream_id"
    }

    @Mutation(() => String)
    aiCloseTranscriptionStream(
        @Arg("stream_id") stream_id: string
    ): boolean {
        return true
    }

    @Mutation(() => String)
    aiFeedTranscriptionStream(
        @Arg("stream_id") stream_id: string,
        @Arg("audio", () => [Float]) audio: number[]
    ): boolean {
        return true
    }

    @Subscription({ topics: AI_TRANSCRIPTION_TEXT_TOPIC, nullable: false })
    aiTranscriptionText(
        @Arg("stream_id") stream_id: string
    ): string {
        return "transcription"
    }
}