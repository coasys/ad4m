import { Query, Resolver, Mutation, Arg, InputType, Field, Subscription, Float} from "type-graphql";
import { AIPromptOutput, AITask, AITaskInput } from "./Tasks";
import pako from "pako";
import base64js from "base64js";
import { AI_TRANSCRIPTION_TEXT_TOPIC } from "../PubSub";

@Resolver()
export default class AIResolver {
    @Query(returns => [AITask])
    aiTasks(): AITask[] {
        return []
    }

    @Mutation(returns => AITask)
    aiAddTask(
        @Arg("task") task: AITaskInput,
    ): AITask {
        return new AITask(
            task.modelId,
            "task_id",
            task.systemPrompt,
            task.promptExamples
        )
    }

    @Mutation(() => AITask)
    aiRemoveTask(
        @Arg("task_id") task_id: string
    ): AITask {
        return new AITask(
            "modelId",
            task_id,
            "systemPrompt",
            []
        )
    }

    @Mutation(() => AITask)
    aiUpdateTask(
        @Arg("task_id") task_id: string,
        @Arg("task") task:AITaskInput,
    ): AITask {
        return new AITask(
            task.modelId,
            task_id,
            task.systemPrompt,
            task.promptExamples
        )
    }

    @Mutation(() => AITask)
    aiPrompt(
        @Arg("task_id") task_id: string,
        @Arg("prompt") input: string
    ): AIPromptOutput {
        return new AIPromptOutput(
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