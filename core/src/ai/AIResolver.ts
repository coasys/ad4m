import { Query, Resolver, Mutation, Arg, InputType, Field } from "type-graphql";
import { PromptExamplesInput, PromptOutput, Task } from "./Tasks";

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
    addTask(
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
    removeTask(
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
    updateTask(
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
    prompt(
        @Arg("task_id") task_id: string,
        @Arg("prompt") input: string
    ): PromptOutput {
        return new PromptOutput(
            "output"
        )
    }
}