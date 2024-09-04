import { Field, InputType, ObjectType } from "type-graphql";

@InputType()
export class PromptExamplesInput {
    @Field()
    input: string;

    @Field()
    output: string;

    constructor(input: string, output: string) {
        this.input = input;
        this.output = output;
    }
}


@ObjectType()
export class PromptExamples {
    @Field()
    input: string;

    @Field()
    output: string;

    constructor(input: string, output: string) {
        this.input = input;
        this.output = output;
    }
}

@ObjectType()
export class Task {
    @Field()
    modelId: string;

    @Field()
    taskId: string;

    @Field()
    systemPrompt: string;

    @Field(type => [PromptExamples])
    promptExamples: PromptExamples[];

    constructor(model_id: string, task_id: string, system_prompt: string, prompt_examples: PromptExamples[]) {
        this.modelId = model_id;
        this.taskId = task_id;
        this.systemPrompt = system_prompt;
        this.promptExamples = prompt_examples;
    }
}

@ObjectType()
export class PromptOutput {
    @Field()
    result: string;

    constructor(result: string) {
        this.result = result;
    }
}