import { Field, InputType, ObjectType } from "type-graphql";

@InputType()
export class AIPromptExamplesInput {
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
export class AIPromptExamples {
    @Field()
    input: string;

    @Field()
    output: string;

    constructor(input: string, output: string) {
        this.input = input;
        this.output = output;
    }
}

@InputType()
export class AITaskInput {
    @Field()
    modelId: string;

    @Field()
    systemPrompt: string;

    @Field(type => [AIPromptExamplesInput])
    promptExamples: AIPromptExamplesInput[];

    constructor(model_id: string, system_prompt: string, prompt_examples: AIPromptExamplesInput[]) {
        this.modelId = model_id;
        this.systemPrompt = system_prompt;
        this.promptExamples = prompt_examples;
    }
}

@ObjectType()
export class AITask {
    @Field()
    modelId: string;

    @Field()
    taskId: string;

    @Field()
    systemPrompt: string;

    @Field(type => [AIPromptExamples])
    promptExamples: AIPromptExamples[];

    constructor(model_id: string, task_id: string, system_prompt: string, prompt_examples: AIPromptExamples[]) {
        this.modelId = model_id;
        this.taskId = task_id;
        this.systemPrompt = system_prompt;
        this.promptExamples = prompt_examples;
    }
}
