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
    name: string;

    @Field()
    modelId: string;

    @Field()
    systemPrompt: string;

    @Field(type => [AIPromptExamplesInput])
    promptExamples: AIPromptExamplesInput[];

    @Field(type => String, { nullable: true })
    metadata: string;

    constructor(name: string, model_id: string, system_prompt: string, prompt_examples: AIPromptExamplesInput[], metadata?: string) {
        this.name = name;
        this.modelId = model_id;
        this.systemPrompt = system_prompt;
        this.promptExamples = prompt_examples;
        this.metadata = metadata;
    }
}

@ObjectType()
export class AITask {
    @Field()
    name: string;

    @Field()
    modelId: string;

    @Field()
    taskId: string;

    @Field()
    systemPrompt: string;

    @Field(type => [AIPromptExamples])
    promptExamples: AIPromptExamples[];

    @Field(type => String, { nullable: true })
    metadata?: string;

    @Field(type => Date)
    createdAt: Date;

    @Field(type => Date)
    updatedAt: Date;

    constructor(name: string, model_id: string, task_id: string, system_prompt: string, prompt_examples: AIPromptExamples[], metaData?: string, created_at?: Date, updated_at?: Date) {
        this.name = name;
        this.modelId = model_id;
        this.taskId = task_id;
        this.systemPrompt = system_prompt;
        this.promptExamples = prompt_examples;
        this.metadata = metaData;
        this.createdAt = created_at;
        this.updatedAt = updated_at;
    }
}
