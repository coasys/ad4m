import { Field, InputType, ObjectType } from "type-graphql";
import { string } from "yargs";

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
    metaData: string;

    constructor(name: string, model_id: string, system_prompt: string, prompt_examples: AIPromptExamplesInput[], metaData?: string) {
        this.name = name;
        this.modelId = model_id;
        this.systemPrompt = system_prompt;
        this.promptExamples = prompt_examples;
        this.metaData = metaData;
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
    metaData?: string;

    @Field()
    createdAt: string;

    @Field()
    updatedAt: string;

    constructor(name: string, model_id: string, task_id: string, system_prompt: string, prompt_examples: AIPromptExamples[], metaData?: string, created_at?: string, updated_at?: string) {
        this.name = name;
        this.modelId = model_id;
        this.taskId = task_id;
        this.systemPrompt = system_prompt;
        this.promptExamples = prompt_examples;
        this.metaData = metaData;
        this.createdAt = created_at;
        this.updatedAt = updated_at;
    }
}

@ObjectType()
export class AIModelLoadingStatus {
    @Field()
    model: string;

    @Field()
    status: string;

    @Field()
    progress: number;

    @Field()
    downloaded: boolean;

    @Field()
    loaded : boolean;

    constructor(model: string, status: string, progress: number, downloaded: boolean, loaded: boolean) {
        this.model = model;
        this.status = status;
        this.progress = progress;
        this.downloaded = downloaded;
        this.loaded = loaded;
    }
}