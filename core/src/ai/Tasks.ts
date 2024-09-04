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
    model_id: string;

    @Field()
    task_id: string;

    @Field()
    system_prompt: string;

    @Field(type => [PromptExamples])
    prompt_examples: PromptExamples[];

    constructor(model_id: string, task_id: string, system_prompt: string, prompt_examples: PromptExamples[]) {
        this.model_id = model_id;
        this.task_id = task_id;
        this.system_prompt = system_prompt;
        this.prompt_examples = prompt_examples;
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