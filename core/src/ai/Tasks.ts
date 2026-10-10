export class AIPromptExamplesInput {
    input: string;
    output: string;

    constructor(input: string, output: string) {
        this.input = input;
        this.output = output;
    }
}
export class AIPromptExamples {
    input: string;
    output: string;

    constructor(input: string, output: string) {
        this.input = input;
        this.output = output;
    }
}
export class AITaskInput {
    name: string;
    modelId: string;
    systemPrompt: string;
    promptExamples: AIPromptExamplesInput[];
    metaData: string;

    constructor(name: string, model_id: string, system_prompt: string, prompt_examples: AIPromptExamplesInput[], metaData?: string) {
        this.name = name;
        this.modelId = model_id;
        this.systemPrompt = system_prompt;
        this.promptExamples = prompt_examples;
        this.metaData = metaData;
    }
}
export class AITask {
    name: string;
    modelId: string;
    taskId: string;
    systemPrompt: string;
    promptExamples: AIPromptExamples[];
    metaData?: string;
    createdAt: string;
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
export class AIModelLoadingStatus {
    model: string;
    status: string;
    progress: number;
    downloaded: boolean;
    loaded : boolean;

    constructor(model: string, status: string, progress: number, downloaded: boolean, loaded: boolean) {
        this.model = model;
        this.status = status;
        this.progress = progress;
        this.downloaded = downloaded;
        this.loaded = loaded;
    }
}