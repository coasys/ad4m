import type { ModelType } from "../generated/api/ModelType";

/** A model onboarding offers to set up, by the id its server knows it by. */
export interface RecommendedModel {
    modelType: ModelType;
    /** Ollama's name for it: `ai.pullModel` fetches it onto an Ollama endpoint. */
    ollama?: string;
    /** The id an OpenAI-shaped audio server (speaches) knows it by. */
    openAiAudio?: string;
    /** One line for the person choosing. */
    description: string;
}

/**
 * The models onboarding recommends, the one place they are named: a 4-8B instruct model
 * that calls tools, an embedding model, and a transcription model. Any model the servers
 * serve works; these are the defaults a fresh setup offers.
 */
export const AI_RECOMMENDED_MODELS: readonly RecommendedModel[] = [
    {
        modelType: "LLM",
        ollama: "qwen3:8b",
        description: "Qwen3 8B: an instruct model that calls tools, for interpretation and agents",
    },
    {
        modelType: "EMBEDDING",
        ollama: "nomic-embed-text",
        description: "nomic-embed-text: embeddings for semantic dedup and search",
    },
    {
        modelType: "TRANSCRIPTION",
        openAiAudio: "Systran/faster-whisper-small",
        description: "faster-whisper small on speaches: faster than real time on a CPU",
    },
];

/** Where to get a model server when none runs on this machine. */
export const AI_SERVER_INSTALL_LINKS = {
    ollama: "https://ollama.com/download",
    speaches: "https://speaches.ai/installation/",
} as const;
