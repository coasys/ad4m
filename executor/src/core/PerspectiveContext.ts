import type AgentService from "./agent/AgentService.ts"
import { MainConfig } from "./Config.ts"
import type LanguageController from "./LanguageController.ts"

export default class PerspectiveContext {
    db: any
    agentService?: AgentService
    languageController?: LanguageController
    config?: MainConfig
}