import { PerspectiveState } from "@coasys/ad4m"
import type AgentService from "./agent/AgentService"
import { MainConfig } from "./Config"
import type LanguageController from "./LanguageController"

export default class PerspectiveContext {
    db: any
    agentService?: AgentService
    languageController?: LanguageController
    config?: MainConfig
    updateControllersHandleSyncStatus?: (uuid: string, status: PerspectiveState) => void
}