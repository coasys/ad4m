import type Expression from "./Expression";

export default interface AgentService {
    readonly did: string
    createSignedExpression(data: any): Expression
}