export const AllCapability = "*"
export const RequestAuthCapability = "RequestAuthCapability"

export const AgentQueryCapability = "AgentQueryCapability"
export const AgentMutationCapability = "AgentMutationCapability"

export const LanguageQueryCapability = "LanguageQueryCapability"
export const LanguageMutationCapability = "LanguageMutationCapability"

export const ExpressionQueryCapability = "ExpressionQueryCapability"
export const ExpressionMutationCapability = "ExpressionMutationCapability"

export const PerspectiveQueryCapability = "PerspectiveQueryCapability"
export const PerspectiveMutationCapability = "PerspectiveMutationCapability"

export const RuntimeQueryCapability = "RuntimeQueryCapability"
export const RuntimeMutationCapability = "RuntimeMutationCapability"

export const DefaultTokenValidPeriod = 7 * 24 * 60 * 60; // 7 days in seconds

export interface AuthInfoExtended {
    requestId: string,
    auth: AuthInfo, 
}
export interface AuthInfo {
    appName: string,
    appDesc: string,
    appUrl: string,
    capabilities?: string[], 
}

export const genAuthRand = () => {
    return Math.floor(100000 + Math.random() * 900000).toString()
}

export const genAuthKey = (requestId: string, rand: string) => {
    return `${requestId}-${rand}`
}