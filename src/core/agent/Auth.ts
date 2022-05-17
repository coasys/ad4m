import * as lodash from "lodash"

export type Capabilities = Capability[]
export interface Capability {
    with: Resource,
    can: string[],
}

interface Resource {
    domain: string,
    pointers: string[],
}

const WILD_CARD = "*"

export const ALL_CAPABILITY: Capability = {
    with: {
        domain: WILD_CARD,
        pointers: [WILD_CARD],
    },
    can: [WILD_CARD],
}
export const AUTH_CAPABILITY: Capability = {
    with: {
        domain: "agent",
        pointers: [WILD_CARD],
    },
    can: ["AUTHENTICATE"]
}
export const AGENT_QUERY_CAPABILITY: Capability = {
    with: {
        domain: "agent",
        pointers: [WILD_CARD],
    },
    can: ["QUERY"]
}
export const AGENT_MUTATION_CAPABILITY: Capability = {
    with: {
        domain: "agent",
        pointers: [WILD_CARD],
    },
    can: ["MUTATION"]
}

export const checkCapability = (capabilities: Capabilities, expected: Capability) => {
    const customCapMatch = (cap: Capability, expected: Capability) => {
        if (cap.with.domain !== WILD_CARD && cap.with.domain !== expected.with.domain) {
            return false;
        }

        if (!lodash.isEqual(cap.with.pointers, [WILD_CARD]) && lodash.difference(expected.with.pointers, cap.with.pointers).length > 0 ) {
            return false;
        }

        if (!lodash.isEqual(cap.can, [WILD_CARD]) && lodash.difference(expected.can, cap.can).length > 0) {
            return false;
        }

        return true
    }

    if(!lodash.find(capabilities, cap => lodash.isEqualWith(cap, expected, customCapMatch))) {
        throw Error("Capability is not matched")
    }
}

export const DefaultTokenValidPeriod = 7 * 24 * 60 * 60; // 7 days in seconds

export interface AuthInfoExtended {
    requestId: string,
    auth: AuthInfo, 
}
export interface AuthInfo {
    appName: string,
    appDesc: string,
    appUrl: string,
    capabilities?: Capability[], 
}

export const genAuthRand = () => {
    return Math.floor(100000 + Math.random() * 900000).toString()
}

export const genAuthKey = (requestId: string, rand: string) => {
    return `${requestId}-${rand}`
}