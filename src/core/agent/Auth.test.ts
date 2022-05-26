import { AGENT_MUTATION_CAPABILITY, AGENT_QUERY_CAPABILITY, ALL_CAPABILITY, AUTH_CAPABILITY, checkCapability, genRequestKey, genRandomDigits } from "./Auth"

describe('capability constant', () => {
    it('ALL_CAPABILITY is expected', () => {
        expect(ALL_CAPABILITY.with.domain).toEqual("*")
        expect(ALL_CAPABILITY.with.pointers).toEqual(["*"])
        expect(ALL_CAPABILITY.can).toEqual(["*"])
    })

    it('AUTH_CAPABILITY is expected', () => {
        expect(AUTH_CAPABILITY.with.domain).toEqual("agent")
        expect(AUTH_CAPABILITY.with.pointers).toEqual(["*"])
        expect(AUTH_CAPABILITY.can).toEqual(["AUTHENTICATE"])
    })

    it('AGENT_QUERY_CAPABILITY is expected', () => {
        expect(AGENT_QUERY_CAPABILITY.with.domain).toEqual("agent")
        expect(AGENT_QUERY_CAPABILITY.with.pointers).toEqual(["*"])
        expect(AGENT_QUERY_CAPABILITY.can).toEqual(["QUERY"])
    })

    it('AGENT_MUTATION_CAPABILITY is expected', () => {
        expect(AGENT_MUTATION_CAPABILITY.with.domain).toEqual("agent")
        expect(AGENT_MUTATION_CAPABILITY.with.pointers).toEqual(["*"])
        expect(AGENT_MUTATION_CAPABILITY.can).toEqual(["MUTATION"])
    })
})

describe('checkCapability', () => {
    it('agent with ALL_CAPABILITY can permit an auth request', () => {
        const call = () => {
            checkCapability([ALL_CAPABILITY], ALL_CAPABILITY)
        }
        expect(call).not.toThrow();
    })

    it('agent with ALL_CAPABILITY can request agent status', () => {
        const call = () => {
            checkCapability([ALL_CAPABILITY], AGENT_QUERY_CAPABILITY)
        }
        expect(call).not.toThrow();
    })

    it('agent with ALL_CAPABILITY can mutate the agent', () => {
        const call = () => {
            checkCapability([ALL_CAPABILITY], AGENT_MUTATION_CAPABILITY)
        }
        expect(call).not.toThrow();
    })

    it('agent with AUTH_CAPABILITY can not request the agent status', () => {
        const call = () => {
            checkCapability([AUTH_CAPABILITY], AGENT_QUERY_CAPABILITY)
        }
        expect(call).toThrow(Error("Capability is not matched"));
    })

    it('agent with AUTH_CAPABILITY can not mutate the agent', () => {
        const call = () => {
            checkCapability([AUTH_CAPABILITY], AGENT_MUTATION_CAPABILITY)
        }
        expect(call).toThrow();
    })

    it('agent with AUTH_CAPABILITY can request an auth', () => {
        const call = () => {
            checkCapability([AUTH_CAPABILITY], AUTH_CAPABILITY)
        }
        expect(call).not.toThrow();
    })

    it('agent with AGENT_QUERY_CAPABILITY can request the agent status', () => {
        const call = () => {
            checkCapability([AGENT_QUERY_CAPABILITY], AGENT_QUERY_CAPABILITY)
        }
        expect(call).not.toThrow();
    })
})

describe('genRandomDigits', () => {
    it('should return a 6-digit string', () => {
        let rand = genRandomDigits()
        expect(rand).toMatch(/^[0-9]{6}$/)
    })
})

describe('genRequestKey', () => {
    it('should join the requestId and rand', () => {
        let key = genRequestKey("my-request-id", "123456")
        expect(key).toBe("my-request-id-123456")
    })
})
