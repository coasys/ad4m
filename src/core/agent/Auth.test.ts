import { AGENT_CREATE_CAPABILITY, AGENT_READ_CAPABILITY, ALL_CAPABILITY, AGENT_AUTH_CAPABILITY, checkCapability, genRequestKey, genRandomDigits } from "./Auth"

describe('capability constant', () => {
    it('ALL_CAPABILITY is expected', () => {
        expect(ALL_CAPABILITY.with.domain).toEqual("*")
        expect(ALL_CAPABILITY.with.pointers).toEqual(["*"])
        expect(ALL_CAPABILITY.can).toEqual(["*"])
    })

    it('AGENT_AUTH_CAPABILITY is expected', () => {
        expect(AGENT_AUTH_CAPABILITY.with.domain).toEqual("agent")
        expect(AGENT_AUTH_CAPABILITY.with.pointers).toEqual(["*"])
        expect(AGENT_AUTH_CAPABILITY.can).toEqual(["AUTHENTICATE"])
    })

    it('AGENT_READ_CAPABILITY is expected', () => {
        expect(AGENT_READ_CAPABILITY.with.domain).toEqual("agent")
        expect(AGENT_READ_CAPABILITY.with.pointers).toEqual(["*"])
        expect(AGENT_READ_CAPABILITY.can).toEqual(["READ"])
    })

    it('AGENT_CREATE_CAPABILITY is expected', () => {
        expect(AGENT_CREATE_CAPABILITY.with.domain).toEqual("agent")
        expect(AGENT_CREATE_CAPABILITY.with.pointers).toEqual(["*"])
        expect(AGENT_CREATE_CAPABILITY.can).toEqual(["CREATE"])
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
            checkCapability([ALL_CAPABILITY], AGENT_READ_CAPABILITY)
        }
        expect(call).not.toThrow();
    })

    it('agent with ALL_CAPABILITY can mutate the agent', () => {
        const call = () => {
            checkCapability([ALL_CAPABILITY], AGENT_CREATE_CAPABILITY)
        }
        expect(call).not.toThrow();
    })

    it('agent with AGENT_AUTH_CAPABILITY can not request the agent status', () => {
        const call = () => {
            checkCapability([AGENT_AUTH_CAPABILITY], AGENT_READ_CAPABILITY)
        }
        expect(call).toThrowError("Capability is not matched");
    })

    it('agent with AGENT_AUTH_CAPABILITY can not mutate the agent', () => {
        const call = () => {
            checkCapability([AGENT_AUTH_CAPABILITY], AGENT_CREATE_CAPABILITY)
        }
        expect(call).toThrow();
    })

    it('agent with AGENT_AUTH_CAPABILITY can request an auth', () => {
        const call = () => {
            checkCapability([AGENT_AUTH_CAPABILITY], AGENT_AUTH_CAPABILITY)
        }
        expect(call).not.toThrow();
    })

    it('agent with AGENT_READ_CAPABILITY can request the agent status', () => {
        const call = () => {
            checkCapability([AGENT_READ_CAPABILITY], AGENT_READ_CAPABILITY)
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
