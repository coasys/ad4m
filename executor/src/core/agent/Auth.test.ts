import { expect } from "chai";

import { checkCapability, genRequestKey, genRandomDigits } from "./Auth"
import * as Auth from "./Auth"

describe('capability constant', () => {
    it('ALL_CAPABILITY is expected', () => {
        expect(Auth.ALL_CAPABILITY.with.domain).to.be.eql("*")
        expect(Auth.ALL_CAPABILITY.with.pointers).to.be.eql(["*"])
        expect(Auth.ALL_CAPABILITY.can).to.be.eql(["*"])
    })

    it('AGENT_AUTH_CAPABILITY is expected', () => {
        expect(Auth.AGENT_AUTH_CAPABILITY.with.domain).to.be.eql("agent")
        expect(Auth.AGENT_AUTH_CAPABILITY.with.pointers).to.be.eql(["*"])
        expect(Auth.AGENT_AUTH_CAPABILITY.can).to.be.eql(["AUTHENTICATE"])
    })

    it('AGENT_READ_CAPABILITY is expected', () => {
        expect(Auth.AGENT_READ_CAPABILITY.with.domain).to.be.eql("agent")
        expect(Auth.AGENT_READ_CAPABILITY.with.pointers).to.be.eql(["*"])
        expect(Auth.AGENT_READ_CAPABILITY.can).to.be.eql(["READ"])
    })

    it('AGENT_CREATE_CAPABILITY is expected', () => {
        expect(Auth.AGENT_CREATE_CAPABILITY.with.domain).to.be.eql("agent")
        expect(Auth.AGENT_CREATE_CAPABILITY.with.pointers).to.be.eql(["*"])
        expect(Auth.AGENT_CREATE_CAPABILITY.can).to.be.eql(["CREATE"])
    })
})

describe('perspectiveQueryCapability', () => {
    it('query capability is expected', () => {
        const capability = Auth.perspectiveQueryCapability(["123", "456"])
        expect(capability.with.domain).to.be.eql("perspective")
        expect(capability.with.pointers).to.be.eql(["123", "456"])
        expect(capability.can).to.be.eql(["READ"])
    })
})

describe('checkCapability', () => {
    it('agent with ALL_CAPABILITY can permit an auth request', () => {
        const call = () => {
            checkCapability([Auth.ALL_CAPABILITY], Auth.AGENT_PERMIT_CAPABILITY)
        }
        expect(call).not.throw();
    })

    it('agent with ALL_CAPABILITY can request agent status', () => {
        const call = () => {
            checkCapability([Auth.ALL_CAPABILITY], Auth.AGENT_READ_CAPABILITY)
        }
        expect(call).not.throw();
    })

    it('agent with ALL_CAPABILITY can mutate the agent', () => {
        const call = () => {
            checkCapability([Auth.ALL_CAPABILITY], Auth.AGENT_CREATE_CAPABILITY)
        }
        expect(call).not.throw();
    })

    it('agent with AGENT_AUTH_CAPABILITY can not request the agent status', () => {
        const call = () => {
            checkCapability([Auth.AGENT_AUTH_CAPABILITY], Auth.AGENT_READ_CAPABILITY)
        }
        expect(call).to.Throw("Capability is not matched");
    })

    it('agent with AGENT_AUTH_CAPABILITY can not mutate the agent', () => {
        const call = () => {
            checkCapability([Auth.AGENT_AUTH_CAPABILITY], Auth.AGENT_CREATE_CAPABILITY)
        }
        expect(call).to.throw();
    })

    it('agent with AGENT_AUTH_CAPABILITY can request an auth', () => {
        const call = () => {
            checkCapability([Auth.AGENT_AUTH_CAPABILITY], Auth.AGENT_AUTH_CAPABILITY)
        }
        expect(call).not.throw();
    })

    it('agent with AGENT_READ_CAPABILITY can request the agent status', () => {
        const call = () => {
            checkCapability([Auth.AGENT_READ_CAPABILITY], Auth.AGENT_READ_CAPABILITY)
        }
        expect(call).not.throw();
    })

    it('agent with perspectiveQueryCapability can query a perspective', () => {
        const call = () => {
            checkCapability([Auth.perspectiveQueryCapability(["*"])], Auth.perspectiveQueryCapability(["123"]))
        }
        expect(call).not.throw();
    })
})

describe('genRandomDigits', () => {
    it('should return a 6-digit string', () => {
        let rand = genRandomDigits()
        expect(rand).to.match(/^[0-9]{6}$/)
    })
})

describe('genRequestKey', () => {
    it('should join the requestId and rand', () => {
        let key = genRequestKey("my-request-id", "123456")
        expect(key).to.be.equal("my-request-id-123456")
    })
})
