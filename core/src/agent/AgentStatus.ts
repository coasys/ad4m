export class AgentStatus {
    isInitialized: Boolean
    isUnlocked: Boolean
    did?: string
    didDocument?: string
    error?: string
    /** The `did:scid` the agent's device key speaks for; `did` stays the device `did:key`. */
    identity?: string | null

    constructor(obj?: object) {
        if(obj) {
            //@ts-ignore
            this.isInitialized = obj.isInitialized
            if(!this.isInitialized) {
                this.isInitialized = false
            }
            //@ts-ignore
            this.isUnlocked = obj.isUnlocked
            if(!this.isUnlocked) {
                this.isUnlocked = false
            }
            //@ts-ignore
            this.did = obj.did
            //@ts-ignore
            this.didDocument = obj.didDocument
            //@ts-ignore
            this.error = obj.error
            //@ts-ignore
            this.identity = obj.identity ?? null
        } else {
            this.isInitialized = false
            this.isUnlocked = false
        }
        
    }
}