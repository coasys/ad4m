export class AgentStatus {
    isInitialized: Boolean
    isUnlocked: Boolean
    did?: string
    didDocument?: string
    error?: string

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
        } else {
            this.isInitialized = false
            this.isUnlocked = false
        }
        
    }
}