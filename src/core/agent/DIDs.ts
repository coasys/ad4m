import low from 'lowdb'
import FileSync from 'lowdb/adapters/FileSync'
import path from 'path'
import fetch from 'node-fetch'
import { resolver } from '@transmute/did-key.js';

export class DIDResolver {
    #cacheDB: any

    constructor(dbAdapter) {
        this.#cacheDB = low(dbAdapter)
    }

    async resolve(did: string): Promise<object> {
        if(!did) return null

        if(this.#cacheDB.has(did).value()) {
            return this.#cacheDB.get(did).value()
        } 
        
        try {
            const resolved = await resolver.resolve(did)
            if(resolved) {
                return resolved
            }
        } catch(e){}
        
        console.debug("Downloading document for DID:", did)
        try {
            const response = await fetch(`https://resolver.identity.foundation/1.0/identifiers/${did}`)
            const didDocument = await response.json()

            if(didDocument) {
                this.#cacheDB.set(did, didDocument).write()
            } else {
                throw new Error("Empty JSON response")
            }

            return didDocument
        } catch(e) {
            console.error("Could not resolve DID:", did)
            console.error("Error:", e)
        }

    }
}

export function init(cacheDBFilePath) {
    const adapter = new FileSync(path.join(cacheDBFilePath, 'DIDCache.json'))
    return new DIDResolver(adapter)
}