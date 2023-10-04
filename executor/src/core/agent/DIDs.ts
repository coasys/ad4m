// import { resolver } from '@transmute/did-key.js';

export class DIDResolver {
    #cacheMap = new Map()

    async resolve(did: string): Promise<object | null> {
        if(!did) return null

        if(this.#cacheMap.has(did)) {
            return this.#cacheMap.get(did)
        } 
        
        try {
            // const resolved = await resolver.resolve(did)
            // if(resolved) {
            //     return resolved
            // }
        } catch(e){}
        
        console.debug("Downloading document for DID:", did)
        try {
            const response = await fetch(`https://resolver.identity.foundation/1.0/identifiers/${did}`)
            const didDocument = await response.json()

            if(didDocument) {
                this.#cacheMap.set(did, didDocument)
            } else {
                throw new Error("Empty JSON response")
            }

            return didDocument
        } catch(e) {
            console.error("Could not resolve DID:", did)
            console.error("Error:", e)
            return null
        }

    }
}

export function init(cacheDBFilePath: string) {
    //const adapter = new FileSync(path.join(cacheDBFilePath, 'DIDCache.json'))
    return new DIDResolver()
}