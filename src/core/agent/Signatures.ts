import type { Expression } from "@perspect3vism/ad4m"
import type { DIDResolver } from ".//DIDs"
import sha256 from 'sha256'
import secp256k1 from 'secp256k1'
import baseX from 'base-x'
import stringify from 'json-stable-stringify'
const BASE58 = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
const bs58 = baseX(BASE58)
export default class Signatures {
    #didResolver: DIDResolver

    constructor(didResolver: DIDResolver) {
        this.#didResolver = didResolver
    }

    async verify(expr: Expression): Promise<boolean> {
        // @ts-ignore
        const { didDocument } = await this.#didResolver.resolve(expr.author)
        if(!didDocument) {
            console.debug("DID document not found for", expr.author)
            return false
        }


        const availableKeys = didDocument.publicKey ? didDocument.publicKey : didDocument.verificationMethod
        //@ts-ignore
        const key = availableKeys.find(k => k.id === expr.proof.key)
        if(!key) {
            console.debug("Key not found in DID document", expr.proof.key, didDocument)
            return false
        }

        let pubKey: Uint8Array | undefined
        if(key.publicKeyHex)
            pubKey = Uint8Array.from(Buffer.from(key.publicKeyHex, "hex"))
        if(key.publicKeyBase58)
            pubKey = Uint8Array.from(bs58.decode(key.publicKeyBase58))
        const sigBytes = Uint8Array.from(Buffer.from(expr.proof.signature, "hex"))
        const message = Signatures.buildMessage(expr.data, expr.timestamp)

        if (!pubKey) {
            throw Error("Could not find publicKeyHex or publicKeyBase58 in did document")
        }
        return secp256k1.ecdsaVerify(sigBytes, message, pubKey)
    }

    static buildMessage(data: any, timestamp: string): Uint8Array {
        const payload = { data, timestamp }
        const payloadString = stringify(payload)
        const payloadBuffer = Buffer.from(payloadString)
        const payloadBytes = Uint8Array.from(sha256(Buffer.from(payloadBuffer), { asBytes: true }))
        return payloadBytes
    }
    
    static buildMessageRaw(data: any): Uint8Array {
        const payload = { data }
        const payloadString = stringify(payload)
        const payloadBuffer = Buffer.from(payloadString)
        const payloadBytes = Uint8Array.from(sha256(Buffer.from(payloadBuffer), { asBytes: true }))
        return payloadBytes
    }
}
