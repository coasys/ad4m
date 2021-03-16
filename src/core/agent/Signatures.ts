import type Expression from "../../ad4m/Expression"
import type { DIDResolver } from ".//DIDs"
import sha256 from 'sha256'
import secp256k1 from 'secp256k1'
import baseX from 'base-x'
const BASE58 = '123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz'
const bs58 = baseX(BASE58)
export default class Signatures {
    #didResolver: DIDResolver

    constructor(didResolver: DIDResolver) {
        this.#didResolver = didResolver
    }

    async verify(expr: Expression): Promise<boolean> {
        // @ts-ignore
        const { didDocument } = await this.#didResolver.resolve(expr.author.did)
        if(!didDocument) return false


        const availableKeys = didDocument.publicKey ? didDocument.publicKey : didDocument.verificationMethod
        const key = availableKeys.find(k => k.id === expr.proof.key)
        if(!key) return false

        let pubKey
        if(key.publicKeyHex)
            pubKey = Uint8Array.from(Buffer.from(key.publicKeyHex, "hex"))
        if(key.publicKeyBase58)
            pubKey = Uint8Array.from(bs58.decode(key.publicKeyBase58))
        const sigBytes = Uint8Array.from(Buffer.from(expr.proof.signature, "hex"))
        const message = Signatures.buildMessage(expr.data, expr.timestamp)

        return secp256k1.ecdsaVerify(sigBytes, message, pubKey)
    }

    static buildMessage(data: any, timestamp: string): Uint8Array {
        const payload = { data, timestamp}
        const payloadString = JSON.stringify(payload)
        const payloadBuffer = Buffer.from(payloadString)
        const payloadBytes = Uint8Array.from(sha256(Uint8Array.from(payloadBuffer), { asBytes: true }))
        return payloadBytes
    }
}
