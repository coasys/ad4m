import type { Expression } from "@coasys/ad4m"
import sha256 from 'sha256'
import stringify from 'json-stable-stringify'
export default class Signatures {
    constructor() {}

    async verifyStringSignedByDid(did: string, didSigningKeyId: string, data: string, signedData: string): Promise<boolean> {
        const sigBytes = Uint8Array.from(Buffer.from(signedData, "hex"))
        const message = Signatures.buildMessageRaw(data)
        return WALLET.verify(did, message, sigBytes)
    }

    async verify(expr: Expression): Promise<boolean> {
        const sigBytes = Uint8Array.from(Buffer.from(expr.proof.signature, "hex"))
        const message = Signatures.buildMessage(expr.data, expr.timestamp)
        return WALLET.verify(expr.author, message, sigBytes)
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

    static async tagExpressionSignatureStatus(expression: Expression) {
        let signatures = new Signatures()
        if(!await signatures.verify(expression)) {
            let expressionString = JSON.stringify(expression);
            let endingLog = expressionString.length > 50 ? "... \x1b[0m" : "\x1b[0m";
            console.error(new Date().toISOString(),"tagExpressionSignatureStatus - BROKEN SIGNATURE FOR EXPRESSION: (object):", expressionString.substring(0, 50), endingLog)
            expression.proof.invalid = true
            expression.proof.valid = false
        } else {
            expression.proof.valid = true
            expression.proof.invalid = false
        }
    }
}

