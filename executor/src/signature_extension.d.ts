export interface SignatureVerificationResult {
    isValid: boolean;
}

export interface Expression {
    author: string;
    timestamp: string;
    data: any;
    proof: ExpressionProof;
}

export interface ExpressionProof {
    signature: string;
    key: string;
}

declare global {
    interface RustSignature {
        verifyStringSignedByDid: (did: string, data: string, signedData: string) => SignatureVerificationResult;
        verify: (expr: Expression) => SignatureVerificationResult;
    }

    const SIGNATURE: RustSignature;
}