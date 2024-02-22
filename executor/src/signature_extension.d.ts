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
        verifyStringSignedByDid: (did: string, data: string, signedData: string) => boolean;
        verify: (expr: Expression) => boolean;
    }

    const SIGNATURE: RustSignature;
}