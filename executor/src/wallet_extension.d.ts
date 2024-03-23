export interface Key {
    publicKey: string;
    privateKey: string;
    encoding: string;
}

export interface Document {
    "@context": string;
    id: string;
    assertionMethod?: string[];
    authentication?: string[];
    capabilityDelegation?: string[];
    capabilityInvocation?: string[];
    keyAgreement?: string[];
    verificationMethod: VerificationMethod[];
}

interface VerificationMethod {
    id: string;
    type: string;
    controller: string;
    publicKeyBase58?: string;
    publicKeyMultibase?: Uint8Array;
    publicKeyJwk?: KeyFormat;
    privateKeyBase58?: string;
    privateKeyMultibase?: Uint8Array;
    privateKeyJwk?: KeyFormat;
}

type KeyFormat = Base58 | Multibase | JWK;

interface Base58 {
    type: "Base58";
    value: string;
}

interface Multibase {
    type: "Multibase";
    value: Uint8Array;
}

interface JWK {
    kid?: string;
    kty: string;
    crv: string;
    x?: string;
    y?: string;
    d?: string;
}

interface JWSHeader {
    alg: string;
    kid?: string;
}

interface JWS {
    header: JWSHeader;
    payload: Uint8Array;
    signature: Uint8Array;
}

declare global {
    interface RustWallet {
        getMainKey: () => Key;
        getMainKeyDocument: () => Document;
        createMainKey: () => void;
        isUnlocked: () => boolean;
        unlock: (password: String) => void;
        lock: (password: String) => void;
        export: () => String;
        load: (data: String) => void;
        sign: (payload: Uint8Array) => Uint8Array;
    }

    const WALLET: RustWallet;
}