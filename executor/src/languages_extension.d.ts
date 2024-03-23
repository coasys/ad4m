export interface LinkExpression {
    author: string;
    timestamp: string;
    data: Triple;
    proof: ExpressionProof;
}

export interface Triple {
    source?: string;
    target?: string;
    predicate?: string;
}

export interface ExpressionProof {
    key: string;
    signature: string;
}

export interface PerspectiveDiff {
    additions: LinkExpression[];
    removals: LinkExpression[];
}

export interface PerspectiveExpression {
    author: string;
    timestamp: string;
    data: Perspective;
    proof: ExpressionProof;
}

export interface Perspective {
    links: LinkExpression[];
}

// PerspectiveState is an enum in Rust, which can be represented as a union type in TypeScript
export type PerspectiveState = 'PRIVATE' | 'NEIGHBOURHOOD_JOIN_INITIATED' | 'LINK_LANGUAGE_FAILED_TO_INSTALL' | 'LINK_LANGUAGE_INSTALLED_BUT_NOT_SYNCED' | 'SYNCED';

declare global {
    interface RustLanguages {
        perspectiveDiffReceived: (diff: PerspectiveDiff, languageAddress: string) => void;
        syncStateChanged: (state: PerspectiveState, languageAddress: string) => void;
        telepresenceSignalReceived: (signal: PerspectiveExpression, languageAddress: string) => void;
    }

    const LANGUAGES: RustLanguages;
}