/**
 * Standalone types for the AD4M Language Development Kit.
 *
 * These mirror the canonical types in `@coasys/ad4m` but are kept
 * separate so that the LDK has no runtime dependency on the executor
 * package — Languages built against the LDK can ship as standalone
 * bundles.
 */

export type DID = string;
export type Address = string;

export interface Expression<T = unknown> {
    author: DID;
    timestamp: string;
    data: T;
    proof: ExpressionProof;
}

export interface ExpressionProof {
    signature: string;
    key: string;
    valid?: boolean;
    invalid?: boolean;
}

export interface Link {
    source: string;
    target: string;
    predicate?: string;
}

export interface LinkExpression extends Expression<Link> {
    status?: string;
}

export interface PerspectiveDiff {
    additions: LinkExpression[];
    removals: LinkExpression[];
}

export interface Perspective {
    links: LinkExpression[];
}

/**
 * Coalesced query request — discriminated by `kind`. The set of kinds
 * a Language supports is reported via `perspective-query.supportedKinds`.
 */
export interface QueryRequest {
    kind: string;
    payload: unknown;
}

export interface QueryResponse {
    kind: string;
    payload: unknown;
}

export interface DnaSpec {
    nick: string;
    bundle?: Uint8Array;
    path?: string;
    network_seed?: string;
    source?: { type: "path" | "bundle" | "bytes"; value: string | Uint8Array };
}

export interface AppInfo {
    [key: string]: unknown;
}

export interface ZomeCall {
    dnaNick: string;
    zomeName: string;
    fnName: string;
    params: unknown;
}
