import type { AppSignalCb } from '@holochain/client'
import { Expression } from "../expression/Expression";
import type { IPFS } from 'ipfs-core-types'

export interface AgentService {
    readonly did: string
    createSignedExpression(data: any): Expression
}

export interface SignaturesService {
    verify(expr: Expression): boolean
}

export interface LanguageContext {
    agent: AgentService;
    IPFS: IPFS;
    gun: any;
    signatures: SignaturesService;
    storageDirectory: string;
    customSettings: object;
    Holochain: HolochainLanguageDelegate | undefined;
    ad4mSignal: Ad4mSignalCB;
}

export class Dna {
    file: Buffer
    nick: string
    zomeCalls: [string, string][]
}

export interface HolochainLanguageDelegate {
    /** Installs/registers a given DNA in the ad4m-executor */
    registerDNAs(dnas: Dna[], holochainSignalCallback?: AppSignalCb): Promise<void>;
    /** Makes a single call to a given holochain DNA. Underlying implementation puts these calls into a sync fifo queue */
    call(dnaNick: string, zomeName: string, fnName: string, params: object|string): Promise<any>;
    /** Makes all supplied calls in parallel to the provided holochain dna... Should only be called on read operations to avoid source chain async mutation errors */
    callAsync(calls: {dnaNick: string, zomeName: string, fnName: string, params: object|string}[], timeoutMs?: number): Promise<any[]>;
}

export type Ad4mSignalCB = (signal: any) => void
