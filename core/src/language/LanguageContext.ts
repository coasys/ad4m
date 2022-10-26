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
    signatures: SignaturesService;
    storageDirectory: string;
    customSettings: object;
    Holochain: HolochainLanguageDelegate | undefined;
    ad4mSignal: Ad4mSignalCB;
}

export class Dna {
    file: Buffer
    nick: string
}

export interface HolochainLanguageDelegate {
    registerDNAs(dnas: Dna[], holochainSignalCallback?: AppSignalCb);
    call(dnaNick: string, zomeName: string, fnName: string, params: object|string): Promise<any>;
}

export type Ad4mSignalCB = (signal: any) => void
