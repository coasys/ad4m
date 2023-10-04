import * as path from "https://deno.land/std@0.203.0/path/mod.ts";
import * as fs from "https://deno.land/std@0.203.0/fs/mod.ts";
import AgentService from "./agent/AgentService";

import { EntanglementProof } from '@perspect3vism/ad4m';

export default class EntanglementProofController {
    #entanglementProofsFile: string
    #agentService: AgentService

    constructor(rootConfigPath: string, agentService: AgentService) {
        this.#entanglementProofsFile = path.join(rootConfigPath, "entanglementProofs.json")
        this.#agentService = agentService
    }

    signDeviceKey(deviceKey: string, deviceKeyType: string): EntanglementProof {
        return new EntanglementProof(this.#agentService.did!, this.#agentService.signingKeyId!, deviceKeyType, deviceKey, this.#agentService.signString(deviceKey))
    }

    generateHolochainProof(holochainPubKey: string, signedDid: string): EntanglementProof {
        return new EntanglementProof(this.#agentService.did!, this.#agentService.signingKeyId!, "holochain", holochainPubKey, this.#agentService.signString(holochainPubKey), signedDid)
    }

    addEntanglementProofs(proofs: EntanglementProof[]): void {
        let entanglementProofs: EntanglementProof[];
        if (fs.existsSync(this.#entanglementProofsFile)) {
            const decoder = new TextDecoder("utf-8");
            const data = decoder.decode(Deno.readFileSync(this.#entanglementProofsFile));
            entanglementProofs = Array.from(JSON.parse(data));
            entanglementProofs = entanglementProofs.concat(proofs);
            entanglementProofs = Array.from(new Set(entanglementProofs));
        } else {
            entanglementProofs = proofs 
        }
        const encoder = new TextEncoder();
        const data = encoder.encode(JSON.stringify(entanglementProofs));
        Deno.writeFileSync(this.#entanglementProofsFile, data)
    }

    deleteEntanglementProofs(proofs: EntanglementProof[]): void {
        if (fs.existsSync(this.#entanglementProofsFile)) {
            const decoder = new TextDecoder("utf-8");
            const data = decoder.decode(Deno.readFileSync(this.#entanglementProofsFile));
            let entanglementProofs = Array.from(JSON.parse(data));
            for (const agent of proofs) {
                entanglementProofs.splice(entanglementProofs.findIndex((value) => value == agent), 1);
            }
            const encoder = new TextEncoder();
            const entanglementProofsEncoded = encoder.encode(JSON.stringify(entanglementProofs));
            Deno.writeFileSync(this.#entanglementProofsFile, entanglementProofsEncoded)
        }
    }

    getEntanglementProofs(): EntanglementProof[] {
        if (fs.existsSync(this.#entanglementProofsFile)) {
            const decoder = new TextDecoder("utf-8");
            const data = decoder.decode(Deno.readFileSync(this.#entanglementProofsFile));
            let entanglementProofs: EntanglementProof[] = Array.from(JSON.parse(data));
            return entanglementProofs
        } else {
            return [] 
        }
    }
}