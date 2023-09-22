import path from "path";
import fs from "fs";
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
            entanglementProofs = Array.from(JSON.parse(fs.readFileSync(this.#entanglementProofsFile).toString()));
            entanglementProofs = entanglementProofs.concat(proofs);
            entanglementProofs = Array.from(new Set(entanglementProofs));
        } else {
            entanglementProofs = proofs 
        }

        fs.writeFileSync(this.#entanglementProofsFile, JSON.stringify(entanglementProofs))
    }

    deleteEntanglementProofs(proofs: EntanglementProof[]): void {
        if (fs.existsSync(this.#entanglementProofsFile)) {
            let entanglementProofs = Array.from(JSON.parse(fs.readFileSync(this.#entanglementProofsFile).toString()));
            for (const agent of proofs) {
                entanglementProofs.splice(entanglementProofs.findIndex((value) => value == agent), 1);
            }
            fs.writeFileSync(this.#entanglementProofsFile, JSON.stringify(entanglementProofs))
        }
    }

    getEntanglementProofs(): EntanglementProof[] {
        if (fs.existsSync(this.#entanglementProofsFile)) {
            let entanglementProofs: EntanglementProof[] = Array.from(JSON.parse(fs.readFileSync(this.#entanglementProofsFile).toString()));
            return entanglementProofs
        } else {
            return [] 
        }
    }
}