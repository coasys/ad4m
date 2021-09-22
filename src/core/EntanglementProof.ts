import path from "path";
import fs from "fs";
import AgentService from "./agent/AgentService";

import { EntanglementProof } from '@perspect3vism/ad4m';

export default class EntanglementProofController {
    #entanglementProofs: string
    #agentService: AgentService

    constructor(rootConfigPath: string, agentService: AgentService) {
        this.#entanglementProofs = path.join(rootConfigPath, "entanglementProofs.json")
        this.#agentService = agentService
    }

    signDeviceKey(deviceKey: string): EntanglementProof {
        return new EntanglementProof(this.#agentService.did!, deviceKey, this.#agentService.signString(deviceKey))
    }

    generateHolochainProof(holochainPubKey: string, signedDid: string): EntanglementProof {
        return new EntanglementProof(this.#agentService.did!, holochainPubKey, this.#agentService.signString(holochainPubKey), signedDid)
    }

    addEntanglementProofs(proofs: EntanglementProof[]): void {
        let entanglementProofs: EntanglementProof[];
        if (fs.existsSync(this.#entanglementProofs)) {
            entanglementProofs = Array.from(JSON.parse(fs.readFileSync(this.#entanglementProofs).toString()));
            entanglementProofs = entanglementProofs.concat(proofs);
            entanglementProofs = Array.from(new Set(entanglementProofs));
        } else {
            entanglementProofs = proofs 
        }

        fs.writeFileSync(this.#entanglementProofs, JSON.stringify(entanglementProofs))
    }

    deleteEntanglementProofs(proofs: EntanglementProof[]): void {
        if (fs.existsSync(this.#entanglementProofs)) {
            let entanglementProofs = Array.from(JSON.parse(fs.readFileSync(this.#entanglementProofs).toString()));
            for (const agent of proofs) {
                entanglementProofs.splice(entanglementProofs.findIndex((value) => value == agent), 1);
            }
            fs.writeFileSync(this.#entanglementProofs, JSON.stringify(entanglementProofs))
        }
    }

    getEntanglementProofs(): EntanglementProof[] {
        if (fs.existsSync(this.#entanglementProofs)) {
            let entanglementProofs: EntanglementProof[] = Array.from(JSON.parse(fs.readFileSync(this.#entanglementProofs).toString()));
            return entanglementProofs
        } else {
            return [] 
        }
    }
}