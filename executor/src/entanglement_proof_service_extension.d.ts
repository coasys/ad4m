declare global {
  interface EntanglementProofService {
    signDeviceKey: (deviceKey: string) => Promise<string>;
    generateHolochainProof: (holochainPubKey: string, signedDid: string) => Promise<string>;
    addEntanglementProofs: (proofs: string[]) => Promise<void>;
    deleteEntanglementProofs: (proofs: string[]) => Promise<void>;
    getEntanglementProofs: () => Promise<string[]>;
  }
}