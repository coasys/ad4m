import { 
  sign_device_key, generate_entanglement_proof, add_entanglement_proofs,
  delete_entanglement_proofs, get_entanglement_proofs
} from 'ext:core/ops';

((globalThis) => {
  globalThis.ENTANGLEMENT_SERVICE = {
    signDeviceKey: async (deviceKey, deviceKeyType) => {
      return sign_device_key(deviceKey, deviceKeyType);
    },
    generateHolochainProof: async (holochainPubkey, signedDid) => {
      return generate_entanglement_proof(holochainPubkey, signedDid);
    },
    addEntaglementProofs: async (proofs) => {
      return add_entanglement_proofs(proofs);
    },
    deleteEntanglementProofs: async (proofs) => {
      return delete_entanglement_proofs(proofs);
    },
    getEntanglementProofs: async () => {
      return get_entanglement_proofs(proofs);
    }
  }
})(globalThis);