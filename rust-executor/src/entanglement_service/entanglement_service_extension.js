((globalThis) => {
  const core = Deno.core;

  globalThis.ENTANGLEMENT_SERVICE = {
    signDeviceKey: async (deviceKey, deviceKeyType) => {
      return core.ops.sign_device_key(deviceKey, deviceKeyType);
    },
    generateHolochainProof: async (holochainPubkey, signedDid) => {
      return core.ops.generate_entanglement_proof(holochainPubkey, signedDid);
    },
    addEntaglementProofs: async (proofs) => {
      return core.ops.add_entanglement_proofs(proofs);
    },
    deleteEntanglementProofs: async (proofs) => {
      return core.ops.delete_entanglement_proofs(proofs);
    },
    getEntanglementProofs: async () => {
      return core.ops.get_entanglement_proofs(proofs);
    }
  }
})(globalThis);