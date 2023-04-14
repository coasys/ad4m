((globalThis) => {
    const core = Deno.core;

    globalThis.UTILS = {
        getSigningDNA: () => {
            return core.ops.get_signing_dna();
        },
        hash: (data) => {
            return core.ops.hash(data);
        }
    };
  })(globalThis);
  