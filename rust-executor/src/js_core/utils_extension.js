((globalThis) => {
    const core = Deno.core;

    globalThis.UTILS = {
        getSigningDNA: () => {
            return core.ops.get_signing_dna();
        }
    };
  })(globalThis);
  