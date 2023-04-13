declare global {
    interface Utils {
        getSigningDNA: () => Uint8Array;
    }

    const UTILS: Utils;
}

export {};