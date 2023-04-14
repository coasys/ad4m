declare global {
    interface Utils {
        getSigningDNA: () => Uint8Array;
        hash: (data: string | buffer) => string;
    }

    const UTILS: Utils;
}

export {};