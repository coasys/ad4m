declare global {
    interface Utils {
        getSigningDNA: () => Uint8Array;
        hash: (data: string | buffer) => string;
        async loadModule: (path: String) => string;
    }

    const UTILS: Utils;
}

export {};