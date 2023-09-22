declare global {
    interface Utils {
        getSigningDNA: () => Uint8Array;
        hash: (data: string | buffer) => string;
        consoleLog: (...args) => void;
        consoleDebug: (...args) => void;
        consoleError: (...args) => void;
        consoleWarn: (...args) => void;
        async loadModule: (path: String) => string;
    }

    const UTILS: Utils;
}

export {};