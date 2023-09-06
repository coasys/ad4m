declare global {
    interface PrologService {
        async spawnEngine: (name: String) => void;
        async runQuery: (name: Strin, query: String) => String;
        async loadModuleString: (name: String, module_name: String, program: String) => void;
    }

    const PROLOG: PrologService;
}

export {};