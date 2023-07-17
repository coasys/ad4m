declare global {
    interface PrologService {
        async startPrologService: () => void;
        async runQuery: (query: String) => String;
        async loadModuleString: (module_name: String, program: String) => void;
    }

    const PROLOG: PrologService;
}

export {};