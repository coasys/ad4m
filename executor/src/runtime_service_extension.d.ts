declare global {
    interface RuntimeService {
        friends(): Promise<string[]>;
        addMessageOutbox(did: string, message: object, wasSent: boolean): Promise<void>;
        getTrustedAgents(): Promise<string[]>;
        addDebugString(languageAddress: string, debugString: string, operation: string): Promise<void>;
        getDebugStrings(languageAddress?: string): Promise<DebugStringEntry[]>;
    }

    interface DebugStringEntry {
        language_address: string;
        debug_string: string;
        operation: string;
        timestamp: string;
    }
        
    const RUNTIME_SERVICE: RuntimeService;
}

export {};