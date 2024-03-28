declare global {
    interface RuntimeService {
        friends(): Promise<string[]>;
        addMessageOutbox(did: string, message: object, wasSent: boolean): Promise<void>;
        getTrustedAgents(): Promise<string[]>;
    }
        
    const RUNTIME_SERVICE: RuntimeService;
}

export {};