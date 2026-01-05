import { Agent, AgentStatus } from "@coasys/ad4m";

export interface VerificationMethod {
    id: string;
    type: string;
    controller: string;
    publicKeyBase58?: string;
    publicKeyMultibase?: string;
    publicKeyJwk?: any; // Adjusted to 'any' since the specific structure is not provided
    // Add other fields from the did_key::VerificationMethod if they exist
}

export interface DidDocument {
    "@context": string | string[];
    id: string;
    alsoKnownAs?: string[];
    controller?: string | string[];
    verificationMethod: VerificationMethod[];
    authentication?: (string | VerificationMethod)[];
    assertionMethod?: (string | VerificationMethod)[];
    keyAgreement?: (string | VerificationMethod)[];
    capabilityInvocation?: (string | VerificationMethod)[];
    capabilityDelegation?: (string | VerificationMethod)[];
    service?: ServiceEndpoint[];
    // Add other fields from the did_key::Document if they exist
}

export interface ServiceEndpoint {
    id: string;
    type: string;
    serviceEndpoint: string;
    // Add other fields from the did_key::ServiceEndpoint if they exist
}

export interface Expression {
    author: string;
    timestamp: string;
    data: any;
    proof: ExpressionProof;
}

export interface ExpressionProof {
    signature: string;
    key: string;
}

declare global {
    interface RustAgent {
        didDocument: () => DidDocument;
        signingKeyId: () => string;
        did: () => string;
        createSignedExpression: (data: any) => Expression;
        sign: (payload: Uint8Array) => Uint8Array;
        signStringHex: (payload: string) => string;
        load: () => AgentStatus;
        agent: () => Agent;
        isInitialized: () => boolean;
        isUnlocked: () => boolean;
        unlock: (password: string) => boolean;
        lock: () => void;
        save_agent_profile: (agent: Agent) => void;
        createSignedExpressionForUser: (userEmail: string, data: unknown) => Promise<Expression>;
        
        /**
         * Gets the DID for a user by their email address.
         * 
         * This method follows the consistent naming pattern used by other user-scoped
         * operations (createSignedExpressionForUser, agentForUser).
         * 
         * @param userEmail - The email address of the user
         * @returns A promise that resolves to the user's DID
         * @throws {TypeError} If userEmail is not a non-empty string
         * @throws {Error} If no key is found for the given user email (user does not exist)
         * 
         * @example
         * ```typescript
         * try {
         *   const did = await AGENT.didForUser("user@example.com");
         *   console.log(`User DID: ${did}`);
         * } catch (error) {
         *   console.error("User not found or error occurred:", error);
         * }
         * ```
         */
        didForUser: (userEmail: string) => Promise<string>;
        
        agentForUser: (userEmail: string) => Promise<Agent>;
        listUserEmails: () => Promise<string[]>;
        getAllLocalUserDIDs: () => Promise<string[]>;
    }

    const AGENT: RustAgent;
}