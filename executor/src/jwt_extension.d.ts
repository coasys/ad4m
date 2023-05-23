declare global {
    interface Jwt {
        async publish: (topic: String, data: any) => void;
        async generateJwt: (issuer, audience, expiration_time, capabililities) => string;
        async verifyJwt: (token: String) => {iss: String, aud: String, exp: number, iat: number, capabilities: any}
    }

    const JWT: Jwt;
}

export {};