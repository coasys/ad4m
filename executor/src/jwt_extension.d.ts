declare global {
    interface Jwt {
        async publish: (topic: String, data: any) => void;
        async generateJwt: (issuer, audience, expiration_time) => string;
        async verifyJwt: (token: String) => {iss: String, aud: String, exp: String, iat: number}
    }

    const JWT: Jwt;
}

export {};