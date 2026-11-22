/** PKCE (RFC 7636, S256) and the random values the Connect flow needs. WebCrypto only. */

function base64url(bytes: Uint8Array): string {
  let binary = '';
  for (const byte of bytes) binary += String.fromCharCode(byte);
  return btoa(binary).replace(/\+/g, '-').replace(/\//g, '_').replace(/=+$/, '');
}

function randomToken(bytes: number): string {
  return base64url(crypto.getRandomValues(new Uint8Array(bytes)));
}

/** The session's `state`: 16 random bytes. */
export const newState = (): string => randomToken(16);

/** A PKCE `code_verifier`: 32 random bytes, 43 characters. It never leaves the page. */
export const newVerifier = (): string => randomToken(32);

/** The S256 `code_challenge` of `verifier`: `base64url(SHA-256(verifier))`. */
export async function challengeOf(verifier: string): Promise<string> {
  const digest = await crypto.subtle.digest('SHA-256', new TextEncoder().encode(verifier));
  return base64url(new Uint8Array(digest));
}
