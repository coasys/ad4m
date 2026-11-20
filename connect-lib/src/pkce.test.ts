import { describe, expect, it } from 'vitest';
import { challengeOf, newState, newVerifier } from './pkce';

describe('pkce', () => {
  it('derives the RFC 7636 appendix B challenge', async () => {
    expect(await challengeOf('dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk')).toBe(
      'E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM',
    );
  });

  it('makes a 43 character verifier and a 22 character state from random bytes', () => {
    const verifier = newVerifier();
    expect(verifier).toMatch(/^[A-Za-z0-9_-]{43}$/);
    expect(newState()).toMatch(/^[A-Za-z0-9_-]{22}$/);
    expect(newVerifier()).not.toBe(verifier);
  });
});
