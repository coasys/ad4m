import { beforeEach, describe, expect, it } from 'vitest';
import { getLocal, removeLegacyTokens, setLocal } from './utils';

describe('utils', () => {
  beforeEach(() => localStorage.clear());

  it('removes the tokens of every old version and nothing else', () => {
    localStorage.setItem('0.13.0/ad4m-token', 'a');
    localStorage.setItem('0.12.1-staging.3/ad4m-token', 'b');
    localStorage.setItem('0.13.0/ad4m-url', 'http://localhost:12000');
    localStorage.setItem('ad4m-token', 'unversioned stays');
    localStorage.setItem('other/app-token', 'c');
    expect(removeLegacyTokens()).toBe(2);
    expect(localStorage.getItem('0.13.0/ad4m-token')).toBeNull();
    expect(localStorage.getItem('0.12.1-staging.3/ad4m-token')).toBeNull();
    expect(localStorage.getItem('0.13.0/ad4m-url')).toBe('http://localhost:12000');
    expect(localStorage.getItem('ad4m-token')).toBe('unversioned stays');
    expect(localStorage.getItem('other/app-token')).toBe('c');
  });

  it('keeps values under the package version', () => {
    setLocal('x', '1');
    expect(getLocal('x')).toBe('1');
    expect(Object.keys(localStorage).filter((k) => k.endsWith('/x'))).toHaveLength(1);
  });
});
