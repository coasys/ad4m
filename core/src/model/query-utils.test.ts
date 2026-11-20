import { formatQueryValue } from './query-utils';

describe('formatQueryValue', () => {
  it('escapes backslash, quotes, newline, carriage return and tab in a string', () => {
    expect(formatQueryValue(`a\\b'c"d\ne\rf\tg`)).toBe(`'a\\\\b\\'c\\"d\\ne\\rf\\tg'`);
  });

  it('escapes the backslash before the quote it precedes', () => {
    expect(formatQueryValue(`\\'`)).toBe(`'\\\\\\''`);
  });

  it('passes numbers and booleans through and recurses into arrays', () => {
    expect(formatQueryValue(3)).toBe('3');
    expect(formatQueryValue(false)).toBe('false');
    expect(formatQueryValue(['x', 1])).toBe(`['x', 1]`);
  });
});
