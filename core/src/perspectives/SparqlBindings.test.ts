import { Literal } from '../Literal';
import { parseLit, parseLitNumber, parseLitBoolean, parseSparqlCount } from './SparqlBindings';

describe('parseLit', () => {
  it('returns empty string for undefined/null/empty', () => {
    expect(parseLit(undefined)).toBe('');
    expect(parseLit(null)).toBe('');
    expect(parseLit('')).toBe('');
  });

  it('returns the raw value unchanged when Literal.fromUrl throws (non-literal URI)', () => {
    expect(parseLit('just a string')).toBe('just a string');
    expect(parseLit('http://example.com/resource')).toBe('http://example.com/resource');
  });

  it('decodes literal:string: primitives', () => {
    expect(parseLit(Literal.from('hello').toUrl())).toBe('hello');
    expect(parseLit(Literal.from('hello world').toUrl())).toBe('hello world');
  });

  it('decodes literal:number: and literal:boolean: primitives to their string form', () => {
    expect(parseLit(Literal.from(42).toUrl())).toBe('42');
    expect(parseLit(Literal.from(true).toUrl())).toBe('true');
    expect(parseLit(Literal.from(false).toUrl())).toBe('false');
  });

  // Signed-envelope literals are produced by the `literal` language on write
  // for any property declared with `resolveLanguage: 'literal'` — see
  // rust-executor/src/languages/mod.rs (`expression_create` literal branch,
  // which calls `create_signed_expression` before `literal_encode`).
  // Consumers expect `.data` on read, not the envelope JSON.
  it("extracts `.data` from signed-envelope literals (resolveLanguage: 'literal')", () => {
    const envelope = {
      author: 'did:key:z6MkTest',
      timestamp: '2026-08-19T14:00:00.000Z',
      data: '<p>hello</p>',
      proof: { key: 'did:key:z6MkTest#z6MkTest', signature: 'deadbeef' },
    };
    expect(parseLit(Literal.from(envelope).toUrl())).toBe('<p>hello</p>');
  });

  it('extracts `.data` when the envelope only carries a `data` field', () => {
    expect(parseLit(Literal.from({ data: 'extracted' }).toUrl())).toBe('extracted');
  });

  it('falls back to JSON.stringify for objects without a string `.data`', () => {
    expect(parseLit(Literal.from({ foo: 'bar' }).toUrl())).toBe(JSON.stringify({ foo: 'bar' }));
  });

  it('falls back to JSON.stringify when `.data` is a non-string value', () => {
    const payload = { data: { nested: 'object' } };
    expect(parseLit(Literal.from(payload).toUrl())).toBe(JSON.stringify(payload));
  });

  // Some storage backends / query paths surface signed envelopes for
  // `resolveLanguage: 'literal'` properties as a bare JSON string rather
  // than a `literal:json:*` URL — the Flux Synergy view hit this when
  // Message.body arrived as `{"author":..,"timestamp":..,"data":"hi","proof":..}`
  // straight out of a SPARQL binding.  parseLit must still unwrap `.data`
  // in that shape or the raw envelope leaks into the chat UI.
  it('extracts .data from a plain-JSON signed envelope (no literal: URL wrapper)', () => {
    const envelope = JSON.stringify({
      author: 'did:key:z6MkTest',
      timestamp: '2026-08-19T14:00:00.000Z',
      data: '<p>hello</p>',
      proof: { key: 'did:key:z6MkTest#z6MkTest', signature: 'deadbeef' },
    });
    expect(parseLit(envelope)).toBe('<p>hello</p>');
  });

  it('leaves plain-JSON objects without a string .data unchanged', () => {
    // Not an envelope — preserve original JSON so downstream code sees
    // exactly what was stored.
    const raw = JSON.stringify({ foo: 'bar' });
    expect(parseLit(raw)).toBe(raw);
  });

  it('leaves plain text values that happen to start with `{` unchanged', () => {
    expect(parseLit('{ not really json')).toBe('{ not really json');
  });
});

describe('parseLitNumber', () => {
  it('returns 0 for empty input', () => {
    expect(parseLitNumber(undefined)).toBe(0);
    expect(parseLitNumber(null)).toBe(0);
    expect(parseLitNumber('')).toBe(0);
  });

  it('decodes literal:number: bindings', () => {
    expect(parseLitNumber(Literal.from(42).toUrl())).toBe(42);
    expect(parseLitNumber(Literal.from(3.14).toUrl())).toBe(3.14);
  });

  it('returns 0 for non-numeric decoded values', () => {
    expect(parseLitNumber(Literal.from('not a number').toUrl())).toBe(0);
  });
});

describe('parseLitBoolean', () => {
  it('returns false for empty input', () => {
    expect(parseLitBoolean(undefined)).toBe(false);
    expect(parseLitBoolean(null)).toBe(false);
    expect(parseLitBoolean('')).toBe(false);
  });

  it('decodes literal:boolean: bindings', () => {
    expect(parseLitBoolean(Literal.from(true).toUrl())).toBe(true);
    expect(parseLitBoolean(Literal.from(false).toUrl())).toBe(false);
  });

  it('returns false for any non-"true" decoded value', () => {
    expect(parseLitBoolean(Literal.from('yes').toUrl())).toBe(false);
    expect(parseLitBoolean(Literal.from(1).toUrl())).toBe(false);
  });
});

describe('parseSparqlCount', () => {
  it('returns 0 for undefined/null/empty result arrays', () => {
    expect(parseSparqlCount(undefined)).toBe(0);
    expect(parseSparqlCount(null)).toBe(0);
    expect(parseSparqlCount([])).toBe(0);
  });

  it('parses the first row `count` value', () => {
    expect(parseSparqlCount([{ count: '5' }])).toBe(5);
    expect(parseSparqlCount([{ count: '0' }])).toBe(0);
    expect(parseSparqlCount([{ count: '123' }, { count: '999' }])).toBe(123);
  });

  it('returns 0 for non-integer count values', () => {
    expect(parseSparqlCount([{ count: 'not-a-number' }])).toBe(0);
  });
});
