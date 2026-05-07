/**
 * Unit tests for PerspectiveProxy utility functions.
 */

describe('PerspectiveProxy getClassShape sh:in URI-decoding', () => {
    // This tests the sh:in parsing logic in getClassShape() which was fixed
    // to URI-decode values from the Rust executor's SPARQL results

    function parseShInValue(raw: string): Array<{ value: string; label?: string }> | undefined {
        // Reproduce the exact parsing logic from PerspectiveProxy.getClassShape()
        try {
            if (raw.startsWith('literal:string:')) {
                raw = decodeURIComponent(raw.substring('literal:string:'.length));
            }
            return JSON.parse(raw);
        } catch { return undefined; }
    }

    it('parses plain JSON sh:in values', () => {
        const result = parseShInValue('[{"value":"a"},{"value":"b","label":"B"}]');
        expect(result).toEqual([
            { value: 'a' },
            { value: 'b', label: 'B' },
        ]);
    });

    it('strips literal:string: prefix and parses', () => {
        const result = parseShInValue('literal:string:[{"value":"x"},{"value":"y"}]');
        expect(result).toEqual([
            { value: 'x' },
            { value: 'y' },
        ]);
    });

    it('URI-decodes encoded sh:in values from Rust executor', () => {
        // Rust executor may URI-encode the JSON when returning SPARQL results
        const encoded = 'literal:string:' + encodeURIComponent('[{"value":"hello world"},{"value":"a&b"}]');
        const result = parseShInValue(encoded);
        expect(result).toEqual([
            { value: 'hello world' },
            { value: 'a&b' },
        ]);
    });

    it('handles double-encoded brackets and quotes', () => {
        const encoded = 'literal:string:%5B%7B%22value%22%3A%22active%22%7D%2C%7B%22value%22%3A%22inactive%22%7D%5D';
        const result = parseShInValue(encoded);
        expect(result).toEqual([
            { value: 'active' },
            { value: 'inactive' },
        ]);
    });

    it('returns undefined for malformed JSON', () => {
        const result = parseShInValue('literal:string:not-json');
        expect(result).toBeUndefined();
    });

    it('handles already-decoded values without prefix', () => {
        const result = parseShInValue('[{"value":"test"}]');
        expect(result).toEqual([{ value: 'test' }]);
    });
})
