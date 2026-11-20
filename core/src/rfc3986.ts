// Internal to the SDK: index.ts re-exports utils.ts, so this lives on its own.

/**
 * Percent-encodes a value as an RFC 3986 URI component: `encodeURIComponent`,
 * plus `!'()*` as `%21 %27 %28 %29 %2A`. `Literal` URLs, flow transition URIs
 * and the `literal:string:` alternatives of getter WHERE filters use it.
 */
export function encodeRFC3986URIComponent(value: string | number | boolean): string {
    return encodeURIComponent(value)
        .replace(
            /[!'()*]/g,
            (c) => `%${c.charCodeAt(0).toString(16).toUpperCase()}`
        );
}
