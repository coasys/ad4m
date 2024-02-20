((globalThis) => {
    const core = Deno.core;

    globalThis.SIGNATURE = {
        verifyStringSignedByDID: (did, didSigningKeyId, data, signedData) => {
            return core.ops.signature_verify_string_signed_by_did(did, didSigningKeyId, data, signedData);
        },
        verify: (expression) => {
            return core.ops.signature_verify(expression);
        }
    };
})(globalThis);