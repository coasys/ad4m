((globalThis) => {
    const core = Deno.core;

    globalThis.SIGNATURE = {
        verifyStringSignedByDID: (did, didSigningKeyId, data, signedData) => {
            let { isValid } = core.ops.signature_verify_string_signed_by_did(did, didSigningKeyId, data, signedData)
            return isValid;
        },
        verify: (expression) => {
            let { isValid } = core.ops.signature_verify(expression);
            return isValid;
        }
    };
})(globalThis);