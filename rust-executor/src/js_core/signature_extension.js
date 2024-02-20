((globalThis) => {
    const core = Deno.core;

    globalThis.SIGNATURE = {
        verifyStringSignedByDID: (did, didSigningKeyId, data, signedData) => {
            let { is_valid } = core.ops.signature_verify_string_signed_by_did(did, didSigningKeyId, data, signedData)
            return is_valid;
        },
        verify: (expression) => {
            let { is_valid } =  core.ops.signature_verify(expression);
            return is_valid;
        }
    };
})(globalThis);