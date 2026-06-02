/**
 * Canonical error type for AD4M Language exports.
 *
 * The runtime classifies errors by `code`. Unknown codes fall through
 * to a generic "language error" in the runtime side.
 */

export type LanguageErrorCode =
    | "not-found"
    | "invalid-input"
    | "permission-denied"
    | "transient"
    | "internal";

export class LanguageError extends Error {
    code: LanguageErrorCode;
    constructor(code: LanguageErrorCode, message: string) {
        super(message);
        this.name = "LanguageError";
        this.code = code;
    }
}

export const NotFound = (msg: string) => new LanguageError("not-found", msg);
export const InvalidInput = (msg: string) => new LanguageError("invalid-input", msg);
export const PermissionDenied = (msg: string) => new LanguageError("permission-denied", msg);
export const Transient = (msg: string) => new LanguageError("transient", msg);
export const Internal = (msg: string) => new LanguageError("internal", msg);
