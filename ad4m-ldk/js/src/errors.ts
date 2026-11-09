/**
 * Canonical error type for AD4M Language exports.
 *
 * The runtime classifies errors by `code`. Unknown codes fall through
 * to a generic "language error" in the runtime side.
 */

import type { IdentityErrorCode, TokenErrorCode } from "./types.js";

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

const IDENTITY_ERROR_CLASS: Record<IdentityErrorCode["code"], LanguageErrorCode> = {
    conflict: "invalid-input",
    "key-taken": "invalid-input",
    forbidden: "permission-denied",
    invalid: "invalid-input",
    "rate-limited": "transient",
    gone: "not-found",
    "not-found": "not-found",
    unavailable: "transient",
};

const TOKEN_ERROR_CLASS: Record<TokenErrorCode["code"], LanguageErrorCode> = {
    disabled: "permission-denied",
    "unknown-asset": "not-found",
    "unknown-account": "not-found",
    insufficient: "invalid-input",
    refused: "permission-denied",
    unavailable: "transient",
};

/**
 * A refusal of the identity interface. The message is the refusal as JSON,
 * so it survives the trip through the runtime, which keeps only `code`
 * and `message`; `parseIdentityError` reads it back.
 */
export class IdentityError extends LanguageError {
    refusal: IdentityErrorCode;
    constructor(refusal: IdentityErrorCode) {
        super(IDENTITY_ERROR_CLASS[refusal.code], JSON.stringify(refusal));
        this.name = "IdentityError";
        this.refusal = refusal;
    }
}

/** A refusal of the token interface, carried as `IdentityError` carries its refusal. */
export class TokenError extends LanguageError {
    refusal: TokenErrorCode;
    constructor(refusal: TokenErrorCode) {
        super(TOKEN_ERROR_CLASS[refusal.code], JSON.stringify(refusal));
        this.name = "TokenError";
        this.refusal = refusal;
    }
}

function parseRefusal<C extends string>(message: string, codes: Record<C, unknown>): { code: C } | null {
    let value: unknown;
    try {
        value = JSON.parse(message);
    } catch {
        return null;
    }
    if (typeof value !== "object" || value === null) return null;
    const code = (value as { code?: unknown }).code;
    if (typeof code !== "string" || !Object.prototype.hasOwnProperty.call(codes, code)) return null;
    return value as { code: C };
}

/** The identity refusal an error message carries, or null when it carries none. */
export function parseIdentityError(message: string): IdentityErrorCode | null {
    return parseRefusal(message, IDENTITY_ERROR_CLASS) as IdentityErrorCode | null;
}

/** The token refusal an error message carries, or null when it carries none. */
export function parseTokenError(message: string): TokenErrorCode | null {
    return parseRefusal(message, TOKEN_ERROR_CLASS) as TokenErrorCode | null;
}

export const NotFound =(msg: string) => new LanguageError("not-found", msg);
export const InvalidInput = (msg: string) => new LanguageError("invalid-input", msg);
export const PermissionDenied = (msg: string) => new LanguageError("permission-denied", msg);
export const Transient = (msg: string) => new LanguageError("transient", msg);
export const Internal = (msg: string) => new LanguageError("internal", msg);
