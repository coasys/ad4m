/**
 * Link server protocol 2: what names a room and what a key signs in it.
 *
 * - The room id follows from the space's creator and its `UID` template parameter:
 *   `base58(sha256(CREATOR ‖ 0x00 ‖ UID))`, as `roomIdOf` in `link-server/src/membership.ts`
 *   checks it at genesis. Two spaces with the same `UID` and different creators are two rooms.
 * - The auth message a key signs (with `agentSignStringHex`):
 *   `coasys-link-server:auth:2:<origin>:<roomId>:<timestamp>`, where `<origin>` is the server's
 *   origin (`serverOrigin`: scheme, host and port of the server URL, lower case, without the
 *   default port). A server takes only its own origins, so it cannot relay a signature it was
 *   given to another link server, which would admit the relayer as this key.
 * - The room key statement (K7): a device key signs its X25519 key, raw Ed25519 over the
 *   `coasys:room-key` signing bytes of `{x25519PublicKey, did}`, which are the identity core's
 *   `room_key_signing_bytes` (`ad4m-identity-core/src/wire.rs`): canonical JSON (JCS) of the
 *   object with `domain` added. Every member checks the statement before it seals a room key to
 *   that X25519 key (K1), so a server cannot hand it a key of its own.
 * - Membership links (predicate prefix `coasys://membership/`) are plain signed links that go
 *   to `/membership`, never to `/commit` (K5).
 */

import { ed25519 } from "@noble/curves/ed25519";
import { sha256 } from "@noble/hashes/sha2";

import { base58Encode, didToEd25519PublicKey, hexToBytes } from "./encryption.js";
import type { LinkExpression, WireMembershipLink } from "./types.js";

/** The room id of the space `creator` made with `uid`: `base58(sha256(creator ‖ 0x00 ‖ uid))`. */
export function roomIdOf(creator: string, uid: string): string {
    const creatorBytes = new TextEncoder().encode(creator);
    const uidBytes = new TextEncoder().encode(uid);
    const input = new Uint8Array(creatorBytes.length + 1 + uidBytes.length);
    input.set(creatorBytes);
    input.set(uidBytes, creatorBytes.length + 1);
    return base58Encode(sha256(input));
}

/** What a key signs to authenticate to `roomId` on the server at `origin` (`serverOrigin`) at `timestamp` (ms). */
export function authMessage(origin: string, roomId: string, timestamp: number): string {
    return `coasys-link-server:auth:2:${origin}:${roomId}:${timestamp}`;
}

/**
 * The origin of the server at `serverUrl`, as the auth message names it: scheme, host and port,
 * lower case, without the default port or a trailing slash (the URL standard's origin). Throws
 * for a URL that is not http(s).
 */
export function serverOrigin(serverUrl: string): string {
    const url = new URL(serverUrl);
    if (url.protocol !== "http:" && url.protocol !== "https:") {
        throw new Error(`the link server URL ${serverUrl} is not http(s)`);
    }
    return url.origin;
}

export const ROOM_KEY_DOMAIN = "coasys:room-key";

const X25519_HEX = /^[0-9a-f]{64}$/;

/**
 * The `coasys:room-key` signing bytes of `{x25519PublicKey, did}`. Throws unless `did` is an
 * Ed25519 `did:key` and `x25519PublicKey` is 32 bytes of lowercase hex, as the core does.
 * Both values are ASCII without characters JSON escapes, so the canonical form is the object
 * with its keys sorted.
 */
export function roomKeySigningBytes(did: string, x25519PublicKey: string): Uint8Array {
    if (!X25519_HEX.test(x25519PublicKey)) throw new Error("x25519PublicKey must be 32 bytes of lowercase hex");
    if (did.includes("#")) throw new Error("a room key statement names a did:key without a fragment");
    didToEd25519PublicKey(did);
    const canonical = JSON.stringify({ did, domain: ROOM_KEY_DOMAIN, x25519PublicKey });
    return new TextEncoder().encode(canonical);
}

/** True when `signatureHex` is `did`'s Ed25519 signature over its room key statement (K1). */
export function verifyRoomKeySignature(did: string, x25519PublicKey: string, signatureHex: string): boolean {
    try {
        const bytes = roomKeySigningBytes(did, x25519PublicKey);
        return ed25519.verify(hexToBytes(signatureHex), bytes, didToEd25519PublicKey(did));
    } catch {
        return false;
    }
}

// ---------------------------------------------------------------------------
// Membership links
// ---------------------------------------------------------------------------

export const MEMBERSHIP_PREFIX = "coasys://membership/";

/** Whether `link` is a membership link: it goes to `/membership`, never to `/commit` (K5). */
export function isMembershipLink(link: LinkExpression): boolean {
    return typeof link.data?.predicate === "string" && link.data.predicate.startsWith(MEMBERSHIP_PREFIX);
}

/** A membership link as the link server carries it: author, timestamp, data, proof key and signature. */
export function toWireMembershipLink(link: LinkExpression): WireMembershipLink {
    const proof = link.proof as { key?: string; signature?: string; anchor?: string };
    return {
        author: link.author,
        timestamp: link.timestamp,
        data: { source: link.data.source, predicate: link.data.predicate as string, target: link.data.target },
        proof: {
            key: proof.key ?? "",
            signature: proof.signature ?? "",
            ...(proof.anchor !== undefined ? { anchor: proof.anchor } : {}),
        },
    };
}

/** A membership link from the link server, as the executor's `membershipLinks` takes it. */
export function fromWireMembershipLink(link: WireMembershipLink): LinkExpression {
    return {
        author: link.author,
        timestamp: link.timestamp,
        data: { source: link.data.source, predicate: link.data.predicate, target: link.data.target },
        proof: { ...link.proof },
    } as LinkExpression;
}
