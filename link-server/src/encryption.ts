/**
 * Room keys sealed to members' DIDs.
 *
 * Trust model: the room admin's language client generates each room's
 * AES-256-GCM key and seals it to every ACL member; any member holding a
 * version may seal it to a member lacking it. The server stores the sealed
 * boxes in `room_keys` and relays them via `GET /keys`; it never sees a room
 * key, and only checks a box's shape.
 *
 * A box is the AD4M executor's sealed box (`rust-executor/src/agent/sealed_box.rs`,
 * `docs-src/host-contract.md` "Sealed boxes"); tests/sealed-box.ts seals and
 * opens it as clients do.
 */

/** `{ ephemeralPublicKey, nonce, ciphertext }`, hex. */
export interface SealedBox {
  ephemeralPublicKey: string;
  nonce: string;
  ciphertext: string;
}
