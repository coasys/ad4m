# @coasys/identity-wasm

The new device's side of `did:scid:coasys:1` for the browser, as WebAssembly: replay a key log
(signatures, receipts), take the link steps of a new device, open the private record and list
the devices that can answer a sign-in. It never holds a signing key: it returns the bytes to
sign, and WebCrypto signs them with a key that cannot be exported.

```ts
import init, { EncKey, KelChain, deviceString, newR, preInboxAddress, preInboxBytes,
  preInboxHeader, openMessage, ticketScid, checkMessage, claimBytes, claim, matchCode,
  openRecord, pickerDevices, proofValue } from "@coasys/identity-wasm";

await init(); // loads identity_bg.wasm next to the module

const enc = EncKey.generate();               // X25519; the only secret held in WASM memory
const text = deviceString(signMultikey, enc.multikey(), newR(), exp, hint, storeUrl);

const address = preInboxAddress(signMultikey, r);
const auth = { address, sign: signMultikey, r, store, method: "GET", path, nonce, expires };
const sig = proofValue(new Uint8Array(await crypto.subtle.sign("Ed25519", key, preInboxBytes(auth))));
const header = preInboxHeader(auth, sig);    // X-Coasys-Inbox
```

Exports (see `pkg/identity.d.ts`): `EncKey`, `KelChain` / `KelHandle` (`deviceKeyId`), `Pow`,
`deviceString`, `newR`, `preInboxAddress`, `preInboxBytes`, `preInboxHeader`, `openMessage`,
`ticketScid`, `checkMessage`, `claimBytes`, `claim`, `matchCode`, `bodyHash`, `requestBytes`,
`requestHeader`, `openRecord`, `pickerDevices`, `statementBytes`, `signedStatement`,
`ed25519Multikey`, `x25519Multikey`, `proofValue`. A failure throws `{ code, reason, detail }`.

Build: `pnpm --filter @coasys/identity-wasm build` (Rust, the `wasm32-unknown-unknown` target and
the `wasm-bindgen` CLI at the version `Cargo.lock` pins).
