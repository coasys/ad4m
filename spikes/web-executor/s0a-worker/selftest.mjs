import * as shim from "./web/shim.js";
import { ed25519, x25519 } from "@noble/curves/ed25519";
import { sha256 } from "@noble/hashes/sha2";
import { hkdf } from "@noble/hashes/hkdf";
import { gcm } from "@noble/ciphers/aes";
import { createHash } from "node:crypto";
const { did } = shim.configure({ log: () => {} });
console.log("did", did);
// seal like encryption.ts sealToDid
const B58="123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";
function b58d(s){let n=0n;for(const c of s)n=n*58n+BigInt(B58.indexOf(c));let h=n.toString(16);if(h.length%2)h="0"+h;return Uint8Array.from(Buffer.from(h,"hex"));}
const pub = b58d(did.slice(9)).slice(2);
const rpk = ed25519.utils.toMontgomery(pub);
const es = x25519.utils.randomPrivateKey(); const epk = x25519.getPublicKey(es);
const ss = x25519.getSharedSecret(es, rpk);
const salt = new Uint8Array(64); salt.set(epk); salt.set(rpk,32);
const key = hkdf(sha256, ss, salt, new TextEncoder().encode("ad4m:sealed-box:v1"), 32);
const nonce = crypto.getRandomValues(new Uint8Array(12));
const pt = crypto.getRandomValues(new Uint8Array(32));
const ct = gcm(key, nonce).encrypt(pt);
const h=u=>Buffer.from(u).toString("hex");
const opened = shim.agentOpenSealed({ephemeralPublicKey:h(epk),nonce:h(nonce),ciphertext:h(ct)});
console.log("sealed-box roundtrip", opened === h(pt));
// signature verify like link-server
const ch = "ab".repeat(32);
const sig = shim.agentSignStringHex(ch);
console.log("sig verifies (sha256 of challenge)", ed25519.verify(sig, createHash("sha256").update(ch).digest(), pub));
// hash vs generate-seed algorithm
function seedHash(c){const d=createHash("sha256").update(c,"utf8").digest();const cid=Buffer.concat([Buffer.from([1,0,0x12,0x20]),d]);let digits=[0];for(const b of cid){let carry=b;for(let j=0;j<digits.length;j++){carry+=digits[j]<<8;digits[j]=carry%58;carry=(carry/58)|0;}while(carry>0){digits.push(carry%58);carry=(carry/58)|0;}}let r="";for(let i=0;i<cid.length&&cid[i]===0;i++)r+="1";for(let i=digits.length-1;i>=0;i--)r+=B58[digits[i]];return "Qmz"+r;}
for (const v of ["", "hello", "{\"source\":\"a\",\"target\":\"b\"}", "ünïcödé ✓"]) console.log(JSON.stringify(v), shim.hash(v), shim.hash(v)===seedHash(v));
