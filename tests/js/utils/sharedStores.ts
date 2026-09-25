// Shared mode for the local language-language and neighbourhood store.
//
// Every test executor gets its own data path, so their ad4m:host KV stores
// are separate. bootstrap-languages/local/language-language.js and
// local/neighbourhood-language.js store in a directory instead when their
// language settings carry a `storagePath`. Pointing all executors at the
// same directories lets one executor publish a language or neighbourhood
// and another fetch it, without Holochain.
//
// Settings live in `<data>/ad4m/languages/<address>/settings.json`, the file
// the executor reads when it loads a language, so they are written after
// `ad4m-executor init` and before `run`.
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import path from "path";
import { fileURLToPath } from "url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));

// Both lie under tests/js, the executor's working directory: system languages
// may read and write there. The language directory is the one
// get-builtin-test-langs.js fills, and the location the old
// local-language-persistence blob used.
export const SHARED_LANGUAGES_DIR = path.resolve(__dirname, "..", "tst-tmp", "languages");
export const SHARED_NEIGHBOURHOODS_DIR = path.resolve(__dirname, "..", "tst-tmp", "neighbourhoods");

const BASE58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

function base58(bytes: Buffer): string {
    const digits = [0];
    for (const byte of bytes) {
        let carry = byte;
        for (let j = 0; j < digits.length; j++) {
            carry += digits[j] << 8;
            digits[j] = carry % 58;
            carry = (carry / 58) | 0;
        }
        while (carry > 0) {
            digits.push(carry % 58);
            carry = (carry / 58) | 0;
        }
    }
    let out = "";
    for (let i = 0; i < bytes.length && bytes[i] === 0; i++) out += BASE58[0];
    for (let i = digits.length - 1; i >= 0; i--) out += BASE58[digits[i]];
    return out;
}

/** The executor's language address for a bundle (`calculate_language_hash`):
 *  "Qm" + base58btc multibase of a CIDv1 over the SHA-256 multihash. Same
 *  algorithm as bootstrap-languages/local/generate-seed.mjs. */
export function languageAddress(bundle: string): string {
    const digest = createHash("sha256").update(bundle, "utf8").digest();
    const cid = Buffer.concat([Buffer.from([0x01, 0x00, 0x12, 0x20]), digest]);
    return "Qmz" + base58(cid);
}

function writeSettings(dataPath: string, address: string, settings: object) {
    const dir = path.join(dataPath, "ad4m", "languages", address);
    mkdirSync(dir, { recursive: true });
    writeFileSync(path.join(dir, "settings.json"), JSON.stringify(settings));
}

/** Point the seed's language-language and neighbourhood language at the shared
 *  directories. Languages that don't read a `storagePath` setting ignore it. */
export function configureSharedStores(dataPath: string, bootstrapSeedPath: string) {
    const seed = JSON.parse(readFileSync(bootstrapSeedPath, "utf8"));
    if (seed.languageLanguageBundle) {
        mkdirSync(SHARED_LANGUAGES_DIR, { recursive: true });
        writeSettings(dataPath, languageAddress(seed.languageLanguageBundle), { storagePath: SHARED_LANGUAGES_DIR });
    }
    if (seed.neighbourhoodLanguage) {
        mkdirSync(SHARED_NEIGHBOURHOODS_DIR, { recursive: true });
        writeSettings(dataPath, seed.neighbourhoodLanguage, { storagePath: SHARED_NEIGHBOURHOODS_DIR });
    }
}

/** Whether a language with this address was published into the shared directory. */
export function sharedLanguageExists(address: string): boolean {
    return existsSync(path.join(SHARED_LANGUAGES_DIR, `meta-${address}.json`));
}
