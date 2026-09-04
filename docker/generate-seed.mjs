#!/usr/bin/env node
// generate-seed.mjs — Produces docker_seed.json and a pre-seed directory
// structure matching the executor's language storage layout.
//
// Usage: node generate-seed.mjs <bootstrap-dir> <output-dir>
//
// The output directory receives:
//   docker_seed.json          — seed file for the executor
//   languages/<hash>/bundle.js — pre-populated language bundles
//
// The hash algorithm matches the executor's calculate_language_hash():
//   SHA-256 → CIDv1 (raw codec 0x00) → multibase base58btc → "Qm" prefix

import { createHash } from "node:crypto";
import { readFileSync, mkdirSync, writeFileSync, existsSync } from "node:fs";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";

// --- CID / multibase encoding (no npm deps) ---
// Hand-rolled to avoid npm dependencies in the Docker build context.
// This script runs during `docker build` where node_modules may not be
// available and adding a `pnpm install` would bust the cache layer.
// The algorithm exactly mirrors rust-executor's calculate_language_hash():
//   SHA-256 → multihash (0x12, 0x20) → CIDv1 (raw codec 0x00) →
//   multibase base58btc ('z' prefix) → "Qm" prefix.
// If the Rust implementation changes, this must change to match.

const BASE58_ALPHABET = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

function base58Encode(bytes) {
    const digits = [0];
    for (let i = 0; i < bytes.length; i++) {
        let carry = bytes[i];
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
    let result = "";
    // leading zeros
    for (let i = 0; i < bytes.length && bytes[i] === 0; i++) {
        result += BASE58_ALPHABET[0];
    }
    for (let i = digits.length - 1; i >= 0; i--) {
        result += BASE58_ALPHABET[digits[i]];
    }
    return result;
}

function computeLanguageHash(bundleContent) {
    // SHA-256 digest
    const sha256 = createHash("sha256").update(bundleContent, "utf8").digest();

    // Multihash: 0x12 = sha2-256, 0x20 = 32 bytes
    const multihash = Buffer.concat([Buffer.from([0x12, 0x20]), sha256]);

    // CIDv1: version=1, codec=0x00 (raw identity)
    // Varint encoding: 1 → 0x01, 0 → 0x00
    const cidBytes = Buffer.concat([Buffer.from([0x01, 0x00]), multihash]);

    // Multibase base58btc: prefix 'z'
    const encoded = "z" + base58Encode(cidBytes);

    return "Qm" + encoded;
}

// --- Main ---

const bootstrapDir = resolve(process.argv[2] || "docker/bootstrap-languages");
const outputDir = resolve(process.argv[3] || "docker/seed-output");

const LANGUAGES = [
    { role: "languageLanguage", file: "language-language.js" },
    { role: "agentLanguage",    file: "agent-language.js" },
    { role: "neighbourhoodLanguage", file: "neighbourhood-language.js" },
    { role: "perspectiveLanguage",   file: "perspective-language.js" },
    { role: "linkLanguage",     file: "link-language.js" },
    { role: "fileStorageLanguage",   file: "file-storage-language.js" },
];

mkdirSync(join(outputDir, "languages"), { recursive: true });

const addresses = {};
const bundles = {};

for (const lang of LANGUAGES) {
    const filePath = join(bootstrapDir, lang.file);
    if (!existsSync(filePath)) {
        console.error("Missing: " + filePath);
        process.exit(1);
    }
    const content = readFileSync(filePath, "utf8");
    const hash = computeLanguageHash(content);
    addresses[lang.role] = hash;
    bundles[lang.role] = content;

    // Write bundle to the pre-seed directory
    const langDir = join(outputDir, "languages", hash);
    mkdirSync(langDir, { recursive: true });
    writeFileSync(join(langDir, "bundle.js"), content);

    console.log(lang.role + ": " + hash);
}

// Trusted agents MUST come from the bootstrap seed committed in this repo
// (`rust-executor/src/mainnet_seed.json`). Regular seed generation
// (`cli/src/bootstrap_publish.rs`) writes the publishing agent's DID there;
// language install refuses any language whose Expression `author` is not in
// that list (or equal to the local agent DID). An empty array here is not
// a valid standalone-mode shortcut.
//
// Language *hashes* in this file are still computed from the local
// `bootstrap-languages/` copies (this image is a self-contained deploy, not
// a republish of mainnet addresses). Their Expression `author` is left
// empty at build time and rewritten to the container agent's DID on first
// boot (`stamp_bootstrap_language_authors` in docker-entrypoint.sh) so
// `agent_did == language_author` passes. Do not stamp a mainnet trusted
// DID onto locally-hashed bundles — that would claim those agents published
// code they did not.
const repoRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
const committedSeedPath = join(repoRoot, "rust-executor/src/mainnet_seed.json");
if (!existsSync(committedSeedPath)) {
    console.error("Missing committed bootstrap seed: " + committedSeedPath);
    process.exit(1);
}
const committedSeed = JSON.parse(readFileSync(committedSeedPath, "utf8"));
const trustedAgents = Array.isArray(committedSeed.trustedAgents)
    ? committedSeed.trustedAgents.filter((d) => typeof d === "string" && d.length > 0)
    : [];
if (trustedAgents.length === 0) {
    console.error("Committed bootstrap seed has no trustedAgents: " + committedSeedPath);
    process.exit(1);
}
console.log("trustedAgents (from committed seed):");
for (const did of trustedAgents) console.log("  " + did);

// Build seed JSON
const seed = {
    trustedAgents,
    knownLinkLanguages: [addresses.linkLanguage],
    directMessageLanguage: "",
    agentLanguage: addresses.agentLanguage,
    perspectiveLanguage: addresses.perspectiveLanguage,
    neighbourhoodLanguage: addresses.neighbourhoodLanguage,
    languageLanguageBundle: bundles.languageLanguage,
};

writeFileSync(
    join(outputDir, "docker_seed.json"),
    JSON.stringify(seed, null, 2)
);

// Build language-language KV file so storageGet("bundle-<hash>") and
// storageGet("meta-<hash>") both work at executor startup. Without these,
// applyTemplateAndPublish (used by Flux createCommunity) fails with
// "Language not found" because expressionGet returns null.
const langLangAddr = addresses.languageLanguage;
const kv = {};
for (const lang of LANGUAGES) {
    if (lang.role === "languageLanguage") continue;
    const addr = addresses[lang.role];
    kv[langLangAddr + "::bundle-" + addr] = bundles[lang.role];

    // author left empty on purpose — stamped to the container DID at first boot.
    const expression = {
        author: "",
        timestamp: "1970-01-01T00:00:00.000Z",
        data: {
            address: addr,
            author: "",
            name: lang.role,
            description: "Bootstrap " + lang.role,
            possibleTemplateParams: ["uid", "name"],
            templateAppliedParams: null,
            templateSourceLanguageAddress: null,
            templated: false,
        },
        proof: { key: "", signature: "" },
    };
    kv[langLangAddr + "::meta-" + addr] = JSON.stringify(expression);
}
const sortedKv = {};
Object.keys(kv).sort().forEach(k => { sortedKv[k] = kv[k]; });

const kvDir = join(outputDir, "language-language-kv");
mkdirSync(kvDir, { recursive: true });
writeFileSync(join(kvDir, "ad4m-language-kv.json"), JSON.stringify(sortedKv));
writeFileSync(join(kvDir, "address.txt"), langLangAddr);

// Write file-storage language hash so the Dockerfile can patch WE's
// hardcoded FILE_STORAGE_LANGUAGE constant to use the Docker-local version.
if (addresses.fileStorageLanguage) {
    writeFileSync(join(outputDir, "file-storage-hash.txt"), addresses.fileStorageLanguage);
    console.log("\nFile-storage hash: " + addresses.fileStorageLanguage);
}

console.log("\nSeed written to: " + join(outputDir, "docker_seed.json"));
console.log("Pre-seed languages in: " + join(outputDir, "languages"));
console.log("Language-language KV: " + join(kvDir, "ad4m-language-kv.json"));
console.log("Language-language address: " + langLangAddr);
