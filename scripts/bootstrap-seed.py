#!/usr/bin/env python3
"""Computes the network seed that the built bootstrap languages give, without a key.

A language's address is the hash of its bundle (see `calculate_language_hash` in
rust-executor/src/languages/mod.rs), and the seed's trustedAgents come from the
seed proto. So the seed `ad4m-executor dev generate-bootstrap` writes is known
before anything is published.

Usage, from the repo root, after `pnpm run build-languages`:
  scripts/bootstrap-seed.py --check   # exit 1 if a committed seed differs from the built one
  scripts/bootstrap-seed.py --write   # write the built seed to every seedOutputs path
--out FILE also writes the built seed to FILE.
"""

import argparse
import hashlib
import json
import os
import sys

BASE58 = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"


def base58(data: bytes) -> str:
    n = int.from_bytes(data, "big")
    out = ""
    while n:
        n, r = divmod(n, 58)
        out = BASE58[r] + out
    return "1" * (len(data) - len(data.lstrip(b"\0"))) + out


def language_address(bundle: str) -> str:
    """The same address as `calculate_language_hash`: "Qm" + the base58btc
    multibase of a CIDv1 (codec 0x00) of the bundle's sha2-256 multihash."""
    digest = hashlib.sha256(bundle.encode("utf-8")).digest()
    cid = bytes([0x01, 0x00, 0x12, 0x20]) + digest
    return "Qm" + "z" + base58(cid)


def built_seed(proto: dict, base: str) -> dict:
    def read(resource: str) -> str:
        path = os.path.join(base, resource)
        try:
            with open(path, encoding="utf-8", newline="") as f:
                return f.read()
        except FileNotFoundError:
            sys.exit(f"{os.path.relpath(path)} is missing: run pnpm run build-languages first.")

    def address(language: dict) -> str:
        return language_address(read(language["resource"]))

    trusted = proto.get("trustedAgents") or []
    if not trusted:
        sys.exit("The seed proto has no trustedAgents: the seed would trust whoever publishes, "
                 "so it can't be known before the publish.")
    # Field order of BootstrapSeed in cli/src/bootstrap_publish.rs.
    return {
        "trustedAgents": trusted,
        "knownLinkLanguages": [address(l) for l in proto["linkLanguages"]],
        "directMessageLanguage": "",
        "agentLanguage": address(proto["agentLanguage"]),
        "perspectiveLanguage": address(proto["perspectiveLanguage"]),
        "neighbourhoodLanguage": address(proto["neighbourhoodLanguage"]),
        "languageLanguageBundle": read(proto["languageLanguageRef"]),
    }


def describe(field: str, value) -> str:
    if field == "languageLanguageBundle":
        return f"bundle sha256 {hashlib.sha256(value.encode('utf-8')).hexdigest()[:16]}…, {len(value)} chars"
    return json.dumps(value)


def serialize(seed: dict) -> str:
    # Byte-equal to serde_json::to_string_pretty for this shape.
    return json.dumps(seed, indent=2, ensure_ascii=False)


def main() -> int:
    parser = argparse.ArgumentParser()
    mode = parser.add_mutually_exclusive_group(required=True)
    mode.add_argument("--check", action="store_true")
    mode.add_argument("--write", action="store_true")
    parser.add_argument("--proto", default="cli/seed_proto.json")
    parser.add_argument("--out")
    args = parser.parse_args()

    # Resources and seedOutputs are relative to the proto's directory, the
    # working directory generate-bootstrap runs in.
    base = os.path.dirname(os.path.abspath(args.proto))
    with open(args.proto, encoding="utf-8") as f:
        proto = json.load(f)
    seed = built_seed(proto, base)
    outputs = [os.path.join(base, p) for p in proto.get("seedOutputs") or ["mainnet_seed.json"]]

    if args.out:
        with open(args.out, "w", encoding="utf-8", newline="") as f:
            f.write(serialize(seed))

    if args.write:
        for path in outputs:
            with open(path, "w", encoding="utf-8", newline="") as f:
                f.write(serialize(seed))
            print(f"Wrote {os.path.relpath(path)}")
        return 0

    drift = False
    for path in outputs:
        with open(path, encoding="utf-8") as f:
            committed = json.load(f)
        for field, value in seed.items():
            if committed.get(field) != value:
                drift = True
                print(f"DRIFT {os.path.relpath(path)} {field}: committed {describe(field, committed.get(field))}, "
                      f"built {describe(field, value)}")
    if drift:
        print("The committed seed doesn't pin the languages this source builds. "
              "Run scripts/bootstrap-seed.py --write after pnpm run build-languages and commit the seed.")
        return 1
    print("The committed seed pins the languages this source builds:")
    for field in ("agentLanguage", "perspectiveLanguage", "neighbourhoodLanguage"):
        print(f"  {field} {seed[field]}")
    for address in seed["knownLinkLanguages"]:
        print(f"  link language {address}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
