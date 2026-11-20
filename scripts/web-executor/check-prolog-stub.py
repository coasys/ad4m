"""Check that the web executor's Prolog stub has every item ring 2 names.

The web executor (wasm32) builds without scryer-prolog. `rust-executor/src/web/prolog_service/`
stands in for `crate::prolog_service`, and `src/web/scryer_types.rs` for the crate
`scryer_prolog` (`lib_wasm.rs`). This script reads the ring-2 code that the web executor compiles
(the census's ring 2, without tests and without items under
`#[cfg(not(target_arch = "wasm32"))]`) and checks:

1. every `prolog_service::...` path exists in the stub (a module, or a `pub` item in it);
2. every `scryer_prolog::...` path exists in `scryer_types.rs`;
3. every method of the native `PrologService` that ring 2 calls (`.name(`) is in the stub.

Prints each path with "ok" or "MISSING" and exits 1 when one is missing.

Usage: python3 -I scripts/web-executor/check-prolog-stub.py
No dependencies beyond the standard library.
"""

import importlib.util
import os
import re
import sys

sys.dont_write_bytecode = True
HERE = os.path.dirname(os.path.abspath(__file__))
_spec = importlib.util.spec_from_file_location("census", os.path.join(HERE, "census.py"))
census = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(census)

CRATE = census.CRATE
SRC = os.path.join(CRATE, "src")
STUB = os.path.join(SRC, "web", "prolog_service")
SCRYER = os.path.join(SRC, "web", "scryer_types.rs")
NATIVE = os.path.join(SRC, "prolog_service", "mod.rs")

ITEM = re.compile(r"^\s*pub(?:\([\w:]+\))?\s+(?:async\s+)?(?:fn|const|static|struct|enum|type|mod|trait)\s+(\w+)", re.M)


def pub_items(path):
    return set(ITEM.findall(open(path, encoding="utf-8").read()))


def stub_module(parts):
    """Items of the stub module at `parts` (relative to prolog_service), or None."""
    base = os.path.join(STUB, *parts)
    for p in (os.path.join(base, "mod.rs"), base + ".rs"):
        if os.path.isfile(p):
            return pub_items(p)
    return None


def expand(tree):
    """`a::{b::C, D}` -> ["a::b::C", "a::D"] (one level of nesting per brace, recursively)."""
    tree = re.sub(r"\s+as\s+\w+", "", tree)  # `use a::B as C`: the path is a::B
    tree = re.sub(r"\s+", "", tree)
    m = re.match(r"^(.*?)\{(.*)\}$", tree)
    if not m:
        return [tree]
    prefix, body = m.groups()
    parts, depth, cur = [], 0, ""
    for c in body:
        if c == "," and depth == 0:
            parts.append(cur)
            cur = ""
            continue
        depth += c == "{"
        depth -= c == "}"
        cur += c
    if cur:
        parts.append(cur)
    return [p for part in parts if part for p in expand(prefix + part)]


def ring2_code():
    skip = census.test_modules(SRC)
    for dp, _, fs in sorted(os.walk(SRC)):
        for f in sorted(fs):
            rel = os.path.relpath(os.path.join(dp, f), CRATE)
            if not f.endswith(".rs") or not census.is_ring2(rel) or rel.startswith(skip):
                continue
            lines = open(os.path.join(dp, f), encoding="utf-8").read().split("\n")
            lines = census.strip_native_only(census.strip_tests(lines))
            yield rel, "\n".join(census.code_part(line) for line in lines)


PATH = re.compile(r"\b(prolog_service|scryer_prolog)::((?:\w+::)*(?:\w+|\{[^;]*?\}))", re.S)


def main():
    paths, calls = {}, {}
    methods = pub_items(NATIVE) - {"PrologService", "PrologMode", "PROLOG_MODE"}
    for rel, code in ring2_code():
        for m in PATH.finditer(code):
            for p in expand(m.group(2)):
                paths.setdefault((m.group(1), p), set()).add(rel)
        for name in methods:
            if re.search(r"\.\s*" + name + r"\s*\(", code):
                calls.setdefault(name, set()).add(rel)

    scryer = pub_items(SCRYER)
    stub_methods = stub_module([]) or set()
    missing = 0
    for (root, p), files in sorted(paths.items()):
        parts = [x for x in p.split("::") if x and x != "self"]
        if root == "scryer_prolog":
            ok = len(parts) >= 1 and parts[0] in scryer
        else:
            # A module path, or a module path and one item in it. A trailing segment may be
            # an enum variant or a method (`PrologMode::Pooled`): then the one before is the item.
            ok = False
            for cut in range(len(parts), -1, -1):
                mod = stub_module(parts[:cut])
                if mod is None:
                    continue
                rest = parts[cut:]
                ok = not rest or rest[0] in mod
                break
        missing += not ok
        print(f"{'ok     ' if ok else 'MISSING'} {root}::{p}  ({', '.join(sorted(files))})")
    for name, files in sorted(calls.items()):
        ok = name in stub_methods
        missing += not ok
        print(f"{'ok     ' if ok else 'MISSING'} PrologService::{name}()  ({', '.join(sorted(files))})")
    print(f"prolog stub: {len(paths)} paths, {len(calls)} methods, {missing} missing")
    return 1 if missing else 0


if __name__ == "__main__":
    sys.exit(main())
