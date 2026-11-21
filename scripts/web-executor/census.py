"""Census of the wasm32 hazards left in the web executor's ring-2 files.

Reads rust-executor/src (relative to the repo root this script lives in) and prints one
line per hazard class: "<class> in ring 2: <count>". A count is a number of source lines.
Not counted: test files, `#[cfg(test)] mod x { ... }` blocks, modules declared under
`#[cfg(test)]`, comment lines, and items under `#[cfg(not(target_arch = "wasm32"))]`
(native only: the web executor never compiles them).

Ring 2 is the part of the executor the web executor builds (E1's ring2.py). A site that
goes through `crate::rt` (the runtime shim) is not a hazard: `rt::spawn(`,
`rt::time::Instant::now()`, or a bare `Instant::now()` / `sleep(` in a file that imports
the name from `rt::time`.

Usage: python3 -I scripts/web-executor/census.py [--sites]
  --sites  also print every counted line, grouped by class.
No dependencies beyond the standard library.
"""

import os
import re
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
CRATE = os.path.join(ROOT, "rust-executor")

# ---- ring 2 (E1's tools/ring2.py) ----
RING2_EXACT = {
    "src/api/ws_handler.rs", "src/api/perspectives_ws.rs", "src/api/perspective_instances_ws.rs",
    "src/api/agent_ws.rs", "src/api/guards.rs", "src/api/errors.rs", "src/api/types.rs",
    "src/api/event_interest.rs", "src/pubsub.rs", "src/events.rs", "src/secret.rs", "src/wallet.rs",
    "src/db_backend.rs", "src/db.rs", "src/helpers.rs", "src/utils.rs", "src/presence.rs",
    "src/billing.rs", "src/config.rs", "src/config_file.rs", "src/user_management.rs",
    "src/globals.rs",
}
RING2_PREFIX = ("src/perspectives/", "src/agent/", "src/types/", "src/db/")
# Inside the prefixes, but AI: stubbed as a whole on wasm32.
EXCLUDE = (
    "src/perspectives/interpretation/",
    "src/perspectives/agent_session.rs",
    "src/db/ai_sessions.rs",
)
TEST_FILE = re.compile(r"(_tests?|_e2e|test_support|tests/.*)\.rs$|/tests\.rs$|/tests/")


def is_ring2(path):
    if path.startswith(EXCLUDE) or TEST_FILE.search(path):
        return False
    return path in RING2_EXACT or path.startswith(RING2_PREFIX)


# ---- hazard classes ----
# `kind` says how a match is resolved:
#   "path": the matched name counts unless the file imports it from crate::rt.
#   "plain": every match counts.
CLASSES = [
    ("deno_core", "plain", [r"\bdeno_core::"]),
    # tokio::spawn / tokio::task::spawn, or a bare `spawn(` (imported from tokio).
    ("tokio::spawn", "plain", [r"\btokio::(task::)?spawn\b(?!_)", r"(?<![\w:.])(?<!fn )spawn\("]),
    ("spawn_blocking", "plain", [r"(?<!rt::)\bspawn_blocking\b"]),
    # Clock reads: std or tokio Instant / SystemTime. chrono's Utc::now / Local::now are not
    # here: chrono's `wasmbind` feature covers them.
    ("std clock", "path", [r"(?P<pre>[\w:]*)\b(?P<name>Instant|SystemTime)::now\("]),
    ("tokio timer", "path",
     [r"(?P<pre>[\w:]*?)(?<![.\w])(?P<name>sleep|sleep_until|timeout|timeout_at|interval|interval_at)\("]),
    ("runtime handle", "plain",
     [r"\bblock_in_place\b", r"\bHandle::(try_)?current\b", r"\bblock_on\("]),
    ("std::thread", "plain",
     [r"\bstd::thread\b", r"\bthread::(spawn|sleep|Builder|scope)\b"]),
]
COMPILED = [(n, k, [re.compile(p) for p in ps]) for n, k, ps in CLASSES]


def rt_time_imports(text):
    """Names a file imports from crate::rt::time, and whether it imports the module."""
    names = set()
    for m in re.finditer(r"use\s+(?:crate|super)::(?:[\w:]*::)?rt::time::(\{[^}]*\}|\w+)\s*;", text, re.S):
        body = m.group(1)
        for part in re.split(r"[,{}\s]+", body):
            if part:
                names.add(part.split(" as ")[-1])
    module = re.search(r"use\s+(?:crate|super)::(?:[\w:]*::)?rt::(\{[^}]*\btime\b[^}]*\}|time)\s*;", text, re.S)
    return names, bool(module)


def path_hazard(pre, name, names, has_module):
    """Is `<pre><name>(` a std/tokio clock or timer (not crate::rt::time)?"""
    if name in ("sleep",) and pre.endswith("thread::"):
        return False  # std::thread class
    if pre.endswith("rt::time::"):
        return False
    if pre.endswith(("std::time::", "tokio::time::")):
        return True
    if pre == "time::":
        return not has_module
    if pre == "":
        return name not in names
    # Some other path (`Self::`, `foo::`, `JoinSet::`): only std/tokio paths count.
    return pre.endswith("time::")


def strip_tests(lines):
    """Blank out `#[cfg(test)] mod x { ... }` blocks (E1's hazards.py)."""
    out, i = [], 0
    while i < len(lines):
        if (lines[i].strip() == "#[cfg(test)]" and i + 1 < len(lines)
                and re.match(r"\s*(pub(\([\w:]+\))? )?mod \w+ *\{", lines[i + 1])):
            depth, j = 0, i + 1
            while j < len(lines):
                depth += lines[j].count("{") - lines[j].count("}")
                if depth <= 0 and "{" in "".join(lines[i + 1:j + 1]):
                    break
                j += 1
            out += [""] * (j - i + 1)
            i = j + 1
            continue
        out.append(lines[i])
        i += 1
    return out


NATIVE_ONLY = re.compile(r'\s*#\[cfg\(not\(target_arch = "wasm32"\)\)\]\s*$')


def strip_native_only(lines):
    """Blank out each item or statement under `#[cfg(not(target_arch = "wasm32"))]`:
    the web executor never compiles it, so it holds no wasm32 hazard."""
    out, i = [], 0
    while i < len(lines):
        if not NATIVE_ONLY.match(lines[i]):
            out.append(lines[i])
            i += 1
            continue
        depth, opened, j = 0, False, i + 1
        while j < len(lines):
            code = code_part(lines[j])
            if not opened and code.strip().startswith(("#[", "///")):
                j += 1
                continue
            for c in code:
                if c == "{":
                    depth, opened = depth + 1, True
                elif c == "}":
                    depth -= 1
            if (opened and depth <= 0) or (not opened and code.rstrip().endswith(";")):
                break
            j += 1
        out += [""] * (j - i + 1)
        i = j + 1
    return out


def test_modules(src_dir):
    """Paths (relative to the crate) of modules declared as `#[cfg(test)] mod x;`."""
    excluded = []
    for dp, _, fs in os.walk(src_dir):
        for f in fs:
            if not f.endswith(".rs"):
                continue
            p = os.path.join(dp, f)
            lines = open(p, encoding="utf-8").read().split("\n")
            mod_dir = dp if f in ("mod.rs", "lib.rs", "lib_native.rs", "lib_wasm.rs") else os.path.join(dp, f[:-3])
            for i, l in enumerate(lines):
                if l.strip() != "#[cfg(test)]":
                    continue
                for nxt in lines[i + 1:i + 4]:
                    m = re.match(r"\s*(pub(\([\w:]+\))? )?mod (\w+);", nxt)
                    if m:
                        base = os.path.relpath(os.path.join(mod_dir, m.group(3)), CRATE)
                        excluded += [base + ".rs", base + "/"]
                        break
                    if not nxt.strip().startswith("#["):
                        break
    return tuple(excluded)


STRING = re.compile(r'"(?:[^"\\]|\\.)*"')


def code_part(line):
    """The line without string literal contents and without a trailing `//` comment."""
    line = STRING.sub('""', line)
    i = line.find("//")
    return line if i < 0 else line[:i]


def main():
    show_sites = "--sites" in sys.argv[1:]
    src_dir = os.path.join(CRATE, "src")
    skip = test_modules(src_dir)
    counts = {n: 0 for n, _, _ in COMPILED}
    sites = {n: [] for n, _, _ in COMPILED}
    for dp, _, fs in sorted(os.walk(src_dir)):
        for f in sorted(fs):
            if not f.endswith(".rs"):
                continue
            rel = os.path.relpath(os.path.join(dp, f), CRATE)
            if not is_ring2(rel) or rel.startswith(skip):
                continue
            text = open(os.path.join(dp, f), encoding="utf-8").read()
            names, has_module = rt_time_imports(text)
            for no, line in enumerate(strip_native_only(strip_tests(text.split("\n"))), 1):
                if line.strip().startswith("//"):
                    continue
                code = code_part(line)
                for name, kind, pats in COMPILED:
                    hit = False
                    for pat in pats:
                        for m in pat.finditer(code):
                            if kind == "plain" or path_hazard(m.group("pre"), m.group("name"), names, has_module):
                                hit = True
                                break
                        if hit:
                            break
                    if hit:
                        counts[name] += 1
                        sites[name].append(f"{rel}:{no}: {line.strip()}")
    for name, _, _ in COMPILED:
        print(f"{name} in ring 2: {counts[name]}")
    if show_sites:
        for name, _, _ in COMPILED:
            print(f"\n## {name}")
            for s in sites[name]:
                print(s)


if __name__ == "__main__":
    main()
