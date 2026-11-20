// Builds the package: `ad4m-identity-core` (feature `wasm`) for wasm32, then wasm-bindgen for the
// `web` target into `pkg/`: an ES module (`identity.js`, default export `init`), its types and
// `identity_bg.wasm`.
//
// The wasm-bindgen CLI must be the version `Cargo.lock` pins for the `wasm-bindgen` crate (the
// two refuse each other otherwise). It is looked up at `$WASM_BINDGEN_BIN` or on `PATH`.
import { execFileSync } from "node:child_process";
import { mkdirSync, readFileSync, rmSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const root = join(dirname(fileURLToPath(import.meta.url)), "..", "..");
const run = (cmd, args, opts = {}) => execFileSync(cmd, args, { stdio: "inherit", cwd: root, ...opts });
const output = (cmd, args) => execFileSync(cmd, args, { cwd: root, encoding: "utf8" });

const locked = /name = "wasm-bindgen"\nversion = "([^"]+)"/.exec(readFileSync(join(root, "Cargo.lock"), "utf8"))?.[1];
const cli = process.env.WASM_BINDGEN_BIN || "wasm-bindgen";
let have;
try {
  have = /wasm-bindgen (\S+)/.exec(output(cli, ["--version"]))?.[1];
} catch {
  // handled below
}
if (!locked || have !== locked) {
  console.error(
    `wasm-bindgen CLI ${have ?? "not found"}, but Cargo.lock pins ${locked}. ` +
      `Install it: cargo install wasm-bindgen-cli --version ${locked} --locked (or set WASM_BINDGEN_BIN).`,
  );
  process.exit(1);
}

const target = "wasm32-unknown-unknown";
// `--crate-type cdylib` here, not in the crate's manifest: a manifest cdylib would make every
// native consumer (the executor too) link an unused shared library.
run("cargo", [
  "rustc", "-p", "ad4m-identity-core", "--lib", "--release", "--target", target,
  "--features", "wasm", "--crate-type", "cdylib",
]);
const targetDir = JSON.parse(output("cargo", ["metadata", "--format-version", "1", "--no-deps"])).target_directory;

const out = join(root, "identity-wasm", "pkg");
rmSync(out, { recursive: true, force: true });
mkdirSync(out, { recursive: true });
run(cli, [
  join(targetDir, target, "release", "ad4m_identity_core.wasm"),
  "--target", "web",
  "--typescript",
  "--out-dir", out,
  "--out-name", "identity",
]);
