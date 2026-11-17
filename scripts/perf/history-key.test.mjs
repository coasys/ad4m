// scripts/perf/history-key: the deploy key leaves ~/.ssh, and `clean` removes every trace of it
// by fixed paths, also after a `use` that failed half way.
import { test } from "node:test";
import assert from "node:assert/strict";
import { execFileSync, spawnSync } from "node:child_process";
import fs from "node:fs";
import os from "node:os";
import path from "node:path";
import { fileURLToPath } from "node:url";

const script = path.join(path.dirname(fileURLToPath(import.meta.url)), "history-key");
const hasKeygen = spawnSync("ssh-keygen", ["-?"]).error === undefined;

function setup() {
  const root = fs.mkdtempSync(path.join(os.tmpdir(), "history-key-test-"));
  const ssh = path.join(root, "ssh");
  const tmp = path.join(root, "tmp");
  fs.mkdirSync(ssh);
  fs.mkdirSync(tmp);
  const key = path.join(ssh, "id_rsa_abc123");
  const other = path.join(ssh, "id_rsa_other");
  for (const k of [key, other]) execFileSync("ssh-keygen", ["-q", "-t", "ed25519", "-N", "", "-f", k]);
  const fp = execFileSync("ssh-keygen", ["-l", "-E", "sha256", "-f", key], { encoding: "utf8" }).split(" ")[1];
  // `${key}_backup`: another key whose name starts with this one's; its line must stay.
  fs.writeFileSync(path.join(ssh, "config"), `Host !github.com *\n  IdentityFile ${key}\n  IdentityFile ${key}_backup\n  IdentityFile ${other}\nHost example\n  User git\n`);
  const env = { ...process.env, PERF_SSH_DIR: ssh, TMPDIR: tmp, CIRCLE_BUILD_NUM: "4242" };
  const run = (...args) => spawnSync("bash", [script, ...args], { env, encoding: "utf8" });
  // The config's lines, trimmed: a line is kept or dropped whole.
  const config = () => fs.readFileSync(path.join(ssh, "config"), "utf8").split("\n").map((l) => l.trim());
  return { root, key, other, fp, tmp, run, config, copy: path.join(tmp, "ad4m-perf-key-4242") };
}

test("use moves the key to the build's copy and drops its config line; clean removes the copy", { skip: !hasKeygen && "no ssh-keygen" }, (t) => {
  const s = setup();
  t.after(() => fs.rmSync(s.root, { recursive: true, force: true }));
  const u = s.run("use", s.fp);
  assert.equal(u.status, 0, u.stderr);
  assert.equal(u.stdout, `export PERF_HISTORY_SSH_KEY="${s.copy}/key"\n`);
  assert.ok(fs.existsSync(path.join(s.copy, "key")));
  assert.equal(fs.statSync(path.join(s.copy, "key")).mode & 0o777, 0o600);
  assert.ok(!fs.existsSync(s.key) && !fs.existsSync(`${s.key}.pub`));
  assert.ok(fs.existsSync(s.other), "another key stays");
  assert.ok(!s.config().includes(`IdentityFile ${s.key}`));
  assert.ok(s.config().includes(`IdentityFile ${s.key}_backup`), "a longer name that starts with the key's stays");
  assert.ok(s.config().includes(`IdentityFile ${s.other}`) && s.config().includes("Host example"));

  const c = s.run("clean", s.fp);
  assert.equal(c.status, 0, c.stderr);
  assert.ok(!fs.existsSync(s.copy));
  assert.ok(fs.existsSync(s.other));
});

test("clean after a use that never ran removes the key from ~/.ssh and its config line", { skip: !hasKeygen && "no ssh-keygen" }, (t) => {
  const s = setup();
  t.after(() => fs.rmSync(s.root, { recursive: true, force: true }));
  // A use that failed after its copy but before the export: a copy on the fixed path.
  fs.mkdirSync(s.copy);
  fs.copyFileSync(s.key, path.join(s.copy, "key"));
  const c = s.run("clean", s.fp);
  assert.equal(c.status, 0, c.stderr);
  assert.ok(!fs.existsSync(s.key) && !fs.existsSync(s.copy));
  assert.ok(!s.config().includes(`IdentityFile ${s.key}`) && s.config().includes(`IdentityFile ${s.key}_backup`));
  assert.ok(s.config().includes(`IdentityFile ${s.other}`));
});

test("use with no matching key exports nothing and succeeds", { skip: !hasKeygen && "no ssh-keygen" }, (t) => {
  const s = setup();
  t.after(() => fs.rmSync(s.root, { recursive: true, force: true }));
  const u = s.run("use", "SHA256:nosuchkey");
  assert.equal(u.status, 0, u.stderr);
  assert.equal(u.stdout, "");
  assert.match(u.stderr, /no key file/);
  assert.ok(fs.existsSync(s.key));
});

test("use whose key another job moved away first exports nothing and succeeds", { skip: !hasKeygen && "no ssh-keygen" }, (t) => {
  const s = setup();
  t.after(() => fs.rmSync(s.root, { recursive: true, force: true }));
  // The other job's `forget` landing between this job's listing and its copy: a cp that finds
  // no source.
  const bin = path.join(s.root, "bin");
  fs.mkdirSync(bin);
  fs.writeFileSync(path.join(bin, "cp"), "#!/bin/sh\necho \"cp: cannot stat '$1': No such file or directory\" >&2\nexit 1\n", { mode: 0o755 });
  const u = spawnSync("bash", [script, "use", s.fp], {
    env: { ...process.env, PERF_SSH_DIR: path.dirname(s.key), TMPDIR: s.tmp, CIRCLE_BUILD_NUM: "4242", PATH: `${bin}:${process.env.PATH}` },
    encoding: "utf8",
  });
  assert.equal(u.status, 0, u.stderr);
  assert.equal(u.stdout, "", "no key exported");
  assert.match(u.stderr, /went away before it was copied/);
  assert.ok(!fs.existsSync(s.copy), "no empty copy dir left");
});
