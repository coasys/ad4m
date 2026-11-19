#!/usr/bin/env python3
"""PTY smoke test for `ad4m tui`'s Coasys space screen (#1779).

Starts its own executor (temporary data directory, free loopback port, no
admin credential), seeds one perspective with a call, then drives `ad4m tui`
in a pseudo-terminal: 2 (perspectives), o (open Coasys), a resize, F10
(back), o again, Ctrl-C (quit). It saves every byte the TUI wrote to
`<output>/terminal.ansi` and checks that the terminal modes are restored.

It fails on a missing binary or a timeout. It never reads or prints a token:
the executor runs with --insecure-no-admin-credential and the TUI gets an
empty token directory.

usage: tui_space_pty.py --ad4m target/debug/ad4m --executor /path/ad4m-executor
                        --output target/1779-slice3 [--seed path/to/bootstrapSeed.json]

The default seed is we-ad4m's test seed (no network bootstrap), from the
coasys/we checkout cargo made for the pinned rev.
"""
import argparse
import asyncio
import fcntl
import glob
import json
import os
import pty
import re
import select
import signal
import socket
import struct
import subprocess
import sys
import tempfile
import termios
import time
import urllib.parse

ROOT = os.path.abspath(os.path.join(os.path.dirname(__file__), "..", ".."))
WE_REV = "aebab9a2e0532000bac8458585a55fa080c0333f"
STEP_TIMEOUT = 30.0
SPACE = "slice3-pty"
CALL = "slice3-pty-call"
# The smallest WE-shaped CollectionBlock, as we-ad4m's testkit seeds it.
COLLECTION_SHACL = json.dumps({
    "target_class": "we://CollectionBlock",
    "constructor_actions": [
        {"action": "addLink", "source": "this", "predicate": "we://kind", "target": "placeholder"}
    ],
    "properties": [
        {"path": "we://kind", "name": "kind", "min_count": 1, "max_count": 1,
         "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "we://kind", "target": "value"}]},
        {"path": "we://title", "name": "title", "min_count": 0, "max_count": 1,
         "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "we://title", "target": "value"}]},
    ],
})
ANSI = re.compile(rb"\x1b\[[0-?]*[ -/]*[@-~]|\x1b[()][0-9A-Za-z]|\x1b[=>78]")
F10 = b"\x1b[21~"
CTRL_C = b"\x03"


def fail(message):
    print(f"FAIL: {message}", file=sys.stderr)
    sys.exit(1)


def free_port():
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def literal(text):
    return "literal:string:" + urllib.parse.quote(text, safe="")


async def rpc(url, calls, check=True):
    """Run `calls` ([(type, params)]) in order on one socket; return results.
    With `check`, an error reply fails the run."""
    import websockets

    results = []
    async with websockets.connect(url, max_size=2**26) as ws:
        for n, (kind, params) in enumerate(calls):
            await ws.send(json.dumps({"id": f"pty-{n}", "type": kind, "params": params}))
            while True:
                frame = json.loads(await asyncio.wait_for(ws.recv(), STEP_TIMEOUT))
                if frame.get("id") == f"pty-{n}":
                    break
            if frame.get("error") and check:
                fail(f"{kind}: {frame['error']}")
            results.append(frame.get("result"))
    return results


def wait_executor(ws_url, deadline):
    while True:
        try:
            # Any reply means the socket is up; before the agent exists the
            # executor answers with an error.
            asyncio.run(rpc(ws_url, [("runtime.info", {})], check=False))
            return
        except SystemExit:
            raise
        except Exception as error:  # not up yet
            if time.time() > deadline:
                fail(f"executor did not come up: {error}")
            time.sleep(0.5)


def seed(ws_url):
    (status,) = asyncio.run(rpc(ws_url, [("agent.generate", {"passphrase": "passphrase"})]))
    if not status or not status.get("isUnlocked"):
        fail(f"agent.generate: {status}")
    (handle,) = asyncio.run(rpc(ws_url, [("perspective.create", {"name": SPACE})]))
    uuid = handle["uuid"]
    base = f"pty://call/{os.getpid()}"
    asyncio.run(rpc(ws_url, [
        ("perspective.addSdna", {"uuid": uuid, "name": "CollectionBlock", "sdnaCode": "",
                                 "sdnaType": "subject_class", "shaclJson": COLLECTION_SHACL}),
        ("perspective.addLink", {"uuid": uuid, "link": {
            "source": base, "predicate": "we://kind", "target": literal("call")}}),
        ("perspective.addLink", {"uuid": uuid, "link": {
            "source": base, "predicate": "we://title", "target": literal(CALL)}}),
    ]))
    return uuid


def set_size(fd, rows, cols):
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))


class Tty:
    def __init__(self, argv, env, rows, cols, log):
        self.master, self.slave = pty.openpty()
        set_size(self.slave, rows, cols)
        self.before = termios.tcgetattr(self.slave)
        self.proc = subprocess.Popen(
            argv, stdin=self.slave, stdout=self.slave, stderr=self.slave, env=env,
            start_new_session=True,
            preexec_fn=lambda: fcntl.ioctl(0, termios.TIOCSCTTY, 0),
        )
        self.log = log
        self.raw = b""
        self.mark = 0

    def pump(self, timeout):
        ready, _, _ = select.select([self.master], [], [], timeout)
        if not ready:
            return
        try:
            data = os.read(self.master, 65536)
        except OSError:
            return
        self.raw += data
        self.log.write(data)
        self.log.flush()

    def text(self, since=0):
        return ANSI.sub(b"", self.raw[since:]).decode("utf-8", "replace")

    def wait_for(self, needle, what):
        deadline = time.time() + STEP_TIMEOUT
        while needle not in self.text(self.mark):
            if self.proc.poll() is not None:
                fail(f"ad4m tui exited ({self.proc.returncode}) waiting for {what}")
            if time.time() > deadline:
                fail(f"timed out waiting for {what} ({needle!r})")
            self.pump(0.1)
        print(f"ok: {what}")

    def send(self, data, then=None):
        self.mark = len(self.raw)
        os.write(self.master, data)

    def resize(self, rows, cols):
        self.mark = len(self.raw)
        set_size(self.slave, rows, cols)
        self.proc.send_signal(signal.SIGWINCH)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--ad4m", required=True)
    parser.add_argument("--executor", required=True)
    parser.add_argument("--output", required=True)
    parser.add_argument("--seed")
    args = parser.parse_args()
    if not args.seed:
        home = os.environ.get("CARGO_HOME", os.path.expanduser("~/.cargo"))
        found = glob.glob(os.path.join(home, "git", "checkouts", "we-*", WE_REV[:7] + "*",
                                       "crates", "renderer", "we-ad4m", "testdata",
                                       "bootstrapSeed.json"))
        if not found:
            fail(f"no --seed and no we-ad4m test seed for {WE_REV[:7]} under {home}; "
                 "build the CLI first or pass --seed")
        args.seed = found[0]
    for path in (args.ad4m, args.executor, args.seed):
        if not os.path.isfile(path):
            fail(f"missing file: {path}")
    os.makedirs(args.output, exist_ok=True)

    data = tempfile.TemporaryDirectory(prefix="tui-space-pty-")
    tokens = tempfile.TemporaryDirectory(prefix="tui-space-tokens-")
    port = free_port()
    url = f"http://127.0.0.1:{port}"
    ws_url = f"ws://127.0.0.1:{port}/api/v1/ws"
    init = subprocess.run(
        [args.executor, "init", "--data-path", data.name, "--network-bootstrap-seed", args.seed],
        stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=120,
    )
    if init.returncode != 0:
        fail("ad4m-executor init failed")
    executor = subprocess.Popen(
        [args.executor, "run", "--insecure-no-admin-credential", "--app-data-path", data.name,
         "--port", str(port), "--run-holochain", "false", "--language-language-only", "true",
         "--run-dapp-server", "false"],
        stdout=subprocess.DEVNULL, stderr=open(os.path.join(args.output, "executor.log"), "w"),
        env={**os.environ, "AD4M_EXIT_WITH_PARENT": "true"},
    )
    tty = None
    try:
        wait_executor(ws_url, time.time() + 120)
        uuid = seed(ws_url)
        print(f"ok: seeded {SPACE} {uuid}")
        env = {k: v for k, v in os.environ.items() if not k.startswith("AD4M_")}
        env.update({"TERM": "xterm-256color", "AD4M_TOKEN_DIR": tokens.name})
        with open(os.path.join(args.output, "terminal.ansi"), "wb") as log:
            tty = Tty([args.ad4m, "--executor-url", url, "tui"], env, 40, 120, log)
            tty.wait_for("Executor status", "status screen")
            tty.send(b"2")
            tty.wait_for(SPACE, "perspective row")
            tty.send(b"o")
            tty.wait_for("Coasys (read-only)", "Coasys screen")
            tty.wait_for(f"/space/{uuid}", "space route")
            tty.wait_for(CALL, "seeded call in the app")
            tty.resize(30, 100)
            tty.wait_for("Coasys (read-only)", "Coasys screen after resize")
            tty.send(F10)
            tty.wait_for("Perspectives and neighbourhoods", "back to perspectives (F10)")
            tty.send(b"o")
            tty.wait_for("Coasys (read-only)", "Coasys screen again")
            tty.send(CTRL_C)
            deadline = time.time() + STEP_TIMEOUT
            while tty.proc.poll() is None:
                if time.time() > deadline:
                    tty.proc.kill()
                    fail("Ctrl-C did not exit ad4m tui")
                tty.pump(0.1)
            tty.pump(0.2)
        if tty.proc.returncode != 0:
            fail(f"ad4m tui exited {tty.proc.returncode}")
        print("ok: Ctrl-C exited 0")

        # Terminal modes: raw mode off (same termios as before), the
        # alternate screen left, the cursor shown, mouse capture off.
        after = termios.tcgetattr(tty.slave)
        if after != tty.before:
            fail(f"termios not restored: {tty.before} -> {after}")
        tail = tty.raw[tty.raw.rfind(b"\x1b[?1049h"):]
        for name, seq in [("leave alternate screen", b"\x1b[?1049l"),
                          ("show cursor", b"\x1b[?25h"),
                          ("mouse capture off", b"\x1b[?1000l")]:
            if seq not in tail:
                fail(f"{name} ({seq!r}) not written after the last alternate-screen entry")
        if b"token=" in tty.raw:
            fail("a token-bearing URL was written to the terminal")
        print("ok: terminal modes restored")
        with open(os.path.join(args.output, "pty-result.json"), "w") as out:
            json.dump({"space": SPACE, "uuid": uuid, "call": CALL, "bytes": len(tty.raw),
                       "sizes": ["120x40", "100x30"], "exit": tty.proc.returncode,
                       "termiosRestored": True}, out, indent=2)
        print(f"PASS: {os.path.join(args.output, 'terminal.ansi')}")
    finally:
        if tty and tty.proc.poll() is None:
            tty.proc.kill()
        executor.terminate()
        try:
            executor.wait(10)
        except subprocess.TimeoutExpired:
            executor.kill()


if __name__ == "__main__":
    main()
