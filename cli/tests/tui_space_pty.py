#!/usr/bin/env python3
"""PTY acceptance for ad4m #1779 slice 3: `o` opens the Coasys view, F10
returns, Ctrl-C exits, a resize repaints.

The script starts its own `ad4m-executor` (`init` + `run` on a temporary
data dir, no admin credential — a loopback caller is the operator), seeds
one perspective holding a `CollectionBlock` call over a minimal stdlib
WebSocket client, then drives `ad4m --executor-url <url> --no-capability
tui` under a PTY:

    '2'   → the perspectives screen, the seeded space selected
    'o'   → the live Coasys view (the seeded call renders)
    F10   → back to the perspectives screen
    resize → the view repaints at the new size
    'o'   → the space again
    Ctrl-C → the process exits 0 and leaves the PTY in cooked mode

Everything the child prints is saved as <output>/terminal.ansi. The
script FAILS — never skips — on a missing binary, a startup failure or a
timeout. It captures no token: there is none to capture.

Usage:
    python3 cli/tests/tui_space_pty.py \
        --ad4m target/debug/ad4m \
        --executor /path/to/ad4m-executor \
        --output target/1779-slice3
"""

from __future__ import annotations

import argparse
import base64
import errno
import fcntl
import hashlib
import json
import os
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

# The executor's `init --network-bootstrap-seed`: the same public seed
# W's testkit passes (crates/renderer/we-ad4m/testdata/bootstrapSeed.json).
BOOTSTRAP_SEED = {
    "trustedAgents": ["did:key:zQ3shkkuZLvqeFgHdgZgFMUx8VGkgVWsLA83w2oekhZxoCW2n"],
    "knownLinkLanguages": ["QmzSYwdbpzDfaBt28VZp5LYpy1Daq4agD7z8GTBVpJyyr3MPhTy"],
    "directMessageLanguage": "",
    "agentLanguage": "QmzSYwdeHcdyNWBJPVBvbgWNQh3S9MjDfgQiP2agUMJAwfyrHpC",
    "perspectiveLanguage": "QmzSYwdZ5zUZFVJdWsyg9PjjqyM72mjrxjaaSTX9rXtHmC3S5fi",
    "neighbourhoodLanguage": "QmzSYwdpVQ6RKTJbzFEZ8mfMdn5SYvd7immxudDVG92uxgAJ8nY",
    "languageLanguageBundle": "",
}

# `we://CollectionBlock` with `kind` + `title`, as W's testkit registers it
# (`COLLECTION_SDNA`): the Coasys sidebar groups calls by `we://kind`.
COLLECTION_SDNA = r"""{
  "target_class": "we://CollectionBlock",
  "constructor_actions": [
    {"action": "addLink", "source": "this", "predicate": "we://kind", "target": "placeholder"}
  ],
  "properties": [
    {"path": "we://kind", "name": "kind", "min_count": 1, "max_count": 1, "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "we://kind", "target": "value"}]},
    {"path": "we://title", "name": "title", "min_count": 0, "max_count": 1, "setter": [{"action": "setSingleTarget", "source": "this", "predicate": "we://title", "target": "value"}]}
  ]
}"""

SPACE_NAME = "slice3-pty"
CALL_TITLE = "slice3-pty-call"

# Transition bounds — far under the ~2 s a cursor-querying clear stalls for
# on a terminal that does not answer ESC[6n (this PTY does not). 'o' also
# assembles the space; F10 only repaints the admin screen.
OPEN_BOUND = 1.5
BACK_BOUND = 1.0


def literal(s: str) -> str:
    """`Literal::from_string(s).to_url()` — `literal:string:<urlencoded>`."""
    from urllib.parse import quote

    return "literal:string:" + quote(s, safe="")


class RpcError(Exception):
    pass


class Ws:
    """The smallest correct WS client: RFC6455 upgrade + masked text
    frames. Speaks the executor's `{"id","type","params"}` RPC."""

    def __init__(self, url: str, timeout: float = 10.0):
        base = url.split("//", 1)[1]
        self.host, port_s = base.split(":", 1)
        self.sock = socket.create_connection((self.host, int(port_s)), timeout)
        self.buf = b""
        self.n = 0
        key = base64.b64encode(os.urandom(16)).decode()
        req = (
            f"GET /api/v1/ws HTTP/1.1\r\nHost: {base}\r\n"
            "Upgrade: websocket\r\nConnection: Upgrade\r\n"
            f"Sec-WebSocket-Key: {key}\r\nSec-WebSocket-Version: 13\r\n\r\n"
        )
        self.sock.sendall(req.encode())
        while b"\r\n\r\n" not in self.buf:
            chunk = self.sock.recv(4096)
            if not chunk:
                raise ConnectionError("executor closed the upgrade")
            self.buf += chunk
        head, _, self.buf = self.buf.partition(b"\r\n\r\n")
        status = head.split(b"\r\n", 1)[0]
        if b" 101 " not in status:
            raise ConnectionError(f"WS upgrade refused: {status!r}")
        accept = base64.b64encode(
            hashlib.sha1((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").encode()).digest()
        ).decode()
        if accept.encode() not in head:
            raise ConnectionError("bad Sec-WebSocket-Accept")

    def _read(self, n: int) -> bytes:
        while len(self.buf) < n:
            chunk = self.sock.recv(65536)
            if not chunk:
                raise ConnectionError("executor closed the socket")
            self.buf += chunk
        out, self.buf = self.buf[:n], self.buf[n:]
        return out

    def _send(self, payload: bytes) -> None:
        mask = os.urandom(4)
        ln = len(payload)
        if ln < 126:
            hdr = bytes([0x81, 0x80 | ln])
        elif ln < 65536:
            hdr = bytes([0x81, 0x80 | 126]) + struct.pack(">H", ln)
        else:
            hdr = bytes([0x81, 0x80 | 127]) + struct.pack(">Q", ln)
        self.sock.sendall(hdr + mask + bytes(b ^ mask[i % 4] for i, b in enumerate(payload)))

    def _recv(self) -> tuple[int, bytes]:
        hdr = self._read(2)
        opcode, ln = hdr[0] & 0x0F, hdr[1] & 0x7F
        if ln == 126:
            ln = struct.unpack(">H", self._read(2))[0]
        elif ln == 127:
            ln = struct.unpack(">Q", self._read(8))[0]
        mask = self._read(4) if hdr[1] & 0x80 else b""
        data = self._read(ln)
        if mask:
            data = bytes(b ^ mask[i % 4] for i, b in enumerate(data))
        return opcode, data

    def call(self, method: str, params: dict, timeout: float = 30.0) -> dict:
        self.n += 1
        rid = str(self.n)
        self._send(json.dumps({"id": rid, "type": method, "params": params}).encode())
        deadline = time.monotonic() + timeout
        while True:
            remaining = deadline - time.monotonic()
            if remaining <= 0:
                raise TimeoutError(f"{method}: executor did not reply in {timeout}s")
            self.sock.settimeout(min(remaining, 5.0))
            op, data = self._recv()
            if op == 8:
                raise ConnectionError("executor sent close")
            if op == 9:  # ping → pong
                mask = os.urandom(4)
                self.sock.sendall(
                    bytes([0x8A, 0x80 | len(data)])
                    + mask
                    + bytes(b ^ mask[i % 4] for i, b in enumerate(data))
                )
                continue
            if op != 1:
                continue
            msg = json.loads(data)
            if str(msg.get("id")) != rid:
                continue
            if msg.get("error"):
                raise RpcError(f"{method}: {msg['error']}")
            return msg.get("result")

    def close(self) -> None:
        try:
            self.sock.close()
        except OSError:
            pass


def free_port() -> int:
    with socket.socket() as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def start_executor(binary: str, work: str, log_path: str) -> tuple[subprocess.Popen, str]:
    data = os.path.join(work, "executor-data")
    os.makedirs(data)
    seed = os.path.join(work, "bootstrap-seed.json")
    with open(seed, "w") as f:
        json.dump(BOOTSTRAP_SEED, f)
    try:
        init = subprocess.run(
            [binary, "init", "--data-path", data, "--network-bootstrap-seed", seed],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            timeout=60.0,
        )
    except subprocess.TimeoutExpired:
        raise RuntimeError("ad4m-executor init did not finish in 60 s") from None
    if init.returncode != 0:
        raise RuntimeError(f"ad4m-executor init exited {init.returncode}")
    port = free_port()
    env = dict(os.environ, AD4M_EXIT_WITH_PARENT="true")
    log = open(log_path, "w")
    proc = subprocess.Popen(
        [
            binary,
            "run",
            "--insecure-no-admin-credential",
            "--app-data-path",
            data,
            "--port",
            str(port),
            "--run-holochain",
            "false",
            "--language-language-only",
            "true",
            "--run-dapp-server",
            "false",
        ],
        env=env,
        stdout=subprocess.DEVNULL,
        stderr=log,
    )
    return proc, f"http://127.0.0.1:{port}"


def wait_executor(url: str, proc: subprocess.Popen, timeout: float = 120.0) -> Ws:
    deadline = time.monotonic() + timeout
    last = None
    while time.monotonic() < deadline:
        if proc.poll() is not None:
            raise RuntimeError(f"executor exited {proc.returncode} during startup")
        try:
            return Ws(url, timeout=5.0)
        except (OSError, ConnectionError) as e:
            last = e
            time.sleep(0.5)
    raise TimeoutError(f"executor did not accept a socket in {timeout}s: {last}")


def seed(ws: Ws) -> str:
    """Agent + one space with a `call` collection. Returns the uuid."""
    ws.call("agent.generate", {"passphrase": "pty"}, timeout=60)
    handle = ws.call("perspective.create", {"name": SPACE_NAME})
    uuid = handle["uuid"]
    ws.call(
        "perspective.addSdna",
        {
            "uuid": uuid,
            "name": "CollectionBlock",
            "sdnaCode": "",
            "sdnaType": "subject_class",
            "shaclJson": COLLECTION_SDNA,
        },
    )
    for predicate, target in [
        ("we://kind", literal("call")),
        ("we://title", literal(CALL_TITLE)),
    ]:
        ws.call(
            "perspective.addLink",
            {
                "uuid": uuid,
                "link": {"source": "pty://call/1", "predicate": predicate, "target": target},
            },
        )
    ws.close()
    return uuid


def set_winsize(fd: int, cols: int, rows: int) -> None:
    fcntl.ioctl(fd, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))


class Screen:
    """A tiny VT100 grid: just enough terminal to say what the child
    actually painted — absolute cursor positioning and relative moves,
    erase-display/line, charset/private/OSC sequences ignored, ESC[6n
    counted. Searching this grid cannot be satisfied by text the child
    left behind, the way a whole-transcript search could."""

    def __init__(self, cols: int, rows: int) -> None:
        self.cols, self.rows = cols, rows
        self.grid = [[" "] * cols for _ in range(rows)]
        self.r = self.c = 0
        self.saved = (0, 0)
        self.buf = b""
        self.dsr_queries = 0
        # The largest (row, col) a positioning sequence asked for since
        # the last clear — a repaint at a stale size overshoots the
        # resized grid and shows here.
        self.cup_hi = (0, 0)
        self.clears_seen = 0

    def resize(self, cols: int, rows: int) -> None:
        # ratatui resets both buffers on a backend resize, so the next
        # frame rewrites every non-empty cell — a fresh grid mirrors that.
        self.cols, self.rows = cols, rows
        self.grid = [[" "] * cols for _ in range(rows)]
        self.r = self.c = 0
        self.cup_hi = (0, 0)

    def clear(self) -> None:
        self.grid = [[" "] * self.cols for _ in range(self.rows)]
        self.r = self.c = 0
        self.cup_hi = (0, 0)
        self.clears_seen += 1

    def text(self) -> str:
        return "\n".join("".join(row) for row in self.grid)

    def feed(self, data: bytes) -> None:
        self.buf += data
        b, i, n = self.buf, 0, len(self.buf)
        while i < n:
            ch = b[i]
            if ch == 0x1B:
                step = self._escape(b[i:])
                if step is None:
                    break  # incomplete escape — keep it buffered
                i += step
            elif ch < 0x20:
                self._control(ch)
                i += 1
            elif ch < 0x80:
                self._put(chr(ch))
                i += 1
            else:
                ln = 2 if ch < 0xE0 else 3 if ch < 0xF0 else 4
                if i + ln > n:
                    break  # incomplete UTF-8 — keep it buffered
                try:
                    self._put(b[i : i + ln].decode("utf-8"))
                except UnicodeDecodeError:
                    i += 1
                    continue
                i += ln
        self.buf = self.buf[i:]

    def _put(self, ch: str) -> None:
        if self.c >= self.cols:
            self.c = 0
            self.r += 1
        if self.r >= self.rows:
            self.grid.pop(0)
            self.grid.append([" "] * self.cols)
            self.r = self.rows - 1
        self.grid[self.r][self.c] = ch
        self.c += 1

    def _control(self, ch: int) -> None:
        if ch == 0x0D:
            self.c = 0
        elif ch == 0x0A:  # LF — an index: down, same column
            self.r += 1
            if self.r >= self.rows:
                self.grid.pop(0)
                self.grid.append([" "] * self.cols)
                self.r = self.rows - 1
        elif ch == 0x08:
            self.c = max(0, self.c - 1)
        elif ch == 0x09:
            self.c = min(self.cols - 1, (self.c // 8 + 1) * 8)

    def _escape(self, seq: bytes) -> int | None:
        """Bytes the escape at seq[0] consumed, or None while incomplete."""
        if len(seq) < 2:
            return None
        kind = seq[1]
        if kind == ord("["):
            m = re.match(rb"\x1b\[([0-9;?]*)([@-~])", seq)
            if not m:
                return None if re.fullmatch(rb"\x1b\[[0-9;?]*", seq) else 2
            self._csi(m.group(1).decode(), chr(m.group(2)[0]))
            return m.end()
        if kind == ord("]"):  # OSC — runs to BEL or ST
            m = re.search(rb"\x07|\x1b\\", seq[2:])
            return None if not m else 2 + m.end()
        if kind in (ord("("), ord(")"), ord("*"), ord("+"), ord("#")):
            return None if len(seq) < 3 else 3
        if kind == ord("c"):
            self.clear()
        elif kind == ord("7"):
            self.saved = (self.r, self.c)
        elif kind == ord("8"):
            self.r, self.c = self.saved
        elif kind == ord("M"):  # reverse index
            if self.r == 0:
                self.grid.pop()
                self.grid.insert(0, [" "] * self.cols)
            else:
                self.r -= 1
        return 2  # every remaining escape is the two-byte kind

    def _csi(self, params: str, final: str) -> None:
        if final in "Hf":
            parts = params.split(";")
            row = int(parts[0] or "1")
            col = int(parts[1] or "1") if len(parts) > 1 else 1
            self.cup_hi = (max(self.cup_hi[0], row), max(self.cup_hi[1], col))
            self.r = max(0, min(self.rows - 1, row - 1))
            self.c = max(0, min(self.cols - 1, col - 1))
        elif final == "n" and params == "6":
            self.dsr_queries += 1
        elif final in "hl" and params == "?1049":
            self.clear()
        elif final == "J":
            self._erase_display(params or "0")
        elif final == "K":
            self._erase_line(params or "0")
        elif final in "ABCDEFGd`":
            n = int((params.split(";")[0] or "1"))
            if final == "A":
                self.r = max(0, self.r - n)
            elif final == "B":
                self.r = min(self.rows - 1, self.r + n)
            elif final == "C":
                self.c = min(self.cols - 1, self.c + n)
            elif final == "D":
                self.c = max(0, self.c - n)
            elif final == "E":
                self.r, self.c = min(self.rows - 1, self.r + n), 0
            elif final == "F":
                self.r, self.c = max(0, self.r - n), 0
            elif final in "G`":
                self.c = min(self.cols - 1, n - 1)
            elif final == "d":
                self.r = min(self.rows - 1, n - 1)
        elif final == "s":
            self.saved = (self.r, self.c)
        elif final == "u":
            self.r, self.c = self.saved
        # SGR, private modes and the rest change nothing the asserts read.

    def _erase_display(self, mode: str) -> None:
        if mode in ("2", "3"):
            self.clear()
        elif mode == "1":
            for y in range(self.r):
                self.grid[y] = [" "] * self.cols
            for x in range(self.c + 1):
                self.grid[self.r][x] = " "
        else:
            for x in range(self.c, self.cols):
                self.grid[self.r][x] = " "
            for y in range(self.r + 1, self.rows):
                self.grid[y] = [" "] * self.cols

    def _erase_line(self, mode: str) -> None:
        if mode == "2":
            self.grid[self.r] = [" "] * self.cols
        elif mode == "1":
            for x in range(self.c + 1):
                self.grid[self.r][x] = " "
        else:
            for x in range(self.c, self.cols):
                self.grid[self.r][x] = " "


def drive_pty(ad4m: str, url: str, uuid: str, out_ansi: str, timeout: float = 90.0) -> None:
    """The keystroke path: 2 → o → F10 → resize → o → Ctrl-C.

    Every step asserts on an emulated screen fed the child's output, so a
    step cannot be satisfied by text left over from an earlier screen.
    The slave fd stays open through the exit check — raw mode restored is
    proven by the pty's own termios, not just restore sequences.
    """
    import pty as pty_mod

    master, slave = pty_mod.openpty()
    set_winsize(slave, 120, 40)
    saved_termios = termios.tcgetattr(slave)
    child = subprocess.Popen(
        [ad4m, "--executor-url", url, "--no-capability", "tui"],
        stdin=slave,
        stdout=slave,
        stderr=slave,
        close_fds=True,
        start_new_session=True,
    )
    screen = Screen(120, 40)
    log = open(out_ansi, "wb")
    deadline = time.monotonic() + timeout

    def check_deadline() -> None:
        if time.monotonic() > deadline:
            raise TimeoutError(f"pty drive exceeded its {timeout}s bound")

    # This PTY never answers a device-status report. A transition clear
    # that queries the cursor (ESC[6n) blocks ~2 s here, so the 'o' and
    # F10 transitions below are time-bounded — a regression shows up as a
    # stall — and the run asserts the child never emitted the query.
    def pump(want: float = 0.25) -> None:
        end = time.monotonic() + want
        while time.monotonic() < end:
            check_deadline()
            r, _, _ = select.select([master], [], [], 0.05)
            if not r:
                continue
            try:
                chunk = os.read(master, 65536)
            except OSError as e:
                if e.errno == errno.EIO:
                    return  # child closed its side
                raise
            if not chunk:
                return
            log.write(chunk)
            log.flush()
            screen.feed(chunk)

    def send(keys: bytes) -> None:
        os.write(master, keys)
        pump()

    def wait_screen(needle: str, within: float, what: str) -> float:
        start = time.monotonic()
        while time.monotonic() < start + within:
            check_deadline()
            pump(0.2)
            if needle in screen.text():
                return time.monotonic() - start
            if child.poll() is not None:
                raise RuntimeError(
                    f"ad4m exited {child.returncode} while waiting for {what}"
                )
        raise TimeoutError(f"timed out waiting for {what} ({needle!r} not on screen)")

    times: dict[str, float] = {}
    try:
        wait_screen("Executor status", 20.0, "the admin status screen")
        send(b"2")
        wait_screen(
            "Perspectives and neighbourhoods", 10.0, "the perspectives screen"
        )
        wait_screen(SPACE_NAME, 30.0, "the seeded space on the perspectives screen")
        # 'o' opens the Coasys view of the selected (seeded) row. The route
        # line — not the row's bare uuid — is what proves the view mounted.
        # The transition is time-bounded: a clear that queries the cursor
        # would stall ~2 s on this PTY, which never answers ESC[6n.
        route = f"/space/{uuid}"
        send(b"o")
        times["open"] = wait_screen(route, OPEN_BOUND, "the Coasys view (uuid route)")
        wait_screen("Coasys", 5.0, "the Coasys header")
        wait_screen(CALL_TITLE, 30.0, "the seeded call in the Coasys view")
        # F10 returns to the admin screen — bounded the same way, and the
        # screen must really change: the perspectives header is back, the
        # seeded row is listed, and the space route is gone.
        send(b"\x1b[21~")
        times["back"] = wait_screen(
            "Perspectives and neighbourhoods",
            BACK_BOUND,
            "the admin screen after F10",
        )
        if route in screen.text():
            raise RuntimeError("F10 left the space route on screen")
        if SPACE_NAME not in screen.text():
            raise RuntimeError("the seeded row did not come back after F10")
        # Reopen: the route must appear on a screen that was just admin.
        send(b"o")
        times["reopen"] = wait_screen(route, OPEN_BOUND, "the Coasys view again")
        # A resize while the space view is open repaints at the new size:
        # on a horizontal shrink ratatui clears (ESC[2J) before the new
        # frame, so the repaint is proven by the clear, the route on the
        # fresh grid, and every border corner at 100x30.
        set_winsize(slave, 100, 30)
        screen.resize(100, 30)
        clears = screen.clears_seen
        send(b"")
        clear_deadline = time.monotonic() + 10.0
        while screen.clears_seen == clears:
            check_deadline()
            pump(0.2)
            if child.poll() is not None:
                raise RuntimeError(f"ad4m exited {child.returncode} during resize")
            if time.monotonic() > clear_deadline:
                raise TimeoutError("no clear — the child never repainted at 100x30")
        wait_screen(route, 10.0, "the space view repainted at 100x30")
        corners = (
            screen.grid[0][0],
            screen.grid[0][-1],
            screen.grid[-1][0],
            screen.grid[-1][-1],
        )
        if corners != ("┌", "┐", "└", "┘"):
            raise RuntimeError(f"no repaint at 100x30 — corners are {corners}")
        if screen.cup_hi[0] > 30 or screen.cup_hi[1] > 100:
            raise RuntimeError(
                f"the repaint still targets the old size — cursor reached {screen.cup_hi}"
            )
        # Ctrl-C exits normally.
        send(b"\x03")
        exit_deadline = time.monotonic() + 10.0
        while child.poll() is None and time.monotonic() < exit_deadline:
            pump(0.2)
        if child.poll() is None:
            child.kill()
            raise TimeoutError("Ctrl-C did not exit the TUI in 10s")
        pump(0.5)
        if child.returncode != 0:
            raise RuntimeError(f"ad4m tui exited {child.returncode}, expected 0")
        if screen.dsr_queries:
            raise RuntimeError(
                f"ad4m queried the cursor (ESC[6n) {screen.dsr_queries}x — "
                "a transition clear must not"
            )
        # Raw mode restored: the pty's own termios is exactly what it was
        # before the child ran — `stty -a` shows icanon/echo back on.
        stty = subprocess.run(
            ["stty", "-a", "-F", os.ttyname(slave)],
            capture_output=True,
            text=True,
        ).stdout
        if not (
            re.search(r"(?<!-)\bicanon\b", stty)
            and re.search(r"(?<!-)\becho\b", stty)
        ):
            raise RuntimeError(f"pty left in raw mode — stty shows: {stty!r}")
        if termios.tcgetattr(slave) != saved_termios:
            raise RuntimeError("pty termios changed across the run")
        print(
            f"  transitions: open {times['open']:.2f}s · "
            f"back {times['back']:.2f}s · reopen {times['reopen']:.2f}s"
        )
    finally:
        log.close()
        try:
            os.close(master)
        except OSError:
            pass
        os.close(slave)
        if child.poll() is None:
            child.kill()
            child.wait()


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--ad4m", required=True, help="path to the ad4m binary")
    ap.add_argument("--executor", required=True, help="path to the ad4m-executor binary")
    ap.add_argument("--output", required=True, help="artifact dir (terminal.ansi lands here)")
    args = ap.parse_args()

    for name, path in [("--ad4m", args.ad4m), ("--executor", args.executor)]:
        if not os.path.isfile(path) or not os.access(path, os.X_OK):
            print(f"FAIL: {name} binary missing or not executable: {path}", file=sys.stderr)
            return 2
    os.makedirs(args.output, exist_ok=True)

    # The executor's data dir is a TemporaryDirectory: deleted on success
    # and on failure alike, once the executor is stopped.
    with tempfile.TemporaryDirectory(prefix="ad4m-pty-") as work:
        exec_log = os.path.join(args.output, "executor.log")
        proc, url = start_executor(args.executor, work, exec_log)
        try:
            ws = wait_executor(url, proc)
            uuid = seed(ws)
            out_ansi = os.path.join(args.output, "terminal.ansi")
            drive_pty(args.ad4m, url, uuid, out_ansi)
        finally:
            if proc.poll() is None:
                proc.terminate()
                try:
                    proc.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    proc.kill()

    # The pty's restored termios was already proven inside drive_pty; the
    # capture adds the restore sequences the exit path emits — alt-screen
    # exit + cursor-show.
    with open(out_ansi, "rb") as f:
        capture = f.read()
    for seq, name in [
        (b"\x1b[?1049l", "alternate-screen exit"),
        (b"\x1b[?25h", "cursor show"),
    ]:
        if seq not in capture:
            print(f"FAIL: terminal-mode restoration sequence missing ({name})", file=sys.stderr)
            return 1
    print(f"OK: tui PTY acceptance — capture at {out_ansi}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
