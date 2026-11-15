#!/usr/bin/env python3
"""Peer for the browser tests of ad4m-client (scripts/test-wasm-client.sh).

Two listeners on 127.0.0.1:

  --control PORT   HTTP + scripted WebSocket endpoints:
      GET /count/<key>          {"accepted": n, "open": m} for <key>
      GET /cut                  drop every proxied connection (no close frame)
      WS  /hang/<key>           read the upgrade request, never answer it
      WS  /silent/<key>         answer the upgrade, then never read again
      WS  /close/<code>/<key>   answer the upgrade, then close with <code>
      WS  /echo/<key>           echo every text frame
      WS  /flood/<n>/<size>/<key>  send <n> text frames of <size> bytes
  --proxy PORT --target PORT
      a TCP proxy to the executor; connections count under key "proxy"

Standard library only. The browser test reads the counts with fetch, so
every HTTP answer allows any origin.
"""
import argparse
import asyncio
import base64
import hashlib
import json
import struct
from collections import defaultdict

GUID = b"258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
counts = defaultdict(lambda: {"accepted": 0, "open": 0})
proxied = set()


def frame(opcode, payload):
    head = bytes([0x80 | opcode])
    n = len(payload)
    if n < 126:
        head += bytes([n])
    elif n < 1 << 16:
        head += bytes([126]) + struct.pack(">H", n)
    else:
        head += bytes([127]) + struct.pack(">Q", n)
    return head + payload


async def read_frame(reader):
    b0, b1 = await reader.readexactly(2)
    n = b1 & 0x7F
    if n == 126:
        (n,) = struct.unpack(">H", await reader.readexactly(2))
    elif n == 127:
        (n,) = struct.unpack(">Q", await reader.readexactly(8))
    mask = await reader.readexactly(4) if b1 & 0x80 else b"\0\0\0\0"
    data = bytearray(await reader.readexactly(n))
    for i in range(n):
        data[i] ^= mask[i % 4]
    return b0 & 0x0F, bytes(data)


async def http_reply(writer, status, body):
    body = body.encode()
    writer.write(
        f"HTTP/1.1 {status}\r\nContent-Type: application/json\r\n"
        f"Access-Control-Allow-Origin: *\r\nContent-Length: {len(body)}\r\n"
        f"Connection: close\r\n\r\n".encode() + body
    )
    await writer.drain()
    writer.close()


async def control(reader, writer):
    head = await reader.readuntil(b"\r\n\r\n")
    lines = head.decode().split("\r\n")
    path = lines[0].split(" ")[1]
    headers = {
        k.strip().lower(): v.strip()
        for k, v in (l.split(":", 1) for l in lines[1:] if ":" in l)
    }
    parts = path.strip("/").split("/")
    if "sec-websocket-key" not in headers:
        if parts[0] == "count":
            return await http_reply(writer, "200 OK", json.dumps(counts[parts[1]]))
        if parts[0] == "cut":
            n = len(proxied)
            for w in list(proxied):
                w.transport.abort()
            return await http_reply(writer, "200 OK", json.dumps({"cut": n}))
        return await http_reply(writer, "404 Not Found", "{}")

    key = parts[-1]
    counts[key]["accepted"] += 1
    counts[key]["open"] += 1
    try:
        if parts[0] == "hang":
            await reader.read()  # until the browser gives up or closes
            return
        accept = base64.b64encode(
            hashlib.sha1(headers["sec-websocket-key"].encode() + GUID).digest()
        ).decode()
        writer.write(
            "HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\n"
            f"Connection: Upgrade\r\nSec-WebSocket-Accept: {accept}\r\n\r\n".encode()
        )
        await writer.drain()
        if parts[0] == "silent":
            # Never read past asyncio's 64 KiB stream buffer: the kernel
            # buffers fill, then the browser's.
            for _ in range(600):
                if reader.at_eof():
                    break
                await asyncio.sleep(0.2)
        elif parts[0] == "close":
            writer.write(frame(0x8, struct.pack(">H", int(parts[1])) + b"scripted"))
            await writer.drain()
            await asyncio.sleep(0.5)
        elif parts[0] == "flood":
            n, size = int(parts[1]), int(parts[2])
            for i in range(n):
                writer.write(frame(0x1, (str(i % 10) * size).encode()))
                await writer.drain()
            await echo(reader, writer)
        elif parts[0] == "echo":
            await echo(reader, writer)
    except (asyncio.IncompleteReadError, ConnectionError):
        pass
    finally:
        counts[key]["open"] -= 1
        writer.close()


async def echo(reader, writer):
    """Echo text frames; answer the browser's close frame."""
    while True:
        op, data = await read_frame(reader)
        if op == 0x8:
            writer.write(frame(0x8, data[:2]))
            await writer.drain()
            return
        if op == 0x1:
            writer.write(frame(0x1, data))
            await writer.drain()


async def proxy(target, reader, writer):
    counts["proxy"]["accepted"] += 1
    counts["proxy"]["open"] += 1
    proxied.add(writer)
    try:
        up_r, up_w = await asyncio.open_connection("127.0.0.1", target)
        proxied.add(up_w)

        async def pipe(r, w):
            try:
                while data := await r.read(65536):
                    w.write(data)
                    await w.drain()
            except ConnectionError:
                pass
            finally:
                w.close()

        await asyncio.gather(pipe(reader, up_w), pipe(up_r, writer))
        proxied.discard(up_w)
    except ConnectionError:
        pass
    finally:
        proxied.discard(writer)
        counts["proxy"]["open"] -= 1
        writer.close()


async def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--control", type=int, required=True)
    ap.add_argument("--proxy", type=int)
    ap.add_argument("--target", type=int)
    a = ap.parse_args()
    servers = [await asyncio.start_server(control, "127.0.0.1", a.control)]
    if a.proxy:
        servers.append(
            await asyncio.start_server(
                lambda r, w: proxy(a.target, r, w), "127.0.0.1", a.proxy
            )
        )
    await asyncio.gather(*(s.serve_forever() for s in servers))


asyncio.run(main())
