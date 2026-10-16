import assert from "node:assert/strict";
import { randomBytes, randomUUID } from "node:crypto";
import { get as httpGet, type ClientRequest } from "node:http";
import { connect, type AddressInfo } from "node:net";
import { test } from "node:test";
import { LogTail, MAX_LINE_LENGTH } from "../src/log-tail.js";
import { MAX_LOG_STREAMS } from "../src/operator.js";
import { authenticateAgent, createTestAgent, startTestServer, type TestServerHandle } from "./helpers.js";

const TOKEN = randomBytes(32).toString("hex");
const ORIGIN = "https://prs.ad4m.dev";
const SIGNED_IN = { authorization: `Bearer ${TOKEN}`, "x-forwarded-user": "lucksus" };

interface Logged {
  server: TestServerHandle;
  opUrl: string;
  stdout: string[];
}

/** A server with logging on (lines to `stdout` instead of the process's) and the operator listener. */
async function withLogging(fn: (h: Logged) => Promise<void>, logger = true): Promise<void> {
  const stdout: string[] = [];
  const server = await startTestServer({
    logger,
    logDestination: { write: (line: string) => stdout.push(line) },
    operator: { token: TOKEN, allowedOrigins: [ORIGIN], label: "staging" },
  });
  try {
    const opApp = server.built.operatorApp!;
    await opApp.listen({ port: 0, host: "127.0.0.1" });
    const opUrl = `http://127.0.0.1:${(opApp.server.address() as AddressInfo).port}`;
    await fn({ server, opUrl, stdout });
  } finally {
    await server.close();
  }
}

interface LogStream {
  status: number;
  contentType: string | null;
  /** Events read so far: id and parsed data. */
  events: Array<{ id: string; data: Record<string, any> }>;
  /** Reads until `predicate` holds for the events read so far, or fails after `timeoutMs`. */
  until(predicate: (events: LogStream["events"]) => boolean, timeoutMs?: number): Promise<void>;
  close(): void;
}

/** Opens `GET /api/logs` and parses its Server-Sent Events as they arrive. */
async function openLogs(opUrl: string, headers: Record<string, string> = SIGNED_IN): Promise<LogStream> {
  const abort = new AbortController();
  const res = await fetch(`${opUrl}/api/logs`, { headers, signal: abort.signal });
  const events: LogStream["events"] = [];
  const reader = res.body!.getReader();
  const decoder = new TextDecoder();
  let buffered = "";
  const read = async () => {
    const { value, done } = await reader.read();
    if (done) throw new Error("stream ended");
    buffered += decoder.decode(value, { stream: true });
    let end;
    while ((end = buffered.indexOf("\n\n")) >= 0) {
      const block = buffered.slice(0, end);
      buffered = buffered.slice(end + 2);
      const id = /^id: (\S+)$/m.exec(block);
      const data = /^data: (.*)$/m.exec(block);
      if (id && data) events.push({ id: id[1], data: JSON.parse(data[1]) });
    }
  };
  return {
    status: res.status,
    contentType: res.headers.get("content-type"),
    events,
    until: async (predicate, timeoutMs = 3000) => {
      const deadline = Date.now() + timeoutMs;
      while (!predicate(events)) {
        if (Date.now() > deadline) throw new Error(`log stream: condition not met; got ${JSON.stringify(events.map((e) => e.data.msg))}`);
        await Promise.race([read(), new Promise((r) => setTimeout(r, 100))]);
      }
    },
    close: () => abort.abort(),
  };
}

test("LogTail keeps pino's lines in order, without pid and hostname, up to its capacity", () => {
  const tail = new LogTail(3);
  tail.write(`{"level":30,"time":1,"pid":7,"hostname":"h","msg":"one","roomId":"r"}\n`);
  // Two lines in one write, and a line that is not JSON.
  tail.write(`{"level":40,"time":2,"msg":"two"}\n{"level":50,"time":3,"msg":"three"}\nplain text\n`);
  assert.deepEqual(tail.since().map((e) => e.msg), ["two", "three", "plain text"]);
  assert.deepEqual(tail.since(2).map((e) => e.seq), [3, 4]);
  const [two] = tail.since(1);
  assert.equal(two.level, 40);
  assert.equal(tail.since(3)[0].level, 30, "a line that is not JSON is info");

  const kept = new LogTail(10);
  kept.write(`{"level":30,"time":1,"pid":7,"hostname":"h","msg":"one","roomId":"r"}\n`);
  assert.deepEqual(kept.since()[0], { level: 30, time: 1, msg: "one", roomId: "r", seq: 1 });
});

test("LogTail cuts an oversized line and drops a listener that throws", () => {
  const tail = new LogTail();
  tail.write(JSON.stringify({ level: 30, time: 1, msg: "x".repeat(MAX_LINE_LENGTH * 2) }) + "\n");
  assert.ok(tail.since()[0].msg.length < MAX_LINE_LENGTH + 100);
  assert.match(tail.since()[0].msg, /line cut/);
  // A cut line keeps its level, so an error filter still shows it.
  tail.write(JSON.stringify({ level: 50, time: 1, msg: "x".repeat(MAX_LINE_LENGTH * 2) }) + "\n");
  assert.equal(tail.since(1)[0].level, 50);

  const seen: string[] = [];
  tail.subscribe(() => {
    throw new Error("broken");
  });
  tail.subscribe((e) => seen.push(e.msg));
  tail.write(`{"level":30,"time":2,"msg":"a"}\n`);
  tail.write(`{"level":30,"time":3,"msg":"b"}\n`);
  assert.deepEqual(seen, ["a", "b"], "the other listener keeps getting lines");
});

test("the log stream needs the operator token and a signed-in user, and is not on the public port", async () => {
  await withLogging(async ({ server, opUrl }) => {
    assert.equal((await fetch(`${opUrl}/api/logs`)).status, 401);
    const noUser = await fetch(`${opUrl}/api/logs`, { headers: { authorization: `Bearer ${TOKEN}` } });
    assert.equal(noUser.status, 401);
    assert.equal((await fetch(`${server.url}/api/logs`, { headers: SIGNED_IN })).status, 404);
  });
});

test("the log stream sends the buffered lines, then each new line as it is logged", async () => {
  await withLogging(async ({ server, opUrl, stdout }) => {
    const roomId = randomUUID();
    const admin = await createTestAgent();
    await authenticateAgent(server.url, roomId, admin);

    const stream = await openLogs(opUrl);
    try {
      assert.equal(stream.status, 200);
      assert.match(stream.contentType ?? "", /^text\/event-stream/);
      // The public app's request lines from before the stream opened.
      await stream.until((events) => events.some((e) => e.data.req?.url === `/rooms/${roomId}/auth`));

      const joiner = await createTestAgent();
      const added = await fetch(`${opUrl}/api/rooms/${roomId}/acl`, {
        method: "POST",
        headers: { ...SIGNED_IN, origin: ORIGIN, "content-type": "application/json" },
        body: JSON.stringify({ action: "add", did: joiner.did }),
      });
      assert.equal(added.status, 200);
      await stream.until((events) => events.some((e) => e.data.msg === "operator acl change"));
      const change = stream.events.find((e) => e.data.msg === "operator acl change")!.data;
      assert.equal(change.operator, "lucksus");
      assert.equal(change.did, joiner.did);
      assert.equal(change.pid, undefined);

      const boot = server.built.logTail!.boot;
      assert.ok(stream.events.every((e) => e.id === `${boot}-${e.data.seq}`), "each id is <boot>-<seq>");
      const seqs = stream.events.map((e) => e.data.seq as number);
      assert.deepEqual(seqs, [...seqs].sort((a, b) => a - b), "sequence numbers rise");
      assert.ok(!JSON.stringify(stream.events).includes(TOKEN), "the operator token is not in the stream");
      assert.ok(stdout.some((line) => line.includes("operator acl change")), "lines still reach the destination");
    } finally {
      stream.close();
    }
  });
});

test("a reconnect with Last-Event-ID gets only the lines after it; an id from before a restart gets all", async () => {
  await withLogging(async ({ server, opUrl }) => {
    const tail = server.built.logTail!;
    tail.write(`{"level":30,"time":1,"msg":"first"}\n{"level":30,"time":2,"msg":"second"}\n`);
    const firstSeq = tail.since().find((e) => e.msg === "first")!.seq;

    const resumed = await openLogs(opUrl, { ...SIGNED_IN, "last-event-id": `${tail.boot}-${firstSeq}` });
    try {
      await resumed.until((events) => events.some((e) => e.data.msg === "second"));
      assert.ok(!resumed.events.some((e) => e.data.msg === "first"));
    } finally {
      resumed.close();
    }

    // An id from another boot (this server restarted), even with a sequence
    // number below the current one.
    const otherBoot = tail.boot === "00000000" ? "11111111" : "00000000";
    const afterRestart = await openLogs(opUrl, { ...SIGNED_IN, "last-event-id": `${otherBoot}-1` });
    try {
      await afterRestart.until((events) => events.some((e) => e.data.msg === "first"));
    } finally {
      afterRestart.close();
    }
  });
});

/** Opens `GET /api/logs` on its own connection (no pool) and resolves once the response starts. */
function openRawLogs(opUrl: string): Promise<{ status: number; req: ClientRequest }> {
  return new Promise((resolve, reject) => {
    const req = httpGet(`${opUrl}/api/logs`, { headers: SIGNED_IN, agent: false }, (res) => {
      res.resume();
      resolve({ status: res.statusCode ?? 0, req });
    });
    req.on("error", () => {});
    req.once("error", reject);
  });
}

test("a client that disconnects frees its stream slot and stops getting lines", async () => {
  await withLogging(async ({ server, opUrl }) => {
    // node:http without an agent, so a closed stream closes its socket and
    // the client opens no spare connections for close() to wait on.
    const streams = [];
    for (let i = 0; i < MAX_LOG_STREAMS; i++) streams.push(await openRawLogs(opUrl));
    assert.ok(streams.every((s) => s.status === 200));
    const refused = await openRawLogs(opUrl);
    assert.equal(refused.status, 429);
    streams.forEach((s) => s.req.destroy());

    // LogTail's listener set is private; reading it is the direct check that nothing leaks.
    const listeners = (server.built.logTail as unknown as { listeners: Set<unknown> }).listeners;
    const deadline = Date.now() + 3000;
    while (listeners.size > 0 && Date.now() < deadline) await new Promise((r) => setTimeout(r, 20));
    assert.equal(listeners.size, 0, "every closed stream unsubscribed");

    const again = await openRawLogs(opUrl);
    assert.equal(again.status, 200, "the slots are free again");
    again.req.destroy();
  });
});

test("a client that stops reading is dropped: its connection closed, not left holding the queued lines", async () => {
  await withLogging(async ({ server, opUrl }) => {
    const opServer = server.built.operatorApp!.server;
    const { port } = new URL(opUrl);
    // A client that sends the request and then never reads the response.
    const socket = connect(Number(port), "127.0.0.1");
    await new Promise((r) => socket.once("connect", r));
    socket.pause();
    socket.write(
      `GET /api/logs HTTP/1.1\r\nHost: 127.0.0.1\r\nAuthorization: ${SIGNED_IN.authorization}\r\n` +
        `X-Forwarded-User: ${SIGNED_IN["x-forwarded-user"]}\r\n\r\n`
    );
    const tail = server.built.logTail!;
    const listeners = (tail as unknown as { listeners: Set<unknown> }).listeners;
    const deadline = Date.now() + 3000;
    while (listeners.size === 0 && Date.now() < deadline) await new Promise((r) => setTimeout(r, 20));
    assert.equal(listeners.size, 1, "the stream is open");

    // Far more than the socket buffers and the 1 MiB limit hold.
    const line = JSON.stringify({ level: 30, time: 1, msg: "x".repeat(MAX_LINE_LENGTH - 100) }) + "\n";
    for (let i = 0; i < 4000 && listeners.size > 0; i++) {
      tail.write(line);
      if (i % 50 === 0) await new Promise((r) => setImmediate(r));
    }
    assert.equal(listeners.size, 0, "the stream unsubscribed");

    const connections = () => new Promise<number>((r) => opServer.getConnections((_e, n) => r(n)));
    const closeBy = Date.now() + 3000;
    while ((await connections()) > 0 && Date.now() < closeBy) await new Promise((r) => setTimeout(r, 20));
    assert.equal(await connections(), 0, "the server closed the connection");
    socket.destroy();
  });
});

test("the log stream answers 404 when logging is off", async () => {
  await withLogging(async ({ opUrl }) => {
    const res = await fetch(`${opUrl}/api/logs`, { headers: SIGNED_IN });
    assert.equal(res.status, 404);
  }, false);
});

test(`at most ${MAX_LOG_STREAMS} log streams are open at once, and close() ends them`, async () => {
  const stdout: string[] = [];
  const server = await startTestServer({
    logger: true,
    logDestination: { write: (line: string) => stdout.push(line) },
    operator: { token: TOKEN, allowedOrigins: [ORIGIN] },
  });
  const opApp = server.built.operatorApp!;
  await opApp.listen({ port: 0, host: "127.0.0.1" });
  const opUrl = `http://127.0.0.1:${(opApp.server.address() as AddressInfo).port}`;
  const streams = [];
  try {
    for (let i = 0; i < MAX_LOG_STREAMS; i++) streams.push(await openLogs(opUrl));
    assert.ok(streams.every((s) => s.status === 200));
    assert.equal((await fetch(`${opUrl}/api/logs`, { headers: SIGNED_IN })).status, 429);
  } finally {
    // Open streams must not hold the listener open.
    const closed = server.close().then(() => "closed");
    const timeout = new Promise((r) => setTimeout(() => r("timed out"), 3000));
    assert.equal(await Promise.race([closed, timeout]), "closed");
    streams.forEach((s) => s.close());
  }
});
