/**
 * The last log lines of this process, kept in memory for the operator's
 * live log view (operator.ts `GET /api/logs`). The server's pino loggers
 * write each line to their destination (stdout) and here as well. Nothing
 * is written to disk.
 */

import { randomBytes } from "node:crypto";

/** One log line: pino's JSON fields, plus a sequence number for resuming a stream. */
export interface LogEntry {
  seq: number;
  /** pino level: 10 trace, 20 debug, 30 info, 40 warn, 50 error, 60 fatal. */
  level: number;
  /** Milliseconds since the epoch. */
  time: number;
  msg: string;
  [field: string]: unknown;
}

/** A line longer than this many characters is cut, so one oversized error cannot fill the buffer or a stream. */
export const MAX_LINE_LENGTH = 8192;

// Repeated on every line and of no use to someone reading this server's logs.
const DROPPED_FIELDS = ["pid", "hostname", "v"];

export class LogTail {
  private readonly entries: LogEntry[] = [];
  private readonly listeners = new Set<(entry: LogEntry) => void>();
  private seq = 0;

  constructor(private readonly capacity = 1000) {}

  /** Takes what a pino destination receives: one or more newline-terminated JSON lines. */
  write(chunk: string): void {
    for (const line of chunk.split("\n")) {
      if (line.trim()) this.push(parse(line));
    }
  }

  /** The buffered lines with a sequence number above `after`, oldest first. */
  since(after = 0): LogEntry[] {
    return this.entries.filter((entry) => entry.seq > after);
  }

  /** Differs on every start of the process, so a stream id from before a restart is recognised. */
  readonly boot = randomBytes(4).toString("hex");

  /** Calls `listener` with every line written from now on. Returns the unsubscribe. */
  subscribe(listener: (entry: LogEntry) => void): () => void {
    this.listeners.add(listener);
    return () => {
      this.listeners.delete(listener);
    };
  }

  private push(fields: Omit<LogEntry, "seq">): void {
    const entry = { ...fields, seq: ++this.seq } as LogEntry;
    this.entries.push(entry);
    if (this.entries.length > this.capacity) this.entries.shift();
    for (const listener of this.listeners) {
      // A broken listener must not stop logging for the server or the other listeners.
      try {
        listener(entry);
      } catch {
        this.listeners.delete(listener);
      }
    }
  }
}

function parse(line: string): Omit<LogEntry, "seq"> {
  if (line.length > MAX_LINE_LENGTH) {
    // Keep its level, so a cut error still shows under an error filter.
    const level = Number(/"level":(\d+)/.exec(line)?.[1] ?? 30);
    return { level, time: Date.now(), msg: `${line.slice(0, MAX_LINE_LENGTH)}… (line cut at ${MAX_LINE_LENGTH} characters)` };
  }
  try {
    const fields = JSON.parse(line) as Record<string, unknown>;
    if (fields && typeof fields === "object" && !Array.isArray(fields)) {
      for (const name of DROPPED_FIELDS) delete fields[name];
      return {
        ...fields,
        level: typeof fields.level === "number" ? fields.level : 30,
        time: typeof fields.time === "number" ? fields.time : Date.now(),
        msg: typeof fields.msg === "string" ? fields.msg : "",
      };
    }
  } catch {
    // Not JSON: keep the text as the message.
  }
  return { level: 30, time: Date.now(), msg: line };
}

/**
 * A pino destination that writes to `destination` and records each line in
 * `tail`, so the logs still reach stdout (or a test's sink) unchanged.
 */
export function teeDestination(destination: { write(line: string): unknown }, tail: LogTail) {
  return {
    write(line: string) {
      destination.write(line);
      tail.write(line);
    },
  };
}
