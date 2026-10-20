/**
 * The last log lines of this process, kept in memory for the operator's
 * live log view (operator.ts `GET /api/logs`). The server's pino loggers
 * write each line to their destination (stdout) and here as well. Nothing
 * is written to disk.
 */

/** One log line: pino's JSON fields, or `{ msg }` for a line that is not JSON. */
export interface LogEntry {
  /** pino level: 10 trace, 20 debug, 30 info, 40 warn, 50 error, 60 fatal. */
  level?: number;
  msg?: string;
  [field: string]: unknown;
}

export class LogTail {
  private readonly entries: LogEntry[] = [];
  private readonly listeners = new Set<(entry: LogEntry) => void>();

  constructor(private readonly capacity = 1000) {}

  /** Takes what a pino destination receives: one or more newline-terminated JSON lines. */
  write(chunk: string): void {
    for (const line of chunk.split("\n")) {
      if (!line.trim()) continue;
      let entry: LogEntry;
      try {
        // Repeated on every line and of no use to someone reading this server's logs.
        const { pid: _pid, hostname: _hostname, v: _v, ...fields } = JSON.parse(line);
        entry = fields;
      } catch {
        entry = { msg: line };
      }
      this.entries.push(entry);
      if (this.entries.length > this.capacity) this.entries.shift();
      for (const listener of this.listeners) listener(entry);
    }
  }

  /** The buffered lines, oldest first. */
  lines(): LogEntry[] {
    return [...this.entries];
  }

  /** Calls `listener` with every line written from now on. Returns the unsubscribe. */
  subscribe(listener: (entry: LogEntry) => void): () => void {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  }
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
