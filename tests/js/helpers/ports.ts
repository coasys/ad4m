import net from "net";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

// ---------------------------------------------------------------------------
// Port allocation — OS-assigned free ports
// ---------------------------------------------------------------------------

/**
 * Returns a single port number that is free at the moment of the call.
 *
 * Uses a bind-test on 127.0.0.1 — much safer than picking a static range and
 * hoping nothing collides.  The port is released immediately after probing,
 * so a race is theoretically possible but vanishingly unlikely in practice.
 */
export function getFreePort(): Promise<number> {
  return new Promise((resolve, reject) => {
    const server = net.createServer();
    server.unref();
    server.on("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const addr = server.address();
      const port = typeof addr === "object" && addr !== null ? addr.port : 0;
      server.close(() => resolve(port));
    });
  });
}

/**
 * Returns `count` free ports, all distinct.
 * Each probe opens and immediately closes a TCP server, so the ports are
 * released for the executor to bind to.
 */
export async function getFreePorts(count: number): Promise<number[]> {
  const ports: number[] = [];
  for (let i = 0; i < count; i++) {
    ports.push(await getFreePort());
  }
  return ports;
}

// ---------------------------------------------------------------------------
// Port registry — enables cleanup.js to kill stray executors by port
//
// The registry lives at tst-tmp/active-ports.json (next to helpers/).
// Tests write their allocated ports on start and remove them on teardown.
// cleanup.js reads the file and kills processes on those ports.
// This handles the case where mocha is forcibly killed without running after().
// ---------------------------------------------------------------------------

function getRegistryPath(): string {
  // tst-tmp is a sibling of the helpers/ directory  
  return path.resolve(__dirname, "..", "tst-tmp", "active-ports.json");
}

/** Register ports as "in use by a live test executor". */
export function registerPorts(ports: number[]): void {
  const filePath = getRegistryPath();
  let registry: number[] = [];
  try {
    if (fs.existsSync(filePath)) {
      registry = JSON.parse(fs.readFileSync(filePath, "utf8"));
    }
  } catch { /* ignore — start fresh */ }

  const updated = [...new Set([...registry, ...ports])];
  try {
    fs.mkdirSync(path.dirname(filePath), { recursive: true });
    fs.writeFileSync(filePath, JSON.stringify(updated));
  } catch { /* non-fatal */ }
}

/** Deregister ports when executor is cleanly shut down. */
export function deregisterPorts(ports: number[]): void {
  const filePath = getRegistryPath();
  try {
    if (!fs.existsSync(filePath)) return;
    const registry: number[] = JSON.parse(fs.readFileSync(filePath, "utf8"));
    const updated = registry.filter((p) => !ports.includes(p));
    if (updated.length === 0) {
      fs.unlinkSync(filePath);
    } else {
      fs.writeFileSync(filePath, JSON.stringify(updated));
    }
  } catch { /* non-fatal */ }
}

/** Read the current port registry. Returns [] if no file exists. */
export function readPortRegistry(): number[] {
  const filePath = getRegistryPath();
  try {
    if (!fs.existsSync(filePath)) return [];
    return JSON.parse(fs.readFileSync(filePath, "utf8"));
  } catch {
    return [];
  }
}
