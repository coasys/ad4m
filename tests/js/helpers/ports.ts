import net from "net";

/**
 * Returns `count` port numbers that are free at the moment of the call.
 *
 * Uses a bind-test on 127.0.0.1 — much safer than picking a static range and
 * hoping nothing collides.  The ports are released immediately after probing,
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
