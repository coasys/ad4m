/**
 * The shape of the socket-fault rows (e25, e26): one participant's executor
 * WebSocket is faulted while the network, and so her media, stays up.
 *
 * alice, bob and carol are in an SFU call; carol's socket crosses a
 * `WsShaper` and her media (UDP) never does. The row applies the fault to
 * her open connections, then needs the SDK to dial a new connection through
 * the proxy and to get an RPC answered over it within `reconnectMs`. A peer
 * lives while its ICE path is alive (socket evidence decides the socket's
 * health, not the peer's), so nothing may mark carol, or anyone, left: no
 * `participant-left` for a peer still joined, every pair's audio and video
 * growing through the fault and the redial, no departure.
 */

import { all } from "../call.js";
import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";
import { RunChecks, WsShaper, reconnected } from "../../faults/index.js";
import { SETTLE_MS } from "../../faults/timing.js";

const EVERYONE = ["alice", "bob", "carol"];

export interface SocketFaultRow {
  id: string;
  name: string;
  description: string;
  /** The fault's kind in the checks' report. */
  kind: string;
  /** Fault carol's open connections; returns their ids (none: nothing was faulted). */
  apply(proxy: WsShaper): number[];
  /** What the fault is called in a failure message. */
  doing: string;
  /** A new connection that answers an RPC must come within this of the fault. */
  reconnectMs: number;
}

export function socketFaultRow(row: SocketFaultRow): ReturnType<typeof e2eScenario> {
  return e2eScenario({
    id: row.id,
    name: row.name,
    description: row.description,
    world: {
      users: EVERYONE.map((name) => ({ name })),
      neighbourhoods: [{ name: "space", owner: "alice", members: EVERYONE, call: { mode: "designated" } }],
    },
    async run({ world, call: inSpace, step }) {
      const call = inSpace("space", row.id);
      const proxy = new WsShaper(world.users.carol.port);
      await proxy.listen();
      world.routeThrough("carol", proxy.port);
      let checks: RunChecks | undefined;
      try {
        await step("three join and settle", () => call.settle(EVERYONE));

        checks = new RunChecks(call);
        await checks.start();
        const fault = checks.fault(row.kind, ["carol"], { uninterrupted: true });
        await step(`carol's socket ${row.doing}; she dials a new one that answers`, async () => {
          const faulted = row.apply(proxy);
          if (faulted.length === 0) throw new Error(`carol had no connection to fault (${row.kind})`);
          await reconnected(proxy, call.who("carol"), faulted, row.reconnectMs);
          await call.holds(all(call.hearsEveryone(), call.seesEveryone(CAMERA)), SETTLE_MS, "media on the new socket");
          fault.end();
        });
        await step("the call kept carol, and media still flows on every pair", async () => {
          const violations = await checks!.stop();
          if (violations.length) throw new Error(violations.join("\n"));
        });
        await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
      } finally {
        await checks?.abort();
        proxy.close();
      }
    },
  });
}
