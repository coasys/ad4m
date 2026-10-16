/**
 * E15: one participant's network drops out, then comes back.
 *
 * Cuts one participant off — every socket its process holds, its executor
 * connection and its WebRTC sockets, found from /proc — while the others'
 * traffic flows. A short dropout, which the connection rides out, and a long
 * one past the point where the server drops the peer: the SFU is ICE-lite, so
 * it calls a peer disconnected only once 15 s pass without a binding request
 * (str0m's `RECENT_BINDING_REQUEST`), and drops it 10 s after that
 * (`ICE_DISCONNECT_GRACE`). The client gives up on the connection 10 s after
 * ICE reports it disconnected and rebuilds, mid-dropout, through a signalling
 * connection that is cut too. After each, everyone must hear
 * and see everyone again with nobody doing anything: no track replaced, no
 * rejoin, and no app re-reading `session.participants` — media is measured on
 * the streams the session's events handed over (`Snapshot.shown`).
 *
 * Needs `sudo -n tc`; skipped without it. Shares E6's host lease.
 */

import { leaseNet, netAvailable, socketPorts, type NetLease } from "../../net.js";
import type { Call } from "../call.js";
import { CAMERA } from "../media.js";
import { e2eScenario } from "../scenario.js";

const EVERYONE = ["u1", "u2", "u3"];
const sleep = (ms: number) => new Promise((r) => setTimeout(r, ms));
const debug = (line: string) => {
  if (process.env.E2E_DEBUG) console.log(`[e15] ${new Date().toISOString().slice(11, 23)} ${line}`);
};

/**
 * Cut `name` off for `ms`. Sockets it opens meanwhile — a rebuilt connection
 * gathers new ones — join the cut as they appear.
 */
async function dropOut(net: NetLease, call: Call, name: string, ms: number): Promise<void> {
  const pid = call.who(name).pid;
  const cut = new Set(socketPorts(pid));
  net.impair([...cut], { lossPct: 100 });
  debug(`${name} cut off: ports ${[...cut].join(" ")}`);
  try {
    // Nobody sees `name` any more: the cut reached them.
    await call.eventually(call.noVideoFrom(name), 5_000, `${name}'s dropout takes effect`);
    const end = Date.now() + ms;
    while (Date.now() < end) {
      const fresh = socketPorts(pid).filter((port) => !cut.has(port));
      if (fresh.length > 0) {
        net.extend(fresh);
        for (const port of fresh) cut.add(port);
        debug(`${name}'s new ports cut off too: ${fresh.join(" ")}`);
      }
      await sleep(100);
    }
  } finally {
    net.clear();
    debug(`${name} reconnected`);
  }
}

export const e15OneDropout = e2eScenario({
  id: "e15",
  name: "One participant drops out",
  description: "One participant loses its network for 4 s, then 30 s: everyone's media comes back with no user action",
  world: {
    users: EVERYONE.map((name) => ({ name })),
    neighbourhoods: [{ name: "space", owner: "u1", members: EVERYONE, call: { mode: "designated" } }],
  },
  unavailable: () => (netAvailable() ? null : "needs passwordless `sudo -n tc` to impair the loopback interface"),
  async run({ call: inSpace, step, metric }) {
    const net = await leaseNet({ onWait: () => console.log("e15: waiting for another run's impairment on this host to end") });
    const call = inSpace("space", "e15");
    const everyoneBack = async (timeoutMs: number, label: string) => {
      await call.eventually(call.rosterIsEveryone(), timeoutMs, `roster ${label}`);
      await call.eventually(call.hearsEveryone("shown"), timeoutMs, `audio ${label}`);
      return call.eventually(call.seesEveryone(CAMERA, "shown"), timeoutMs, `video ${label}`);
    };
    try {
      await step("three join and settle", async () => {
        await call.join(EVERYONE);
        await everyoneBack(60_000, "before the dropout");
      });

      metric("short_dropout_recovery_ms", await step("u2 drops out for 4 s: the same connections carry on, everyone hears and sees everyone again", async () => {
        const departures = async () => [...(await call.snapshots())].map(([name, s]) => `${name}:${s.departures}`).join(" ");
        const before = await departures();
        await dropOut(net, call, "u2", 4_000);
        const recovered = await everyoneBack(20_000, "after a 4 s dropout");
        const after = await departures();
        if (after !== before) throw new Error(`participants left during a 4 s dropout: ${before} → ${after}`);
        return recovered;
      }));

      metric("long_dropout_recovery_ms", await step("u2 drops out for 30 s: its session rebuilds, everyone hears and sees everyone again", async () => {
        const departures = (await call.who("u2").snapshot()).departures;
        await dropOut(net, call, "u2", 30_000);
        const recovered = await everyoneBack(60_000, "after a 30 s dropout");
        // A dropout the call rode out on its old connection would not test the rebuild.
        if ((await call.who("u2").snapshot()).departures === departures) throw new Error("u2's session never rebuilt its connection");
        return recovered;
      }));

      await step("no session reported an error", () => call.eventually(call.noErrors(), 1, "errors"));
    } finally {
      await net.release();
    }
  },
});
