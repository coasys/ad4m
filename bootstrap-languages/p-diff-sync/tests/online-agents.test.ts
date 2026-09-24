/**
 * Tests for online-agents.ts: `getOnlineAgents` issues one
 * `get_agents_status` zome call per active agent. Those calls must go out
 * concurrently (issue #1133) so the Holochain service sees them together
 * instead of one round trip after another.
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";

import { getOnlineAgents } from "../online-agents.ts";

type Deferred = { resolve: (v: any) => void; reject: (e: any) => void };

/** A status lookup whose calls stay pending until the test settles them. */
function controlledStatusLookup() {
    const started: { agent: string; deferred: Deferred }[] = [];
    const lookup = (agent: string) =>
        new Promise<any>((resolve, reject) => {
            started.push({ agent, deferred: { resolve, reject } });
        });
    return { started, lookup };
}

/** Let every pending microtask and I/O callback run. */
const flush = () => new Promise((r) => setImmediate(r));

describe("p-diff-sync getOnlineAgents", () => {
    it("issues every get_agents_status call before the first one resolves", async () => {
        const { started, lookup } = controlledStatusLookup();
        const pending = getOnlineAgents(async () => ["alice", "bob", "carol"], lookup);

        await flush();
        assert.deepEqual(
            started.map((s) => s.agent),
            ["alice", "bob", "carol"],
            "all status calls should be in flight at once",
        );

        // Settle out of order: results must still follow the active-agents order.
        started[2].deferred.resolve({ did: "carol" });
        started[0].deferred.resolve({ did: "alice" });
        started[1].deferred.resolve({ did: "bob" });

        assert.deepEqual(await pending, [{ did: "alice" }, { did: "bob" }, { did: "carol" }]);
    });

    it("rejects the whole lookup when one get_agents_status call fails", async () => {
        const { started, lookup } = controlledStatusLookup();
        const pending = getOnlineAgents(async () => ["alice", "bob"], lookup);

        await flush();
        started[0].deferred.resolve({ did: "alice" });
        await flush();
        started[started.length - 1].deferred.reject(new Error("zome call failed"));

        await assert.rejects(pending, /zome call failed/);
    });

    it("makes no status calls when no agent is active", async () => {
        const { started, lookup } = controlledStatusLookup();
        assert.deepEqual(await getOnlineAgents(async () => [], lookup), []);
        assert.equal(started.length, 0);
    });
});
