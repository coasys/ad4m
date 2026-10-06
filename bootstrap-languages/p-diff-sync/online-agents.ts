/**
 * `telepresence.getOnlineAgents` for p-diff-sync, kept free of host imports
 * so it can be unit-tested (tests/online-agents.test.ts).
 *
 * `get_active_agents` lists the agents with a recent online-status link;
 * each one then needs its own `get_agents_status` zome call. Those calls
 * are independent, so up to STATUS_CONCURRENCY of them run at once rather
 * than one round trip after another (#1133). Results keep the order of
 * `get_active_agents`, and one failing call rejects the whole lookup, as
 * the sequential loop did.
 */

/**
 * Max `get_agents_status` calls in flight per lookup. Each one is a
 * `call_remote` to that agent and holds one of the executor's 32 node-wide
 * zome-call permits (#1138) until the peer answers or the call times out.
 * Unbounded, a space with 32+ active agents (or a few spaces polling at
 * once) would take every permit, and the presence broadcasts #1133 is about
 * would queue behind them. 8 keeps most of the speed-up (N/8 round trips
 * instead of N) and leaves the rest of the permits for other calls.
 * Remove the cap once the batch zome fn from #1135 replaces these N calls.
 */
export const STATUS_CONCURRENCY = 8;

export async function getOnlineAgents(
    getActiveAgents: () => Promise<any[]>,
    getAgentStatus: (agent: any) => Promise<any>,
): Promise<any[]> {
    const active = await getActiveAgents();
    const results: any[] = new Array(active.length);
    let next = 0;
    let failed = false;
    // Each worker takes the next unclaimed index until none are left. After
    // a failure the lookup is going to reject anyway, so no new calls start.
    const worker = async () => {
        while (!failed && next < active.length) {
            const i = next++;
            try {
                results[i] = await getAgentStatus(active[i]);
            } catch (e) {
                failed = true;
                throw e;
            }
        }
    };
    const workers = Math.min(STATUS_CONCURRENCY, active.length);
    await Promise.all(Array.from({ length: workers }, worker));
    return results;
}
