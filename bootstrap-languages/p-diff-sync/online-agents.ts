/**
 * `telepresence.getOnlineAgents` for p-diff-sync, kept free of host imports
 * so it can be unit-tested (tests/online-agents.test.ts).
 *
 * `get_active_agents` lists the agents with a recent online-status link;
 * each one then needs its own `get_agents_status` zome call. Those calls
 * are independent, so they go out together rather than one round trip
 * after another (#1133). Results keep the order of `get_active_agents`,
 * and one failing call rejects the whole lookup, as the sequential loop
 * did.
 */

export async function getOnlineAgents(
    getActiveAgents: () => Promise<any[]>,
    getAgentStatus: (agent: any) => Promise<any>,
): Promise<any[]> {
    const active = await getActiveAgents();
    return Promise.all(active.map((agent) => getAgentStatus(agent)));
}
