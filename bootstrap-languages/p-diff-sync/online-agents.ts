/**
 * `telepresence.getOnlineAgents` for p-diff-sync, kept free of host imports
 * so it can be unit-tested (tests/online-agents.test.ts).
 *
 * `get_active_agents` lists the agents with a recent online-status link;
 * each one then needs its own `get_agents_status` zome call.
 */

export async function getOnlineAgents(
    getActiveAgents: () => Promise<any[]>,
    getAgentStatus: (agent: any) => Promise<any>,
): Promise<any[]> {
    const active = await getActiveAgents();
    const results: any[] = [];
    for (const agent of active) {
        results.push(await getAgentStatus(agent));
    }
    return results;
}
