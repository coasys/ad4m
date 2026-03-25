// e2e/leave-rejoin.spec.ts — Room lifecycle: start, query nodes, stop, restart

import { test, expect } from './fixtures.js';
import { startExecutors, stopAll, type ExecutorInstance } from './helpers/executor.js';

test.describe('Room Lifecycle — Start / Stop / Restart', () => {
  let executors: ExecutorInstance[];
  const nhUrl = 'nh://lifecycle-test';

  test.beforeAll(async () => {
    executors = await startExecutors(2, { holochain: false });
  });

  test.afterAll(async () => {
    await stopAll(executors);
  });

  test('both executors have SFU running', async () => {
    for (const exec of executors) {
      const result = await exec.api.query(`
        { sfuHealth { eventLoopAlive roomCount } }
      `);
      expect(result.errors).toBeUndefined();
      const health = result.data?.sfuHealth as { eventLoopAlive: boolean; roomCount: number };
      expect(health.eventLoopAlive).toBe(true);
    }
  });

  test('start room on executor 1, visible only on executor 1', async () => {
    const [exec1, exec2] = executors;

    // Start room on exec1
    const startResult = await exec1.api.query(`
      mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "room-1") {
        roomName participantCount
      } }
    `);
    expect(startResult.errors).toBeUndefined();

    // Exec1 should have the room
    const rooms1 = (await exec1.api.query(`
      { sfuRooms { roomName } }
    `)).data?.sfuRooms as { roomName: string }[];
    expect(rooms1.length).toBe(1);

    // Exec2 should NOT have it (rooms are local per SFU)
    const rooms2 = (await exec2.api.query(`
      { sfuRooms { roomName } }
    `)).data?.sfuRooms as { roomName: string }[];
    expect(rooms2.length).toBe(0);

    // Clean up
    await exec1.api.query(`
      mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "room-1") }
    `);
  });

  test('stop and restart room preserves config', async () => {
    const exec = executors[0];

    // Set config
    await exec.api.query(`
      mutation {
        sfuSetConfig(neighbourhoodUrl: "${nhUrl}", mode: "designated", designatedPeer: "${exec.did}", maxMeshParticipants: 0)
      }
    `);

    // Start room
    await exec.api.query(`
      mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "restart-room") { roomName } }
    `);

    // Stop room
    await exec.api.query(`
      mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "restart-room") }
    `);

    // Config should survive room stop
    const config = (await exec.api.query(`
      { sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode designatedPeer } }
    `)).data?.sfuConfig as { mode: string; designatedPeer: string | null };
    expect(config.mode).toBe('designated');
    expect(config.designatedPeer).toBe(exec.did);

    // Restart room
    const restartResult = await exec.api.query(`
      mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "restart-room") {
        roomName participantCount
      } }
    `);
    expect(restartResult.errors).toBeUndefined();
    expect((restartResult.data?.sfuStartRoom as any).participantCount).toBe(0);

    // Clean up
    await exec.api.query(`
      mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "restart-room") }
    `);
  });

  test('query SFU nodes for room', async () => {
    const exec = executors[0];

    await exec.api.query(`
      mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "node-room") { roomName } }
    `);

    const nodesResult = await exec.api.query(`
      { sfuNodesForRoom(neighbourhoodUrl: "${nhUrl}", roomId: "node-room") {
        did participantCount capacityHint
      } }
    `);
    expect(nodesResult.errors).toBeUndefined();
    // Nodes might be empty (no cascaded peers) but should not error
    const nodes = nodesResult.data?.sfuNodesForRoom as any[];
    expect(Array.isArray(nodes)).toBe(true);

    await exec.api.query(`
      mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "node-room") }
    `);
  });
});
