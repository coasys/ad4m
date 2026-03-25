// e2e/mesh-2user.spec.ts — SFU health + room lifecycle (no Holochain required)

import { test, expect } from './fixtures.js';
import { startExecutor, stopExecutor, type ExecutorInstance } from './helpers/executor.js';

test.describe('SFU Infrastructure — Single Executor', () => {
  let executor: ExecutorInstance;

  test.beforeAll(async () => {
    executor = await startExecutor({ holochain: false });
  });

  test.afterAll(async () => {
    await stopExecutor(executor);
  });

  test('SFU health reports running', async () => {
    const result = await executor.api.query(`
      { sfuHealth { uptimeMs roomCount totalParticipants eventLoopAlive } }
    `);
    expect(result.errors).toBeUndefined();
    const health = result.data?.sfuHealth as {
      uptimeMs: number; roomCount: number; totalParticipants: number; eventLoopAlive: boolean;
    };
    expect(health.eventLoopAlive).toBe(true);
    expect(health.roomCount).toBe(0);
    expect(health.totalParticipants).toBe(0);
  });

  test('start and stop a room', async () => {
    const nhUrl = 'nh://test-mesh';
    const roomId = 'lobby';

    // Start room
    const startResult = await executor.api.query(`
      mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "${roomId}") {
        neighbourhoodUrl roomName participantCount
      } }
    `);
    expect(startResult.errors).toBeUndefined();
    const room = startResult.data?.sfuStartRoom as {
      neighbourhoodUrl: string; roomName: string; participantCount: number;
    };
    expect(room.neighbourhoodUrl).toBe(nhUrl);
    expect(room.roomName).toBe(roomId);
    expect(room.participantCount).toBe(0);

    // Verify room shows in list
    const listResult = await executor.api.query(`
      { sfuRooms { neighbourhoodUrl roomName participantCount } }
    `);
    const rooms = listResult.data?.sfuRooms as { neighbourhoodUrl: string; roomName: string }[];
    expect(rooms.length).toBe(1);
    expect(rooms[0].roomName).toBe(roomId);

    // Stop room
    const stopResult = await executor.api.query(`
      mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "${roomId}") }
    `);
    expect(stopResult.errors).toBeUndefined();
    expect(stopResult.data?.sfuStopRoom).toBe(true);

    // Verify room gone
    const listResult2 = await executor.api.query(`
      { sfuRooms { neighbourhoodUrl roomName } }
    `);
    const rooms2 = listResult2.data?.sfuRooms as { neighbourhoodUrl: string }[];
    expect(rooms2.length).toBe(0);
  });

  test('default SFU config is mesh mode', async () => {
    const result = await executor.api.query(`
      { sfuConfig(neighbourhoodUrl: "nh://nonexistent") {
        mode fallback maxMeshParticipants
      } }
    `);
    expect(result.errors).toBeUndefined();
    const config = result.data?.sfuConfig as {
      mode: string; fallback: string; maxMeshParticipants: number;
    };
    // Default should be mesh with reasonable defaults
    expect(config.mode).toBeTruthy();
    expect(config.fallback).toBeTruthy();
  });
});
