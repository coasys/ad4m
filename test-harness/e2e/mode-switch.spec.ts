// e2e/mode-switch.spec.ts — SFU config switching (mesh ↔ designated ↔ cascaded)

import { test, expect } from './fixtures.js';
import { startExecutor, stopExecutor, type ExecutorInstance } from './helpers/executor.js';

test.describe('Mode Switch — Config Transitions', () => {
  let executor: ExecutorInstance;
  const nhUrl = 'nh://mode-switch-test';

  test.beforeAll(async () => {
    executor = await startExecutor({ holochain: false });
  });

  test.afterAll(async () => {
    await stopExecutor(executor);
  });

  test('switch config from mesh to designated and back', async () => {
    // Set mesh mode
    await executor.api.query(`
      mutation { sfuSetConfig(neighbourhoodUrl: "${nhUrl}", mode: "mesh") }
    `);
    let config = (await executor.api.query(`
      { sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode designatedPeer maxMeshParticipants } }
    `)).data?.sfuConfig as { mode: string; designatedPeer: string | null; maxMeshParticipants: number };
    expect(config.mode).toBe('mesh');

    // Switch to designated
    await executor.api.query(`
      mutation {
        sfuSetConfig(
          neighbourhoodUrl: "${nhUrl}",
          mode: "designated",
          designatedPeer: "${executor.did}",
          maxMeshParticipants: 0
        )
      }
    `);
    config = (await executor.api.query(`
      { sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode designatedPeer maxMeshParticipants } }
    `)).data?.sfuConfig as { mode: string; designatedPeer: string | null; maxMeshParticipants: number };
    expect(config.mode).toBe('designated');
    expect(config.designatedPeer).toBe(executor.did);
    expect(config.maxMeshParticipants).toBe(0);

    // Switch back to mesh
    await executor.api.query(`
      mutation { sfuSetConfig(neighbourhoodUrl: "${nhUrl}", mode: "mesh", maxMeshParticipants: 4) }
    `);
    config = (await executor.api.query(`
      { sfuConfig(neighbourhoodUrl: "${nhUrl}") { mode maxMeshParticipants } }
    `)).data?.sfuConfig as { mode: string; designatedPeer: string | null; maxMeshParticipants: number };
    expect(config.mode).toBe('mesh');
    expect(config.maxMeshParticipants).toBe(4);
  });

  test('room persists across config changes', async () => {
    // Start room in mesh mode
    await executor.api.query(`
      mutation { sfuSetConfig(neighbourhoodUrl: "${nhUrl}", mode: "mesh") }
    `);
    await executor.api.query(`
      mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "persistent-room") {
        roomName
      } }
    `);

    // Switch config to designated
    await executor.api.query(`
      mutation {
        sfuSetConfig(neighbourhoodUrl: "${nhUrl}", mode: "designated", designatedPeer: "${executor.did}")
      }
    `);

    // Room should still exist
    const rooms = (await executor.api.query(`
      { sfuRooms { neighbourhoodUrl roomName } }
    `)).data?.sfuRooms as { neighbourhoodUrl: string; roomName: string }[];
    const ourRoom = rooms.find(r => r.roomName === 'persistent-room');
    expect(ourRoom).toBeTruthy();

    // Clean up
    await executor.api.query(`
      mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "persistent-room") }
    `);
  });

  test('TURN config can be set', async () => {
    await executor.api.query(`
      mutation {
        sfuSetConfig(
          neighbourhoodUrl: "${nhUrl}",
          mode: "designated",
          designatedPeer: "${executor.did}",
          turnUrl: "turn:turn.example.com:3478",
          turnUsername: "user",
          turnCredential: "pass"
        )
      }
    `);
    const config = (await executor.api.query(`
      { sfuConfig(neighbourhoodUrl: "${nhUrl}") {
        mode turnUrl turnUsername turnCredential
      } }
    `)).data?.sfuConfig as {
      mode: string; turnUrl: string | null; turnUsername: string | null; turnCredential: string | null;
    };
    expect(config.turnUrl).toBe('turn:turn.example.com:3478');
    expect(config.turnUsername).toBe('user');
    expect(config.turnCredential).toBe('pass');
  });
});
