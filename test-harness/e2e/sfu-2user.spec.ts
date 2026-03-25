// e2e/sfu-2user.spec.ts — SFU config management (designated mode)

import { test, expect } from './fixtures.js';
import { startExecutor, stopExecutor, type ExecutorInstance } from './helpers/executor.js';

test.describe('SFU Config — Designated Mode', () => {
  let executor: ExecutorInstance;
  const nhUrl = 'nh://sfu-designated-test';

  test.beforeAll(async () => {
    executor = await startExecutor({ holochain: false });
  });

  test.afterAll(async () => {
    await stopExecutor(executor);
  });

  test('set designated SFU mode with config', async () => {
    const setResult = await executor.api.query(`
      mutation {
        sfuSetConfig(
          neighbourhoodUrl: "${nhUrl}",
          mode: "designated",
          designatedPeer: "${executor.did}",
          maxMeshParticipants: 0,
          sfuPeers: ["${executor.did}"]
        )
      }
    `);
    expect(setResult.errors).toBeUndefined();
    expect(setResult.data?.sfuSetConfig).toBe(true);

    // Verify config
    const getResult = await executor.api.query(`
      { sfuConfig(neighbourhoodUrl: "${nhUrl}") {
        mode designatedPeer sfuPeers fallback maxMeshParticipants
      } }
    `);
    expect(getResult.errors).toBeUndefined();
    const config = getResult.data?.sfuConfig as {
      mode: string; designatedPeer: string | null; sfuPeers: string[];
      fallback: string; maxMeshParticipants: number;
    };
    expect(config.mode).toBe('designated');
    expect(config.designatedPeer).toBe(executor.did);
    expect(config.maxMeshParticipants).toBe(0);
    expect(config.sfuPeers).toContain(executor.did);
  });

  test('start room with SFU config active', async () => {
    // Start room
    const startResult = await executor.api.query(`
      mutation { sfuStartRoom(neighbourhoodUrl: "${nhUrl}", roomId: "call-1") {
        neighbourhoodUrl roomName participantCount participants { agentDid }
      } }
    `);
    expect(startResult.errors).toBeUndefined();
    const room = startResult.data?.sfuStartRoom as {
      roomName: string; participantCount: number; participants: { agentDid: string }[];
    };
    expect(room.roomName).toBe('call-1');
    expect(room.participantCount).toBe(0);

    // Query SFU peer for this neighbourhood
    const peerResult = await executor.api.query(`
      { sfuPeerForNeighbourhood(neighbourhoodUrl: "${nhUrl}") }
    `);
    expect(peerResult.errors).toBeUndefined();

    // Clean up
    await executor.api.query(`
      mutation { sfuStopRoom(neighbourhoodUrl: "${nhUrl}", roomId: "call-1") }
    `);
  });

  test('update config from designated to cascaded', async () => {
    const setResult = await executor.api.query(`
      mutation {
        sfuSetConfig(
          neighbourhoodUrl: "${nhUrl}",
          mode: "cascaded",
          sfuPeers: ["${executor.did}", "did:key:fake-peer-2"],
          maxMeshParticipants: 2,
          maxParticipantsPerNode: 8
        )
      }
    `);
    expect(setResult.errors).toBeUndefined();

    const getResult = await executor.api.query(`
      { sfuConfig(neighbourhoodUrl: "${nhUrl}") {
        mode sfuPeers maxMeshParticipants maxParticipantsPerNode
      } }
    `);
    const config = getResult.data?.sfuConfig as {
      mode: string; sfuPeers: string[]; maxMeshParticipants: number;
      maxParticipantsPerNode: number | null;
    };
    expect(config.mode).toBe('cascaded');
    expect(config.sfuPeers).toHaveLength(2);
    expect(config.maxMeshParticipants).toBe(2);
    expect(config.maxParticipantsPerNode).toBe(8);
  });
});
