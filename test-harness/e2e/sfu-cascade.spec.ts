// e2e/sfu-cascade.spec.ts — Cascaded SFU topology (requires multi-executor Holochain sync)

import { test, expect } from './fixtures.js';
import { startExecutors, stopAll, type ExecutorInstance } from './helpers/executor.js';

test.describe('SFU Cascade — Multi-Node', () => {
  test.skip(true, 'Cascaded SFU requires Holochain neighbourhood sync between executors');

  test('cascaded rooms distribute across nodes', async () => {
    // TODO: Requires Holochain-based neighbourhood with 3+ executors
    // 1. Create neighbourhood with executor 1
    // 2. Join from executors 2 and 3
    // 3. Configure cascaded mode with all 3 as SFU peers
    // 4. Start room, join calls via callJoin with SDP offers
    // 5. Verify participants route to different SFU nodes
  });
});
