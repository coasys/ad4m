// e2e/stress.spec.ts — 8-user stress test (requires 8 executors + Holochain)

import { test, expect } from './fixtures.js';

test.describe('SFU Stress — 8 Users', () => {
  test.skip(true, 'Requires 8 executors with Holochain + real WebRTC SDP negotiation');

  test('8 users in SFU room', async () => {
    // TODO: Requires:
    // 1. 8 executors with Holochain
    // 2. Shared neighbourhood
    // 3. Real SDP offers
    // 4. callJoin from each participant
    // 5. Verify stream routing and quality adaptation
  });
});
