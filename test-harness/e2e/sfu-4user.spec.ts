// e2e/sfu-4user.spec.ts — 4-user SFU test (requires 4 executors + Holochain)

import { test, expect } from './fixtures.js';

test.describe('SFU 4-User Call', () => {
  test.skip(true, 'Requires 4 executors with Holochain + real WebRTC SDP negotiation');

  test('4 users in SFU room', async () => {
    // TODO: Requires:
    // 1. 4 executors with Holochain running
    // 2. Shared neighbourhood
    // 3. Real SDP offers from browser or wrtc library
    // 4. callJoin from each participant
  });
});
