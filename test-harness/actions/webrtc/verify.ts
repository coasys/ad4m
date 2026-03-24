// actions/webrtc/verify.ts — Verify WebRTC streams and connections

import type { Action } from '../../lib/types.js';
import { sleep } from '../../lib/retry.js';

const action: Action = {
  name: 'webrtc/verify',
  description: 'Verify WebRTC streams, participants, and topology across browsers',
  params: {
    browserIds: { type: 'string[]', description: 'Browser resource IDs to check', required: true },
    expectedCount: { type: 'number', description: 'Expected participant count' },
    checkAudio: { type: 'boolean', description: 'Verify audio streams', default: true },
    checkVideo: { type: 'boolean', description: 'Verify video streams', default: true },
    timeout: { type: 'number', description: 'Verification timeout in ms', default: 10000 },
  },

  async run(params, ctx) {
    const start = Date.now();
    const browserIds = params.browserIds as string[];
    const expectedCount = params.expectedCount as number | undefined;
    const checkAudio = (params.checkAudio as boolean) ?? true;
    const checkVideo = (params.checkVideo as boolean) ?? true;
    const timeout = (params.timeout as number) ?? 10000;

    const results: Array<{
      browserId: string;
      audio: boolean;
      video: boolean;
      tracks: number;
      participants: number;
      connectionStates: string[];
    }> = [];

    for (const browserId of browserIds) {
      const browser = ctx.browser(browserId);
      if (!browser) {
        results.push({ browserId, audio: false, video: false, tracks: 0, participants: 0, connectionStates: [] });
        continue;
      }

      const driver = (browser as Record<string, unknown>)._driver;
      if (!driver || typeof (driver as Record<string, unknown>).evaluate !== 'function') {
        results.push({ browserId, audio: false, video: false, tracks: 0, participants: 0, connectionStates: [] });
        continue;
      }

      const d = driver as { evaluate: <T>(fn: string) => Promise<T> };

      // Poll until streams are flowing or timeout
      const deadline = Date.now() + timeout;
      let lastResult = { audio: false, video: false, tracks: 0, participants: 0, connectionStates: [] as string[] };

      while (Date.now() < deadline) {
        const info = await d.evaluate<{ audio: boolean; video: boolean; tracks: number; participants: number; connectionStates: string[] }>(`
          (() => {
            const pcs = window.__rtcPeerConnections || [];
            let audio = false, video = false, tracks = 0;
            const connectionStates = [];
            for (const pc of pcs) {
              connectionStates.push(pc.connectionState || 'unknown');
              for (const r of pc.getReceivers()) {
                if (r.track) {
                  tracks++;
                  if (r.track.kind === 'audio' && !r.track.muted) audio = true;
                  if (r.track.kind === 'video' && !r.track.muted) video = true;
                }
              }
            }
            return { audio, video, tracks, participants: pcs.filter(p => p.connectionState === 'connected').length + 1, connectionStates };
          })()
        `);

        lastResult = info;

        const audioOk = !checkAudio || info.audio;
        const videoOk = !checkVideo || info.video;
        const countOk = !expectedCount || info.participants >= expectedCount;

        if (audioOk && videoOk && countOk) break;
        await sleep(500);
      }

      results.push({ browserId, ...lastResult });
    }

    const allFlowing = results.every(r => {
      const audioOk = !checkAudio || r.audio;
      const videoOk = !checkVideo || r.video;
      const countOk = !expectedCount || r.participants >= expectedCount;
      return audioOk && videoOk && countOk;
    });

    return {
      ok: allFlowing,
      data: {
        allFlowing,
        browsers: results,
        expectedCount,
        checkAudio,
        checkVideo,
      },
      duration_ms: Date.now() - start,
    };
  },
};

export default action;
