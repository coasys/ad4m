// e2e/helpers/webrtc.ts — WebRTC tracking and assertion helpers

import type { Page } from '@playwright/test';

/**
 * Script to inject into the page that monkey-patches RTCPeerConnection
 * so we can track all connections created.
 */
export const WEBRTC_TRACKING_SCRIPT = `
  window.__rtcConnections = [];
  const OriginalRTCPeerConnection = window.RTCPeerConnection;
  window.RTCPeerConnection = function(...args) {
    const pc = new OriginalRTCPeerConnection(...args);
    window.__rtcConnections.push(pc);
    return pc;
  };
  Object.assign(window.RTCPeerConnection, OriginalRTCPeerConnection);
  window.RTCPeerConnection.prototype = OriginalRTCPeerConnection.prototype;
`;

export interface WebRTCConnectionInfo {
  connectionState: string;
  iceConnectionState: string;
  signalingState: string;
  audioSenders: number;
  audioReceivers: number;
  videoSenders: number;
  videoReceivers: number;
}

/**
 * Inject the WebRTC tracking script into a page.
 * Call this before any WebRTC activity (e.g., via page.addInitScript).
 */
export async function injectWebRTCTracking(page: Page): Promise<void> {
  await page.addInitScript(WEBRTC_TRACKING_SCRIPT);
}

/**
 * Get stats for all tracked RTCPeerConnections in the page.
 */
export async function getWebRTCStats(page: Page): Promise<WebRTCConnectionInfo[]> {
  return page.evaluate(() => {
    return ((window as any).__rtcConnections || []).map((pc: RTCPeerConnection) => ({
      connectionState: pc.connectionState,
      iceConnectionState: pc.iceConnectionState,
      signalingState: pc.signalingState,
      audioSenders: pc.getSenders().filter((s) => s.track?.kind === 'audio').length,
      audioReceivers: pc.getReceivers().filter((r) => r.track?.kind === 'audio').length,
      videoSenders: pc.getSenders().filter((s) => s.track?.kind === 'video').length,
      videoReceivers: pc.getReceivers().filter((r) => r.track?.kind === 'video').length,
    }));
  });
}

/**
 * Wait for at least one RTCPeerConnection to reach the "connected" state.
 */
export async function waitForConnection(page: Page, timeout = 15_000): Promise<void> {
  await page.waitForFunction(
    () => {
      return ((window as any).__rtcConnections || []).some(
        (pc: RTCPeerConnection) => pc.connectionState === 'connected',
      );
    },
    { timeout },
  );
}

/**
 * Get the count of connected peer connections.
 */
export async function getConnectedCount(page: Page): Promise<number> {
  return page.evaluate(() => {
    return ((window as any).__rtcConnections || []).filter(
      (pc: RTCPeerConnection) => pc.connectionState === 'connected',
    ).length;
  });
}

/**
 * Wait for a specific number of connected peer connections.
 */
export async function waitForNConnections(page: Page, n: number, timeout = 20_000): Promise<void> {
  await page.waitForFunction(
    (expected) => {
      return ((window as any).__rtcConnections || []).filter(
        (pc: RTCPeerConnection) => pc.connectionState === 'connected',
      ).length >= expected;
    },
    n,
    { timeout },
  );
}
