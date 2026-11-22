/**
 * Network-change triggers: what a host's network layer tells the SDK when
 * the machine moves networks.
 *
 * - A browser tab gets `online` on `window` (redial at once, restart ICE)
 *   and `change` on `navigator.connection` (Chromium: probe first).
 * - The e2e wrtc participants run Node: their `networkChanged()` dispatches
 *   `online` on the child's global EventTarget, the same event browsers get.
 *
 * None of this needs sudo or a real network change: the events are the whole
 * signal the SDK sees.
 */

import type { Page } from "playwright-core";
import type { Participant } from "../e2e/participant.js";

/**
 * Fire a full network change on `target`: a wrtc Participant or a browser
 * tab. The client drops its socket, dials again, and the call's ICE
 * restarts.
 */
export async function networkChanged(target: Participant | Page): Promise<void> {
  if (typeof (target as Participant).networkChanged === "function") {
    await (target as Participant).networkChanged();
    return;
  }
  await (target as Page).evaluate(() => {
    window.dispatchEvent(new Event("online"));
    (navigator as { connection?: EventTarget }).connection?.dispatchEvent(new Event("change"));
  });
}

/**
 * Only the `navigator.connection` `change` a Chromium tab gets when the
 * estimate moves (a busy call moves it): the SDK probes the socket rather
 * than dropping it. wrtc has no equivalent.
 */
export async function connectionEstimateChanged(page: Page): Promise<void> {
  await page.evaluate(() => {
    (navigator as { connection?: EventTarget }).connection?.dispatchEvent(new Event("change"));
  });
}
