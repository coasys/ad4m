/**
 * E28: a renegotiation offer to carol is lost on its way down.
 *
 * dave joins; the executor's `sfu-call-renegotiation-offer` to carol, which
 * carries his tracks, is eaten by the framing proxy. Only the offer brings
 * carol dave's media, so the row proves it recovered: the executor resends
 * the unanswered offer (10 s), carol answers, and she hears and sees dave
 * within the bound, on the same call (nobody leaves). See `lost-signal.ts`
 * for what the row asserts.
 */

import { matchEvent } from "../../faults/index.js";
import { HOLD_PAST_RESEND_MS, RESEND_RECOVER_MS } from "../../faults/timing.js";
import { lostSignalRow } from "./lost-signal.js";

export const e28LostOffer = lostSignalRow({
  id: "e28",
  name: "A lost renegotiation offer event",
  description: "the sfu-call-renegotiation-offer dave's join sends carol is eaten: its resend is answered and carol hears and sees dave without a rebuild",
  kind: "lost-offer",
  // Offers go only to their target's sockets, so carol's proxy sees only hers.
  arm: (proxy) => ({
    ...proxy.intercept({ direction: "down", match: matchEvent("sfu-call-renegotiation-offer"), once: true, action: "drop" }),
    what: "the sfu-call-renegotiation-offer event to carol",
  }),
  recoverMs: RESEND_RECOVER_MS,
  holdMs: HOLD_PAST_RESEND_MS,
});
