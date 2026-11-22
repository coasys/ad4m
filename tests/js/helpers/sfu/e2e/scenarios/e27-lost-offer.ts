/**
 * E27: a renegotiation offer to carol is lost on its way down.
 *
 * dave joins; the executor's `sfu-call-renegotiation-offer` to carol, which
 * carries his tracks, is eaten by the framing proxy. Only the offer brings
 * carol dave's media, so the row proves it recovered: the executor resends
 * the unanswered offer (10 s), carol answers, and she hears and sees dave
 * within the bound, on the same call (nobody leaves). See `lost-signal.ts`
 * for what the row asserts.
 *
 * Skipped while listed in `FAILS_ON_DEV`; `E2E_KNOWN_FAILURES=1` runs it.
 */

import { matchEvent } from "../../faults/index.js";
import { lostSignalRow } from "./lost-signal.js";

export const e27LostOffer = lostSignalRow({
  id: "e27",
  name: "A lost renegotiation offer event",
  description: "the sfu-call-renegotiation-offer dave's join sends carol is eaten: its resend is answered and carol hears and sees dave without a rebuild",
  kind: "lost-offer",
  // Offers go only to their target's sockets, so carol's proxy sees only hers.
  arm: (proxy) => ({
    ...proxy.dropOnce({ direction: "down", match: matchEvent("sfu-call-renegotiation-offer") }),
    what: "the sfu-call-renegotiation-offer event to carol",
  }),
  recoverMs: 25_000,
  holdMs: 40_000,
});
