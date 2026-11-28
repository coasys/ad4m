/**
 * E27: carol's answer to a renegotiation offer is lost on its way up.
 *
 * dave joins; the executor offers carol his tracks; her
 * `sfu.callAnswerServerOffer` request is eaten by the framing proxy, so the
 * executor never applies it. The executor resends the unanswered offer (10 s)
 * and carol's answer to that must land; her lost call must not cost the call
 * (the call's 30 s RPC timeout must not read as a failed renegotiation and
 * rebuild it). See `lost-signal.ts` for what the row asserts.
 */

import { matchRpc } from "../../faults/index.js";
import { HOLD_PAST_TIMEOUT_MS, RESEND_RECOVER_MS } from "../../faults/timing.js";
import { lostSignalRow } from "./lost-signal.js";

export const e27LostAnswer = lostSignalRow({
  id: "e27",
  name: "A lost answer request",
  description: "carol's sfu.callAnswerServerOffer request for the offer dave's join starts is eaten: the offer's resend carries the call without a rebuild",
  kind: "lost-answer",
  arm: (proxy) => ({
    ...proxy.intercept({ direction: "up", match: matchRpc("sfu.callAnswerServerOffer"), once: true, action: "drop" }),
    what: "carol's sfu.callAnswerServerOffer request",
  }),
  recoverMs: RESEND_RECOVER_MS,
  holdMs: HOLD_PAST_TIMEOUT_MS,
});
