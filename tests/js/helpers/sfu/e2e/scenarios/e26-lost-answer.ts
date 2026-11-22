/**
 * E26: carol's answer to a renegotiation offer is lost on its way up.
 *
 * dave joins; the executor offers carol his tracks; her
 * `sfu.callAnswerServerOffer` request is eaten by the framing proxy, so the
 * executor never applies it. The executor resends the unanswered offer (10 s)
 * and carol's answer to that must land; her lost call must not cost the call
 * (on dev the call's 30 s timeout reads as a failed renegotiation and
 * rebuilds it). See `lost-signal.ts` for what the row asserts.
 *
 * Gates slice 3. Skipped while listed in `FAILS_ON_DEV`;
 * `E2E_KNOWN_FAILURES=1` runs it.
 */

import { matchRpc } from "../../faults/index.js";
import { lostSignalRow } from "./lost-signal.js";

export const e26LostAnswer = lostSignalRow({
  id: "e26",
  name: "A lost answer request",
  description: "carol's sfu.callAnswerServerOffer request for the offer dave's join starts is eaten: the offer's resend carries the call without a rebuild",
  kind: "lost-answer",
  arm: (proxy) => ({
    ...proxy.dropOnce({ direction: "up", match: matchRpc("sfu.callAnswerServerOffer") }),
    what: "carol's sfu.callAnswerServerOffer request",
  }),
  recoverMs: 25_000,
  holdMs: 45_000,
});
