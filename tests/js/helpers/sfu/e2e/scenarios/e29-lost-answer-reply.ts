/**
 * E29: the executor's reply to carol's answer is lost on its way down.
 *
 * dave joins; the executor offers carol his tracks; carol's
 * `sfu.callAnswerServerOffer` reaches the executor and is applied, but its
 * reply is eaten by the framing proxy. The connection is in step with the
 * server — media from dave flows — and only carol's client does not know
 * it. Her unanswered call must not cost the call (the call's 30 s RPC
 * timeout must not read as a failed renegotiation and rebuild it), and a
 * client that ignored the error would not tell this apart from a lost
 * request (e27). The eaten reply must be an applied answer
 * (`result.accepted === true`): a lost refusal, or a lost `accepted: false`,
 * is a different fault. See `lost-signal.ts` for what the row asserts.
 */

import { parse, replyTo } from "../../faults/index.js";
import { HOLD_PAST_TIMEOUT_MS, RUN_RECOVER_MS } from "../../faults/timing.js";
import { lostSignalRow } from "./lost-signal.js";

export const e29LostAnswerReply = lostSignalRow({
  id: "e29",
  name: "A lost answer reply",
  description: "the executor applies carol's sfu.callAnswerServerOffer for dave's join but its reply is eaten: the call carries on without a rebuild",
  kind: "lost-answer-reply",
  arm: (proxy) => ({
    ...proxy.intercept({ direction: "down", match: replyTo("sfu.callAnswerServerOffer"), once: true, action: "drop" }),
    what: "the reply to carol's sfu.callAnswerServerOffer",
  }),
  check: (dropped) => {
    const reply = parse(dropped.text);
    const result = reply?.result as { accepted?: unknown } | undefined;
    if (reply?.error !== undefined || result?.accepted !== true) {
      throw new Error(`the eaten reply was not an applied answer ({ accepted: true }), so no applied answer was lost: ${dropped.text.slice(0, 200)}`);
    }
  },
  // No resend to wait for: the answer was applied, so the call's media already flows.
  recoverMs: RUN_RECOVER_MS,
  holdMs: HOLD_PAST_TIMEOUT_MS,
});
