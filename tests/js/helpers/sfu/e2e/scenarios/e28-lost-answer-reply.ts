/**
 * E28: the executor's reply to carol's answer is lost on its way down.
 *
 * dave joins; the executor offers carol his tracks; carol's
 * `sfu.callAnswerServerOffer` reaches the executor and is applied, but its
 * reply is eaten by the framing proxy. The connection is in step with the
 * server — media from dave flows — and only carol's client does not know
 * it. Her unanswered call must not cost the call (on dev the call's 30 s
 * timeout reads as a failed renegotiation and rebuilds it), and a client
 * that ignored the error would not tell this apart from a lost request
 * (e26). See `lost-signal.ts` for what the row asserts.
 *
 * Gates slice 3. Skipped while listed in `FAILS_ON_DEV`;
 * `E2E_KNOWN_FAILURES=1` runs it.
 */

import { lostSignalRow } from "./lost-signal.js";

export const e28LostAnswerReply = lostSignalRow({
  id: "e28",
  name: "A lost answer reply",
  description: "the executor applies carol's sfu.callAnswerServerOffer for dave's join but its reply is eaten: the call carries on without a rebuild",
  kind: "lost-answer-reply",
  arm: (proxy) => ({ ...proxy.dropReplyOnce("sfu.callAnswerServerOffer"), what: "the reply to carol's sfu.callAnswerServerOffer" }),
  recoverMs: 20_000,
  holdMs: 45_000,
});
