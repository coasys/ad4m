/**
 * Every timing the fault rows lean on, each with the code it follows. None of
 * the SDK or executor constants is exported to `tests/js`, so they are
 * mirrored here: when one moves, change it here and the rows follow.
 */

// ── mirrored from the SDK and the executor ───────────────────────────────

/** An RPC with no answer fails after this (`core/src/apiClient.ts` DEFAULT_TIMEOUT_MS). */
export const RPC_TIMEOUT_MS = 30_000;
/** A socket pings on this tick; a ping unanswered for one more tick drops it (`core/src/apiClient.ts` PING_INTERVAL_MS). */
export const PING_INTERVAL_MS = 10_000;
/** The first redial after a dropped socket waits this (`core/src/apiClient.ts` INITIAL_RECONNECT_DELAY_MS). */
export const INITIAL_RECONNECT_DELAY_MS = 500;
/** The executor resends an unanswered renegotiation offer after this (`rust-executor/src/sfu/server.rs` OFFER_RETRY_BASE): resends at t=10 s and t=30 s, then every 30 s (OFFER_RESEND_STEADY). */
export const OFFER_RETRY_BASE_MS = 10_000;
export const OFFER_RESEND_STEADY_MS = 30_000;
/** A peer whose client asked for an ICE restart is kept this long (`rust-executor/src/sfu/server.rs` ICE_RESTART_GRACE). */
export const ICE_RESTART_GRACE_MS = 15_000;

// ── what the rows allow ──────────────────────────────────────────────────

/** A redial and an RPC over the new socket after a cut: the first retry waits INITIAL_RECONNECT_DELAY_MS; the rest is slack for a loaded host. */
export const REDIAL_MS = 10_000;
/** A silent socket is found by the ping deadline (up to 3 ping ticks), then a redial and an RPC. */
export const REDIAL_SILENT_MS = 3 * PING_INTERVAL_MS + REDIAL_MS;
/** Media keeps flowing this long on the new socket, while the SDK repairs the call's signalling on it: one ping tick. */
export const SETTLE_MS = PING_INTERVAL_MS;
/** A message armed to be dropped must cross the proxy within this: the RPC timeout bounds dave's join, which sends it. */
export const DROP_TIMEOUT_MS = RPC_TIMEOUT_MS;
/** carol and dave list, hear and see each other this long after a lost request or offer: the executor's resend, then slack. */
export const RESEND_RECOVER_MS = OFFER_RETRY_BASE_MS + 15_000;
/** Watch the call this long after a lost request or reply: past the call's RPC timeout, which would rebuild it, plus slack. */
export const HOLD_PAST_TIMEOUT_MS = RPC_TIMEOUT_MS + 15_000;
/** Watch the call this long after a lost offer: past the executor's second resend, plus slack. */
export const HOLD_PAST_RESEND_MS = OFFER_RESEND_STEADY_MS + 10_000;
/** After the link returns, the resume reply and the call it repairs: the executor's ICE restart grace. */
export const RESUME_MS = ICE_RESTART_GRACE_MS;
/** One RPC over a healthy socket: the SDK's own pong allowance (`core/src/apiClient.ts` PONG_RETURN_MS). */
export const PROBE_MS = 5_000;

// ── RunChecks ────────────────────────────────────────────────────────────

/**
 * The sampler's cadence. It is evidence gathering, not a readiness wait: a
 * sample is one snapshot RPC per participant, and the cadence is the
 * resolution of every judgement (a recovery is timed to within one sample
 * against RUN_RECOVER_MS, 40 samples). Two samples span GROWTH_MS, so every
 * second sample has a growth baseline.
 */
export const SAMPLE_MS = 500;
/**
 * Frame counters must grow over this span to count as media flowing now. The
 * e2e camera sends 15 frames a second (`e2e/media.ts` videoSource) and Opus
 * one frame each 20 ms, so a second holds about 15 video and 50 audio frames:
 * a counter that does not move over it is a stall, not frame spacing.
 */
export const GROWTH_MS = 1_000;
/** Media flows on every pair again within this of a fault window's end: a redial, then one ping tick for the SDK to repair the call's signalling on the new socket. */
export const RUN_RECOVER_MS = REDIAL_MS + SETTLE_MS;
/** `participant-left` events a session may fire over a run: a fault is nobody leaving. */
export const MAX_DEPARTURES = 0;
