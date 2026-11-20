/**
 * The messages between an app's page (this library) and the Connect frame on the auth domain
 * (`https://<auth domain>/connect`, in an iframe or a popup). Both sides check the sender's
 * origin and `source`; every message but `ready` carries the session's `state`.
 *
 * Nothing in them is secret: the one-time `code` is useless without the PKCE `codeVerifier`, which
 * never leaves the app's page.
 */
import type { CapabilityInput, SessionChallenge, SignedKeyStatement } from '@coasys/ad4m';

/** What the frame shows of the app. The frame checks the app's origin against the registry itself. */
export type ConnectApp = {
  appId: string;
  name: string;
  description: string;
  iconPath?: string;
};

/** Frame to app: the frame has loaded and listens. The app sends its first message after this. */
export type ConnectReady = { type: 'ready' };

/** App to frame: sign this page in. `origin` is the page's own (the frame trusts the event's). */
export type ConnectRequest = {
  type: 'connect';
  app: ConnectApp;
  origin: string;
  capabilities: CapabilityInput[];
  state: string;
  /** `base64url(SHA-256(codeVerifier))`, the PKCE S256 challenge the login statement is bound to. */
  codeChallenge: string;
};

/** Frame to app: the person chose a device and the executor answered a one-time code. */
export type ConnectAnswer = {
  type: 'connected';
  state: string;
  /** The URL of the executor the code is for (`https://host[:port]`, or `ws(s)://`). */
  executor: string;
  code: string;
  /** The person's `did:scid` identity. */
  did: string;
  role: string;
};

/** App to frame: sign a renewal of grant `grantId`, for the challenge the executor gave the app. */
export type ConnectRenew = {
  type: 'renew';
  state: string;
  /** The URL of the executor, as `ConnectAnswer.executor`. */
  executor: string;
  /** The executor's `user.sessionChallenge` answer (its `executor` is the executor's DID). */
  challenge: SessionChallenge;
  grantId: string;
};

/** Frame to app: the `renew` statement, signed with the browser key. The app sends it on. */
export type ConnectRenewed = {
  type: 'renewed';
  state: string;
  proof: SignedKeyStatement;
};

/** Frame to app: the frame will not sign this page in (or renew), with its reason. */
export type ConnectDenied = {
  type: 'denied';
  state: string;
  reason: string;
};

/** App to frame: the person signed out of this app; the frame forgets the session. No reply. */
export type ConnectSignOut = {
  type: 'sign-out';
  state: string;
};

export type ToFrame = ConnectRequest | ConnectRenew | ConnectSignOut;
export type FromFrame = ConnectReady | ConnectAnswer | ConnectRenewed | ConnectDenied;

/** The normalised origin (`scheme://host[:port]`) of `url`, for comparing with `event.origin`. */
export function originOf(url: string): string {
  const origin = new URL(url).origin;
  if (origin === 'null') throw new Error(`${url} has no origin`);
  return origin;
}

/**
 * Whether `event` is a message of the frame at `window` on `origin`. A page that is not the
 * frame, or the frame's origin served by another window, is refused.
 */
export function isFrameMessage(event: MessageEvent, origin: string, window: Window): boolean {
  return event.origin === origin && event.source === window && typeof event.data?.type === 'string';
}
