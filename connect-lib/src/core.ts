import { Ad4mClient, CapabilityInput } from '@coasys/ad4m';
import {
  ConnectAnswer,
  ConnectApp,
  ConnectRenewed,
  FromFrame,
  ToFrame,
  isFrameMessage,
  originOf,
} from './messages';
import { challengeOf, newState, newVerifier } from './pkce';
import { getLocal, removeLegacyTokens, setLocal } from './utils';

export type AuthStates = 'authenticated' | 'unauthenticated';

/** Where the app's services are. Only the auth domain is read here; the frame owns the rest. */
export type AppEndpoints = {
  /** `https://coasys.id` or `https://staging.coasys.id`, or a test's own. */
  authDomain: string;
  platform?: string;
  store?: string;
};

export type Ad4mConnectOptions = {
  /** The app's id in the platform registry, which lists the origins it may run on. */
  appId: string;
  endpoints: AppEndpoints;
  capabilities: CapabilityInput[];
  /** What the frame shows of the app. Default: the page's title. */
  appInfo?: { name?: string; description?: string; iconPath?: string };
  /** Where the Connect frame opens. Default `iframe`; a popup needs a user gesture. */
  mode?: 'iframe' | 'popup';
};

/** The frame answers `ready` this soon after it loads, or it is not the Connect frame. */
const FRAME_READY_MS = 30_000;
/** A renewal's frame signs without the person: it answers at once. */
const RENEW_REPLY_MS = 30_000;
/** The session is renewed this long before it ends. */
const RENEW_LEAD_S = 300;
/** A failed renewal is tried again this often until the session ends. */
const RENEW_RETRY_S = 30;

/** An open window or iframe of the Connect frame. */
type Frame = {
  window: Window;
  ready: Promise<void>;
  closed(): boolean;
  hide(): void;
  close(): void;
};

function openFrame(url: string, origin: string, mode: 'iframe' | 'popup', visible: boolean): Frame {
  let target: Window | null;
  let element: HTMLIFrameElement | undefined;
  if (mode === 'popup') {
    target = window.open(url, 'coasys-connect', 'popup,width=480,height=720');
    if (!target) throw new Error('The browser blocked the Connect window');
  } else {
    element = document.createElement('iframe');
    element.src = url;
    element.title = 'Coasys Connect';
    // The frame draws its own backdrop and dialog; a hidden one only signs.
    Object.assign(element.style, visible
      ? { position: 'fixed', inset: '0', width: '100%', height: '100%', border: '0', zIndex: '2147483647', colorScheme: 'normal' }
      : { display: 'none' });
    document.body.appendChild(element);
    target = element.contentWindow;
    if (!target) throw new Error('The Connect frame did not open');
  }
  const frameWindow = target;
  const ready = new Promise<void>((resolve, reject) => {
    const onMessage = (event: MessageEvent) => {
      if (isFrameMessage(event, origin, frameWindow) && event.data.type === 'ready') {
        clearTimeout(timer);
        window.removeEventListener('message', onMessage);
        resolve();
      }
    };
    const timer = setTimeout(() => {
      window.removeEventListener('message', onMessage);
      reject(new Error('The Connect frame did not answer'));
    }, FRAME_READY_MS);
    window.addEventListener('message', onMessage);
  });
  // `exchange` awaits it and reports; a frame nobody waits on needs no second report.
  ready.catch(() => {});
  return {
    window: frameWindow,
    ready,
    closed: () => (element ? !element.isConnected : frameWindow.closed),
    hide: () => {
      if (element) element.style.display = 'none';
    },
    close: () => {
      if (element) element.remove();
      else frameWindow.close();
    },
  };
}

/** The executor's HTTP base for the URL the frame names; anything but http(s) and ws(s) is refused. */
function executorBase(url: string): string {
  const parsed = new URL(url);
  if (!['http:', 'https:', 'ws:', 'wss:'].includes(parsed.protocol)) {
    throw new Error(`${url} is not an executor URL`);
  }
  return `${parsed.protocol === 'ws:' ? 'http:' : parsed.protocol === 'wss:' ? 'https:' : parsed.protocol}//${parsed.host}`;
}

/** The session this page holds: what a renewal and a sign-out name to the frame. */
type Session = { state: string; grantId: string; expiresAt: number };

/**
 * The app's side of Coasys Connect: it opens the frame on the auth domain, where the person
 * picks one of their devices and the browser key signs the login; it redeems the one-time code
 * the frame answers with the PKCE verifier that never left this page, and keeps the session (one
 * hour) fresh through the frame. The token lives in this object's memory, never in storage.
 *
 * Events: `authstatechange` (detail: `AuthStates`).
 */
export default class Ad4mConnect extends EventTarget {
  readonly options: Ad4mConnectOptions;
  authState: AuthStates = 'unauthenticated';
  ad4mClient?: Ad4mClient;
  token = '';
  /** The executor's HTTP base URL, once connected. */
  baseUrl = '';

  private readonly authUrl: string;
  private readonly authOrigin: string;
  private session?: Session;
  private frame?: Frame;
  private renewal?: ReturnType<typeof setTimeout>;
  private connecting?: Promise<Ad4mClient>;

  constructor(options: Ad4mConnectOptions) {
    super();
    this.options = options;
    this.authOrigin = originOf(options.endpoints.authDomain);
    this.authUrl = `${this.authOrigin}/connect`;
    // A token an older version left in storage outlives the tab, and any script reads it.
    removeLegacyTokens();
  }

  /** The executor's port (0 before a connection). */
  get port(): number {
    if (!this.baseUrl) return 0;
    const url = new URL(this.baseUrl);
    return Number(url.port) || (url.protocol === 'https:' ? 443 : 80);
  }

  /** Opens the Connect frame and answers the client once the person has signed in. */
  connect(): Promise<Ad4mClient> {
    this.connecting ??= this.signIn().finally(() => {
      this.connecting = undefined;
    });
    return this.connecting;
  }

  /** Ends the session: the executor revokes the token, the frame forgets this page. */
  async disconnect(): Promise<void> {
    const { session, ad4mClient: client } = this;
    if (client && session) await client.agent.logout().catch(() => {});
    if (session) {
      try {
        const frame = await this.sessionFrame();
        frame.window.postMessage({ type: 'sign-out', state: session.state } satisfies ToFrame, this.authOrigin);
      } catch (e) {
        console.warn('[Connect] The frame was not told of the sign-out:', e);
      }
    }
    this.endSession();
  }

  private async signIn(): Promise<Ad4mClient> {
    const state = newState();
    const verifier = newVerifier();
    const codeChallenge = await challengeOf(verifier);
    const frame = openFrame(this.authUrl, this.authOrigin, this.options.mode ?? 'iframe', true);
    try {
      const answer = await this.exchange<ConnectAnswer>(
        frame,
        {
          type: 'connect',
          app: this.app(),
          origin: window.location.origin,
          capabilities: this.options.capabilities,
          state,
          codeChallenge,
        },
        'connected',
      );
      if (![answer.executor, answer.code].every((v) => typeof v === 'string' && v)) {
        throw new Error('The Connect frame answered without an executor or a code');
      }
      const base = executorBase(answer.executor);
      const anonymous = new Ad4mClient(base);
      const session = await anonymous.agent.redeemCode(answer.code, verifier).finally(() => anonymous.close());

      this.endSession(false);
      this.session = { state, grantId: session.grantId, expiresAt: session.expiresAt };
      this.baseUrl = base;
      this.token = session.token;
      this.adopt(new Ad4mClient(base, session.token));
      if (this.options.mode === 'popup') frame.close();
      else {
        frame.hide();
        this.frame = frame;
      }
      this.scheduleRenewal();
      this.setAuth('authenticated');
      return this.ad4mClient!;
    } catch (e) {
      frame.close();
      throw e;
    }
  }

  /**
   * Connects to a hosted multi-user node as a guest, with no frame: a random account made on the
   * first visit (its credentials kept in `localStorage`, scoped to the host) and logged in again
   * on the next. The token stays in memory.
   */
  async connectAsGuest(hostUrl: string): Promise<Ad4mClient> {
    const base = executorBase(hostUrl);
    const host = base.toLowerCase();
    const emailKey = `ad4m-guest-email-${host}`;
    const passKey = `ad4m-guest-pass-${host}`;
    const storedEmail = getLocal(emailKey);
    const storedPassword = getLocal(passKey);
    const email = storedEmail ?? `guest-${crypto.randomUUID()}@flux.demo`;
    const password = storedPassword ?? crypto.randomUUID();

    const anonymous = new Ad4mClient(base);
    let token: string;
    try {
      if (storedEmail && storedPassword) {
        token = await anonymous.agent.loginUser(email, password);
      } else {
        const created = await anonymous.agent.createUser(email, password);
        if (!created.success) throw new Error(created.error || 'Failed to create guest account');
        token = await anonymous.agent.loginUser(email, password);
        // Kept only once both steps worked, so a failed first visit starts clean.
        setLocal(emailKey, email);
        setLocal(passKey, password);
      }
    } finally {
      anonymous.close();
    }
    this.endSession(false);
    this.baseUrl = base;
    this.token = token;
    this.adopt(new Ad4mClient(base, token));
    this.setAuth('authenticated');
    return this.ad4mClient!;
  }

  private app(): ConnectApp {
    const info = this.options.appInfo ?? {};
    return {
      appId: this.options.appId,
      name: info.name ?? document.title,
      description: info.description ?? '',
      ...(info.iconPath ? { iconPath: info.iconPath } : {}),
    };
  }

  /** Posts `request` to `frame` once it is ready and answers the frame's `reply` for its state. */
  private async exchange<T extends FromFrame>(
    frame: Frame,
    request: ToFrame,
    reply: T['type'],
    timeoutMs?: number,
  ): Promise<T> {
    await frame.ready;
    return new Promise<T>((resolve, reject) => {
      let timer: ReturnType<typeof setTimeout> | undefined;
      const watch = setInterval(() => {
        if (frame.closed()) settle(() => reject(new Error('The Connect window was closed')));
      }, 500);
      const settle = (done: () => void) => {
        clearTimeout(timer);
        clearInterval(watch);
        window.removeEventListener('message', onMessage);
        done();
      };
      // A message from another window or origin, or for another state, is not for this exchange.
      const onMessage = (event: MessageEvent) => {
        if (!isFrameMessage(event, this.authOrigin, frame.window)) return;
        const message = event.data as FromFrame;
        if (!('state' in message) || message.state !== request.state) return;
        if (message.type === 'denied') {
          settle(() => reject(new Error(`Connect refused: ${message.reason}`)));
        } else if (message.type === reply) {
          settle(() => resolve(message as T));
        }
      };
      window.addEventListener('message', onMessage);
      if (timeoutMs) {
        timer = setTimeout(() => settle(() => reject(new Error('The Connect frame did not answer'))), timeoutMs);
      }
      frame.window.postMessage(request, this.authOrigin);
    });
  }

  /** The frame this session renews through: the hidden one, or a new one after a popup. */
  private async sessionFrame(): Promise<Frame> {
    if (!this.frame || this.frame.closed()) {
      this.frame = openFrame(this.authUrl, this.authOrigin, 'iframe', false);
    }
    await this.frame.ready;
    return this.frame;
  }

  private scheduleRenewal(): void {
    clearTimeout(this.renewal);
    const session = this.session;
    if (!session) return;
    const left = session.expiresAt - Date.now() / 1000;
    this.renewal = setTimeout(() => void this.renew(session), Math.max(left - RENEW_LEAD_S, 0) * 1000);
  }

  /** Renews `session`: the executor's challenge, the frame's signature, the executor's new expiry. */
  private async renew(session: Session): Promise<void> {
    if (this.session !== session || !this.ad4mClient) return;
    const client = this.ad4mClient;
    try {
      const frame = await this.sessionFrame();
      const challenge = await client.agent.sessionChallenge();
      const answer = await this.exchange<ConnectRenewed>(
        frame,
        { type: 'renew', state: session.state, executor: this.baseUrl, challenge, grantId: session.grantId },
        'renewed',
        RENEW_REPLY_MS,
      );
      const renewed = await client.agent.renewSession(answer.proof);
      if (this.session !== session) return;
      session.expiresAt = renewed.expiresAt;
      this.token = renewed.token;
      client.setToken(renewed.token);
      this.scheduleRenewal();
    } catch (e) {
      if (this.session !== session) return;
      if (session.expiresAt - Date.now() / 1000 <= 0) {
        console.warn('[Connect] The session ended; it could not be renewed:', e);
        this.endSession();
        return;
      }
      console.warn('[Connect] Renewing the session failed, trying again:', e);
      this.renewal = setTimeout(() => void this.renew(session), RENEW_RETRY_S * 1000);
    }
  }

  /** Makes `client` the connection. An executor that refuses the token ends the session. */
  private adopt(client: Ad4mClient): void {
    this.ad4mClient?.close();
    this.ad4mClient = client;
    client.onAuthFailed((reason) => {
      if (this.ad4mClient !== client) return;
      console.warn('[Connect] The executor refused the token:', reason);
      this.endSession();
    });
  }

  /** Forgets the session: the token, the client, the timer and the frame. */
  private endSession(announce = true): void {
    clearTimeout(this.renewal);
    this.frame?.close();
    this.frame = undefined;
    this.session = undefined;
    this.ad4mClient?.close();
    this.ad4mClient = undefined;
    this.token = '';
    if (announce) this.setAuth('unauthenticated');
  }

  private setAuth(state: AuthStates): void {
    if (this.authState === state) return;
    this.authState = state;
    this.dispatchEvent(new CustomEvent('authstatechange', { detail: state }));
  }
}
