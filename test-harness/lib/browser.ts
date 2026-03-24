// lib/browser.ts — Playwright wrapper with WebRTC stats extraction

import type { Browser, BrowserContext, Page } from 'playwright';

export interface BrowserDriverOptions {
  headless?: boolean;
  realMedia?: boolean;
}

export interface MediaInfo {
  audio: boolean;
  video: boolean;
  tracks: number;
}

export class BrowserDriver {
  private browser: Browser;
  private _context?: BrowserContext;
  private _page?: Page;

  private constructor(browser: Browser) {
    this.browser = browser;
  }

  static async create(opts?: BrowserDriverOptions): Promise<BrowserDriver> {
    const { chromium } = await import('playwright');

    const args: string[] = [
      '--disable-web-security',
      '--allow-running-insecure-content',
      '--ignore-certificate-errors',
    ];

    // Use fake media devices unless --real-media is specified
    if (!opts?.realMedia) {
      args.push(
        '--use-fake-ui-for-media-stream',
        '--use-fake-device-for-media-stream',
        '--use-fake-device-for-media-stream-input-device=default',
      );
    }

    // Auto-accept permissions
    args.push('--auto-accept-camera-and-microphone-capture');

    const browser = await chromium.launch({
      headless: opts?.headless ?? true,
      args,
    });

    return new BrowserDriver(browser);
  }

  async newContext(opts?: { incognito?: boolean; permissions?: string[] }): Promise<BrowserContext> {
    this._context = await this.browser.newContext({
      ignoreHTTPSErrors: true,
      permissions: opts?.permissions ?? ['camera', 'microphone'],
    });
    return this._context;
  }

  async newPage(): Promise<Page> {
    if (!this._context) await this.newContext();
    this._page = await this._context!.newPage();
    return this._page;
  }

  get page(): Page | undefined {
    return this._page;
  }

  get context(): BrowserContext | undefined {
    return this._context;
  }

  async navigate(url: string): Promise<void> {
    const page = this._page ?? await this.newPage();
    await page.goto(url, { waitUntil: 'networkidle', timeout: 30000 });
  }

  async click(selector: string): Promise<void> {
    if (!this._page) throw new Error('No page open');
    await this._page.click(selector, { timeout: 10000 });
  }

  async type(selector: string, text: string): Promise<void> {
    if (!this._page) throw new Error('No page open');
    await this._page.fill(selector, text);
  }

  async evaluate<T>(fn: string | (() => T)): Promise<T> {
    if (!this._page) throw new Error('No page open');
    return this._page.evaluate(fn) as Promise<T>;
  }

  async screenshot(path?: string): Promise<string> {
    if (!this._page) throw new Error('No page open');
    const outPath = path ?? `screenshot-${Date.now()}.png`;
    await this._page.screenshot({ path: outPath, fullPage: true });
    return outPath;
  }

  async waitForSelector(selector: string, timeout?: number): Promise<void> {
    if (!this._page) throw new Error('No page open');
    await this._page.waitForSelector(selector, { timeout: timeout ?? 10000 });
  }

  /** Extract WebRTC stats from the page */
  async getWebRTCStats(): Promise<Record<string, unknown>[]> {
    if (!this._page) throw new Error('No page open');
    return this._page.evaluate(async () => {
      const pcs = (window as unknown as Record<string, unknown[]>).__rtcPeerConnections ?? [];
      const allStats: Record<string, unknown>[] = [];
      for (const pc of pcs as RTCPeerConnection[]) {
        const stats = await pc.getStats();
        const report: Record<string, unknown> = {};
        stats.forEach((value, key) => { report[key] = value; });
        allStats.push(report);
      }
      return allStats;
    });
  }

  /** Check if media streams are flowing */
  async getMediaStreams(): Promise<MediaInfo> {
    if (!this._page) throw new Error('No page open');
    return this._page.evaluate(() => {
      const pcs = (window as unknown as Record<string, unknown[]>).__rtcPeerConnections ?? [];
      let audio = false;
      let video = false;
      let tracks = 0;
      for (const pc of pcs as RTCPeerConnection[]) {
        const receivers = pc.getReceivers();
        for (const r of receivers) {
          if (r.track) {
            tracks++;
            if (r.track.kind === 'audio') audio = true;
            if (r.track.kind === 'video') video = true;
          }
        }
      }
      return { audio, video, tracks };
    });
  }

  /** Inject WebRTC tracking hook (call early in page lifecycle) */
  async injectWebRTCTracking(): Promise<void> {
    if (!this._page) throw new Error('No page open');
    await this._page.addInitScript(() => {
      const tracked: RTCPeerConnection[] = [];
      (window as unknown as Record<string, unknown>).__rtcPeerConnections = tracked;
      const OriginalPC = window.RTCPeerConnection;
      window.RTCPeerConnection = new Proxy(OriginalPC, {
        construct(target, args) {
          const pc = new target(...args);
          tracked.push(pc);
          return pc;
        },
      }) as typeof RTCPeerConnection;
    });
  }

  async close(): Promise<void> {
    await this.browser.close();
  }
}
