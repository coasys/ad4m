/**
 * Instrumented AD4M Client — WebSocket RPC
 */

import WebSocket from "ws";

export interface TimedResult<T> {
  data: T;
  durationMs: number;
  timestamp: number;
  error?: string;
}

export interface ClientConfig {
  port: number;
  host?: string;
  adminToken: string;
}

export class InstrumentedClient {
  public readonly config: ClientConfig;
  private ws: WebSocket | null = null;
  private wsReady: Promise<void> | null = null;
  private requestId = 0;
  private pendingRequests = new Map<
    string,
    { resolve: (v: any) => void; reject: (e: any) => void }
  >();

  public metrics = {
    totalRequests: 0,
    totalErrors: 0,
    totalDurationMs: 0,
    latencies: [] as number[],
  };

  constructor(config: ClientConfig) {
    this.config = { host: "127.0.0.1", ...config };
  }

  get baseUrl(): string {
    return `http://${this.config.host}:${this.config.port}`;
  }

  get wsUrl(): string {
    return `ws://${this.config.host}:${this.config.port}/api/v1/ws?token=${this.config.adminToken}`;
  }

  async connect(): Promise<void> {
    this.wsReady = new Promise((resolve, reject) => {
      this.ws = new WebSocket(this.wsUrl);
      this.ws.on("open", () => resolve());
      this.ws.on("error", (err) => reject(err));
      this.ws.on("message", (data) => {
        try {
          const msg = JSON.parse(data.toString());
          if (msg.id) {
            const pending = this.pendingRequests.get(String(msg.id));
            if (pending) {
              this.pendingRequests.delete(String(msg.id));
              if (msg.error) {
                pending.reject(new Error(msg.error.message || JSON.stringify(msg.error)));
              } else {
                pending.resolve(msg.result);
              }
            }
          }
        } catch {}
      });
      this.ws.on("close", () => {
        for (const [, p] of this.pendingRequests) {
          p.reject(new Error("WebSocket closed"));
        }
        this.pendingRequests.clear();
      });
    });
    await this.wsReady;
  }

  async disconnect(): Promise<void> {
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  /**
   * Raw WS RPC dispatch.  Public so scenarios can hit handlers the
   * `InstrumentedClient` doesn't wrap with a typed method (e.g. the
   * SFU surface in the WebRTC scenarios).
   */
  async call<T = any>(method: string, params: any = {}): Promise<T> {
    return this.wsCall<T>(method, params);
  }

  private async wsCall<T>(method: string, params: any): Promise<T> {
    if (!this.ws || this.ws.readyState !== WebSocket.OPEN) {
      throw new Error("WebSocket not connected");
    }
    const id = String(++this.requestId);
    return new Promise<T>((resolve, reject) => {
      this.pendingRequests.set(id, { resolve, reject });
      this.ws!.send(JSON.stringify({ id, type: method, params }));
      setTimeout(() => {
        if (this.pendingRequests.has(id)) {
          this.pendingRequests.delete(id);
          reject(new Error(`WS request ${method} timed out`));
        }
      }, 120000);
    });
  }

  async timed<T>(fn: () => Promise<T>): Promise<TimedResult<T>> {
    const start = performance.now();
    const timestamp = Date.now();
    this.metrics.totalRequests++;
    try {
      const data = await fn();
      const durationMs = performance.now() - start;
      this.metrics.totalDurationMs += durationMs;
      this.metrics.latencies.push(durationMs);
      return { data, durationMs, timestamp };
    } catch (err: any) {
      const durationMs = performance.now() - start;
      this.metrics.totalErrors++;
      this.metrics.totalDurationMs += durationMs;
      this.metrics.latencies.push(durationMs);
      return { data: undefined as any, durationMs, timestamp, error: err.message };
    }
  }

  // --- High-level operations ---

  async health(): Promise<TimedResult<any>> {
    return this.timed(async () => {
      const res = await fetch(`${this.baseUrl}/health`);
      if (!res.ok) throw new Error(`Health HTTP ${res.status}`);
      return await res.json();
    });
  }

  async generateAgent(passphrase: string): Promise<TimedResult<any>> {
    return this.timed(() => this.wsCall("agent.generate", { passphrase }));
  }

  async createPerspective(name: string): Promise<TimedResult<any>> {
    return this.timed(() => this.wsCall("perspective.create", { name }));
  }

  async addLink(
    perspectiveUuid: string,
    source: string,
    predicate: string,
    target: string
  ): Promise<TimedResult<any>> {
    return this.timed(() =>
      this.wsCall("perspective.addLink", {
        uuid: perspectiveUuid,
        link: { source, predicate, target },
      })
    );
  }

  async queryLinks(
    perspectiveUuid: string,
    params?: { source?: string; predicate?: string; target?: string }
  ): Promise<TimedResult<any>> {
    return this.timed(() =>
      this.wsCall("perspective.queryLinks", {
        uuid: perspectiveUuid,
        query: params || {},
      })
    );
  }

  async runProlog(perspectiveUuid: string, query: string): Promise<TimedResult<any>> {
    return this.timed(() =>
      this.wsCall("perspective.queryProlog", { uuid: perspectiveUuid, query })
    );
  }

  async querySparql(perspectiveUuid: string, query: string): Promise<TimedResult<any>> {
    return this.timed(() =>
      this.wsCall("perspective.querySparql", { uuid: perspectiveUuid, query })
    );
  }

  async publishLanguage(
    languagePath: string,
    languageMeta: { name: string; description?: string; sourceCodeLink?: string; possibleTemplateParams?: string[] }
  ): Promise<TimedResult<any>> {
    return this.timed(() =>
      this.wsCall("language.publish", {
        languagePath,
        languageMeta: {
          name: languageMeta.name,
          description: languageMeta.description ?? "",
          sourceCodeLink: languageMeta.sourceCodeLink ?? "",
          possibleTemplateParams: languageMeta.possibleTemplateParams ?? ["uid", "name"],
        },
      })
    );
  }

  async applyTemplateAndPublish(
    sourceLanguageHash: string,
    templateData: string
  ): Promise<TimedResult<any>> {
    return this.timed(() =>
      this.wsCall("language.applyTemplate", {
        sourceLanguageHash,
        templateData,
      })
    );
  }

  async publishNeighbourhood(
    perspectiveUuid: string,
    linkLanguageAddress: string,
    meta?: { links?: any[] }
  ): Promise<TimedResult<any>> {
    return this.timed(() =>
      this.wsCall("neighbourhood.publish", {
        perspectiveUuid,
        linkLanguage: linkLanguageAddress,
        meta: meta || { links: [] },
      })
    );
  }

  resetMetrics(): void {
    this.metrics = { totalRequests: 0, totalErrors: 0, totalDurationMs: 0, latencies: [] };
  }

  getStats() {
    const sorted = [...this.metrics.latencies].sort((a, b) => a - b);
    const count = sorted.length;
    if (count === 0) {
      return { count: 0, errors: 0, avgMs: 0, p50Ms: 0, p95Ms: 0, p99Ms: 0, minMs: 0, maxMs: 0 };
    }
    return {
      count,
      errors: this.metrics.totalErrors,
      avgMs: this.metrics.totalDurationMs / count,
      p50Ms: sorted[Math.floor(count * 0.5)],
      p95Ms: sorted[Math.floor(count * 0.95)],
      p99Ms: sorted[Math.floor(count * 0.99)],
      minMs: sorted[0],
      maxMs: sorted[count - 1],
    };
  }
}
