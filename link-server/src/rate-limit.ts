/**
 * In-memory sliding-window rate limiter. No external store (Redis etc.)
 * needed — this is meant to run inside a single self-hosted server
 * process per the project brief.
 */
export interface RateLimitCheck {
  allowed: boolean;
  /** Seconds the caller should wait before retrying, only meaningful when !allowed. */
  retryAfterSec: number;
  remaining: number;
}

export class SlidingWindowLimiter {
  private hits = new Map<string, number[]>();
  private limit: number;
  private windowMs: number;
  private sweepTimer: NodeJS.Timeout;

  constructor(limit: number, windowMs: number) {
    this.limit = limit;
    this.windowMs = windowMs;
    // Periodically drop keys with no recent activity so memory doesn't grow
    // unboundedly across many distinct IPs/tokens over a long-running process.
    this.sweepTimer = setInterval(() => this.sweep(), Math.max(windowMs, 30_000));
    this.sweepTimer.unref();
  }

  check(key: string): RateLimitCheck {
    const now = Date.now();
    const windowStart = now - this.windowMs;
    let timestamps = this.hits.get(key);
    if (!timestamps) {
      timestamps = [];
      this.hits.set(key, timestamps);
    }
    // Drop entries outside the current window.
    while (timestamps.length > 0 && timestamps[0] <= windowStart) {
      timestamps.shift();
    }

    if (timestamps.length >= this.limit) {
      const oldest = timestamps[0];
      const retryAfterMs = oldest + this.windowMs - now;
      return {
        allowed: false,
        retryAfterSec: Math.max(1, Math.ceil(retryAfterMs / 1000)),
        remaining: 0,
      };
    }

    timestamps.push(now);
    return { allowed: true, retryAfterSec: 0, remaining: this.limit - timestamps.length };
  }

  private sweep(): void {
    const now = Date.now();
    const windowStart = now - this.windowMs;
    for (const [key, timestamps] of this.hits) {
      while (timestamps.length > 0 && timestamps[0] <= windowStart) {
        timestamps.shift();
      }
      if (timestamps.length === 0) this.hits.delete(key);
    }
  }

  close(): void {
    clearInterval(this.sweepTimer);
  }
}
