import { PerformanceState } from './types';

const RTT_WINDOW = 100;
const RATE_WINDOW_MS = 10000; // 10 seconds

export class PerformanceTracker {
  private rttBuffer: number[] = [];
  private requestTimestamps: number[] = [];
  private subUpdateTimestamps: number[] = [];
  private eventStreamMessageTimestamps: number[] = [];

  totalRequests = 0;
  totalErrors = 0;
  peakRTT = 0;
  restRequestCount = 0;
  sparqlTraceCount = 0;
  prologRequestCount = 0;

  recordRequest(duration: number, queryLanguage?: 'sparql' | 'prolog') {
    this.totalRequests++;
    this.restRequestCount++;
    this.rttBuffer.push(duration);
    if (this.rttBuffer.length > RTT_WINDOW) this.rttBuffer.shift();
    if (duration > this.peakRTT) this.peakRTT = duration;
    this.requestTimestamps.push(Date.now());
    if (queryLanguage === 'prolog') this.prologRequestCount++;
  }

  recordSparqlTrace() {
    this.sparqlTraceCount++;
  }

  recordError() {
    this.totalErrors++;
  }

  recordSubscriptionUpdate() {
    this.subUpdateTimestamps.push(Date.now());
  }

  recordEventStreamMessage() {
    this.eventStreamMessageTimestamps.push(Date.now());
  }

  private rateInWindow(timestamps: number[]): number {
    const now = Date.now();
    const cutoff = now - RATE_WINDOW_MS;
    while (timestamps.length > 0 && timestamps[0] < cutoff) timestamps.shift();
    return timestamps.length / (RATE_WINDOW_MS / 1000);
  }

  getState(estimatedMemory: number): PerformanceState {
    const avgRTT = this.rttBuffer.length > 0
      ? Math.round(this.rttBuffer.reduce((a, b) => a + b, 0) / this.rttBuffer.length)
      : 0;

    const requestsPerSecond = Math.round(this.rateInWindow(this.requestTimestamps) * 10) / 10;
    const eventStreamMessageRate = Math.round(this.rateInWindow(this.eventStreamMessageTimestamps) * 10) / 10;

    return {
      totalRequests: this.totalRequests,
      totalErrors: this.totalErrors,
      avgRTT,
      peakRTT: Math.round(this.peakRTT),
      requestsPerSecond,
      restRequestCount: this.restRequestCount,
      sparqlTraceCount: this.sparqlTraceCount,
      prologRequestCount: this.prologRequestCount,
      activeSubscriptions: 0, // filled by bridge
      subscriptionUpdateRate: Math.round(this.rateInWindow(this.subUpdateTimestamps) * 10) / 10,
      eventStreamMessageRate,
      estimatedMemory,
      totalQueries: this.totalRequests,
      queriesPerSecond: requestsPerSecond,
      sparqlQueryCount: this.sparqlTraceCount,
      prologQueryCount: this.prologRequestCount,
      wsMessageRate: eventStreamMessageRate,
    };
  }
}
