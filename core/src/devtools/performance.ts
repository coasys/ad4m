import { PerformanceState } from './types';

const RTT_WINDOW = 100;
const QPS_WINDOW = 10000; // 10 seconds

export class PerformanceTracker {
  private rttBuffer: number[] = [];
  private queryTimestamps: number[] = [];
  private subUpdateTimestamps: number[] = [];
  private wsMessageTimestamps: number[] = [];
  
  totalQueries = 0;
  totalErrors = 0;
  peakRTT = 0;
  sparqlQueryCount = 0;
  prologQueryCount = 0;

  recordQuery(duration: number, type: 'sparql' | 'prolog' | 'graphql' = 'graphql') {
    this.totalQueries++;
    this.rttBuffer.push(duration);
    if (this.rttBuffer.length > RTT_WINDOW) this.rttBuffer.shift();
    if (duration > this.peakRTT) this.peakRTT = duration;
    this.queryTimestamps.push(Date.now());
    if (type === 'sparql') this.sparqlQueryCount++;
    else if (type === 'prolog') this.prologQueryCount++;
  }

  recordError() { this.totalErrors++; }
  recordSubscriptionUpdate() { this.subUpdateTimestamps.push(Date.now()); }
  recordWsMessage() { this.wsMessageTimestamps.push(Date.now()); }

  private rateInWindow(timestamps: number[]): number {
    const now = Date.now();
    const cutoff = now - QPS_WINDOW;
    while (timestamps.length > 0 && timestamps[0] < cutoff) timestamps.shift();
    return timestamps.length / (QPS_WINDOW / 1000);
  }

  getState(estimatedMemory: number): PerformanceState {
    const avgRTT = this.rttBuffer.length > 0
      ? Math.round(this.rttBuffer.reduce((a, b) => a + b, 0) / this.rttBuffer.length)
      : 0;

    return {
      totalQueries: this.totalQueries,
      totalErrors: this.totalErrors,
      avgRTT,
      peakRTT: Math.round(this.peakRTT),
      queriesPerSecond: Math.round(this.rateInWindow(this.queryTimestamps) * 10) / 10,
      sparqlQueryCount: this.sparqlQueryCount,
      prologQueryCount: this.prologQueryCount,
      activeSubscriptions: 0, // filled by bridge
      subscriptionUpdateRate: Math.round(this.rateInWindow(this.subUpdateTimestamps) * 10) / 10,
      wsMessageRate: Math.round(this.rateInWindow(this.wsMessageTimestamps) * 10) / 10,
      estimatedMemory,
    };
  }
}
