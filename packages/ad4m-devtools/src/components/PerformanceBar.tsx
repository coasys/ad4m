interface Props {
  perf: any;
  connected: boolean;
}

function formatBytes(bytes: number): string {
  if (bytes < 1024) return `${bytes}B`;
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(1)}KB`;
  return `${(bytes / (1024 * 1024)).toFixed(1)}MB`;
}

export function PerformanceBar({ perf, connected }: Props) {
  return (
    <div class="perf-bar">
      <span class="perf-item">📊 Requests: <b>{perf.totalRequests}</b></span>
      <span class="perf-item perf-errors">Errors: <b>{perf.totalErrors}</b></span>
      <span class="perf-item">Avg RTT: <b>{perf.avgRTT}ms</b></span>
      <span class="perf-item">Peak: <b>{perf.peakRTT > 1000 ? `${(perf.peakRTT / 1000).toFixed(1)}s` : `${perf.peakRTT}ms`}</b></span>
      <span class="perf-item">Req/s: <b>{perf.requestsPerSecond}</b></span>
      <span class="perf-item">Subs: <b>{perf.activeSubscriptions}</b></span>
      <span class="perf-item">Evt/s: <b>{perf.eventStreamMessageRate}</b></span>
      <span class={`perf-item ${connected ? 'perf-ok' : 'perf-err'}`}>
        REST: <b>{connected ? 'connected' : 'disconnected'}</b>
      </span>
      <span class="perf-item">Mem: <b>~{formatBytes(perf.estimatedMemory)}</b></span>
    </div>
  );
}
