import { SubscriptionRecord } from './types';

let nextSubId = 1;

export class SubscriptionTracker {
  private subscriptions = new Map<number, SubscriptionRecord>();

  track(sub: Partial<SubscriptionRecord>): number {
    const id = nextSubId++;
    this.subscriptions.set(id, {
      id,
      query: sub.query || '',
      perspectiveUUID: sub.perspectiveUUID || '',
      modelName: sub.modelName || '',
      updateCount: 0,
      lastUpdateTimestamp: Date.now(),
      fingerprintHits: 0,
      fingerprintMisses: 0,
      callbackTimings: [],
      active: true,
      ...sub,
    });
    return id;
  }

  update(id: number, update: Partial<SubscriptionRecord>) {
    const sub = this.subscriptions.get(id);
    if (sub) Object.assign(sub, update);
  }

  remove(id: number) {
    const sub = this.subscriptions.get(id);
    if (sub) sub.active = false;
  }

  getAll(): SubscriptionRecord[] {
    return Array.from(this.subscriptions.values());
  }

  getActiveCount(): number {
    let count = 0;
    for (const s of this.subscriptions.values()) if (s.active) count++;
    return count;
  }
}
