import { NotificationRecord } from './types';

export class NotificationMonitor {
  private notifications = new Map<string, NotificationRecord>();

  register(notification: NotificationRecord) {
    this.notifications.set(notification.id, notification);
  }

  update(id: string, update: Partial<NotificationRecord>) {
    const n = this.notifications.get(id);
    if (n) Object.assign(n, update);
  }

  getAll(): NotificationRecord[] {
    return Array.from(this.notifications.values());
  }
}
