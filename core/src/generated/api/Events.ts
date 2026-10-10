// Auto-generated from the executor's event table (rust-executor/src/api/events_ws.rs).
// Do NOT edit manually — regenerate with: pnpm run generate:api-types

import type { AIModelLoadingStatus } from "./AIModelLoadingStatus";
import type { AgentStatusChangedEvent } from "./AgentStatusChangedEvent";
import type { AgentUpdatedEvent } from "./AgentUpdatedEvent";
import type { Apps } from "./Apps";
import type { AutoProcessorEvent } from "./AutoProcessorEvent";
import type { AutoProcessorNeighbourhoodState } from "./AutoProcessorNeighbourhoodState";
import type { ExceptionOccurredEvent } from "./ExceptionOccurredEvent";
import type { HostingUserInfo } from "./HostingUserInfo";
import type { MessageReceivedEvent } from "./MessageReceivedEvent";
import type { NeighbourhoodSignalFilter } from "./NeighbourhoodSignalFilter";
import type { NotificationTriggeredEvent } from "./NotificationTriggeredEvent";
import type { PerspectiveLinkUpdatedWithOwner } from "./PerspectiveLinkUpdatedWithOwner";
import type { PerspectiveLinkWithOwner } from "./PerspectiveLinkWithOwner";
import type { PerspectiveQuerySubscriptionFilter } from "./PerspectiveQuerySubscriptionFilter";
import type { PerspectiveRemovedWithOwner } from "./PerspectiveRemovedWithOwner";
import type { PerspectiveStateFilter } from "./PerspectiveStateFilter";
import type { PerspectiveWithOwner } from "./PerspectiveWithOwner";
import type { SfuCallRenegotiationOffer } from "./SfuCallRenegotiationOffer";
import type { SfuDataMessage } from "./SfuDataMessage";
import type { SfuMigrateEvent } from "./SfuMigrateEvent";
import type { TranscriptionTextFilter } from "./TranscriptionTextFilter";

/** Every event the executor emits: its payload (the message without `type`). */
export interface EventMap {
  "agent-status-changed": AgentStatusChangedEvent;
  "agent-updated": AgentUpdatedEvent;
  "apps-changed": Apps;
  "hosting-user-info-changed": HostingUserInfo;
  "perspective-added": PerspectiveWithOwner;
  "perspective-removed": PerspectiveRemovedWithOwner;
  "perspective-updated": PerspectiveWithOwner;
  "sync-state-change": PerspectiveStateFilter;
  "link-added": PerspectiveLinkWithOwner;
  "link-removed": PerspectiveLinkWithOwner;
  "link-updated": PerspectiveLinkUpdatedWithOwner;
  "signal": NeighbourhoodSignalFilter;
  "message-received": MessageReceivedEvent;
  "notification-triggered": NotificationTriggeredEvent;
  "exception-occurred": ExceptionOccurredEvent;
  "transcription-text": TranscriptionTextFilter;
  "model-loading-status": AIModelLoadingStatus;
  "query-subscription-update": PerspectiveQuerySubscriptionFilter;
  "auto-processor-event": AutoProcessorEvent;
  "auto-processor-neighbourhood-state": AutoProcessorNeighbourhoodState;
  "sfu-call-renegotiation-offer": SfuCallRenegotiationOffer;
  "sfu-migrate": SfuMigrateEvent;
  "sfu-data": SfuDataMessage;
}

export type EventName = keyof EventMap;

/** Events about one perspective: they carry `perspectiveUuid`. */
export type ScopedEventName = "perspective-added" | "perspective-removed" | "perspective-updated" | "sync-state-change" | "link-added" | "link-removed" | "link-updated" | "signal" | "notification-triggered" | "query-subscription-update" | "auto-processor-event" | "auto-processor-neighbourhood-state";

/** {@link ScopedEventName} as a set. */
export const SCOPED_EVENTS: ReadonlySet<EventName> = new Set<EventName>([
  "perspective-added",
  "perspective-removed",
  "perspective-updated",
  "sync-state-change",
  "link-added",
  "link-removed",
  "link-updated",
  "signal",
  "notification-triggered",
  "query-subscription-update",
  "auto-processor-event",
  "auto-processor-neighbourhood-state",
]);
