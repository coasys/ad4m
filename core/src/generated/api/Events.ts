// Auto-generated from the executor's event table (rust-executor/src/api/events_ws.rs).
// Do NOT edit manually — regenerate with: pnpm run generate:api-types

import type { AIModelLoadingStatus } from "./AIModelLoadingStatus";
import type { AgentStatusChangedEvent } from "./AgentStatusChangedEvent";
import type { AgentUpdatedEvent } from "./AgentUpdatedEvent";
import type { Apps } from "./Apps";
import type { AutoProcessorEvent } from "./AutoProcessorEvent";
import type { AutoProcessorNeighbourhoodState } from "./AutoProcessorNeighbourhoodState";
import type { ExceptionOccurredEvent } from "./ExceptionOccurredEvent";
import type { FlowStateChangedEvent } from "./FlowStateChangedEvent";
import type { HostingUserInfo } from "./HostingUserInfo";
import type { IdentityForkEvent } from "./IdentityForkEvent";
import type { NeighbourhoodSignalFilter } from "./NeighbourhoodSignalFilter";
import type { NotificationTriggeredEvent } from "./NotificationTriggeredEvent";
import type { PerspectiveEvent } from "./PerspectiveEvent";
import type { PerspectiveLinkEvent } from "./PerspectiveLinkEvent";
import type { PerspectiveLinkUpdatedEvent } from "./PerspectiveLinkUpdatedEvent";
import type { PerspectiveRemovedEvent } from "./PerspectiveRemovedEvent";
import type { PerspectiveStateFilter } from "./PerspectiveStateFilter";
import type { QuerySubscriptionUpdateEvent } from "./QuerySubscriptionUpdateEvent";
import type { SfuCallRenegotiationOffer } from "./SfuCallRenegotiationOffer";
import type { SfuDataMessage } from "./SfuDataMessage";
import type { SfuMigrateEvent } from "./SfuMigrateEvent";
import type { SfuRoomRoster } from "./SfuRoomRoster";
import type { TranscriptionTextFilter } from "./TranscriptionTextFilter";

/** Every event the executor emits: its payload (the message without `type`). */
export interface EventMap {
  "agent-status-changed": AgentStatusChangedEvent;
  "agent-updated": AgentUpdatedEvent;
  "apps-changed": Apps;
  "hosting-user-info-changed": HostingUserInfo;
  "perspective-added": PerspectiveEvent;
  "perspective-removed": PerspectiveRemovedEvent;
  "perspective-updated": PerspectiveEvent;
  "sync-state-change": PerspectiveStateFilter;
  "link-added": PerspectiveLinkEvent;
  "link-removed": PerspectiveLinkEvent;
  "link-updated": PerspectiveLinkUpdatedEvent;
  "signal": NeighbourhoodSignalFilter;
  "notification-triggered": NotificationTriggeredEvent;
  "exception-occurred": ExceptionOccurredEvent;
  "transcription-text": TranscriptionTextFilter;
  "model-loading-status": AIModelLoadingStatus;
  "query-subscription-update": QuerySubscriptionUpdateEvent;
  "auto-processor-event": AutoProcessorEvent;
  "auto-processor-neighbourhood-state": AutoProcessorNeighbourhoodState;
  "sfu-call-renegotiation-offer": SfuCallRenegotiationOffer;
  "sfu-migrate": SfuMigrateEvent;
  "sfu-data": SfuDataMessage;
  "sfu-room-roster": SfuRoomRoster;
  "flow-state-changed": FlowStateChangedEvent;
  "identity-fork": IdentityForkEvent;
}

export type EventName = keyof EventMap;

/** Events about one perspective: they carry `perspectiveUuid`. */
export type ScopedEventName = "perspective-added" | "perspective-removed" | "perspective-updated" | "sync-state-change" | "link-added" | "link-removed" | "link-updated" | "signal" | "notification-triggered" | "query-subscription-update" | "auto-processor-event" | "auto-processor-neighbourhood-state" | "flow-state-changed";

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
  "flow-state-changed",
]);
