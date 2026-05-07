import { PerspectiveExpression } from "../perspectives/Perspective";
import { ExceptionType } from "../Exception";

export class SentMessage {
    recipient: string;
    message: PerspectiveExpression;
}

export class RuntimeInfo {
    ad4mExecutorVersion: string;
    isInitialized: Boolean;
    isUnlocked: Boolean;
}

export class ExceptionInfo {
    title: string;
    message: string;
    type: ExceptionType;
    addon?: string;
}

export class NotificationInput {
    description: string;
    appName: string;
    appUrl: string;
    appIconPath: string;

    // This is a query which will be executed on every perspective change.
    // All matched unbound variables will be part of the triggerMatch, i.e.
    // the content that will be sent to the launcher via event push
    // and to the webhook.
    trigger: string;

    // List Perspectives this Notification is active on.
    perspectiveIds: string[];

    // URL to which the notification matches will be sent via POST
    webhookUrl: string;

    // Authentication bearer token to be sent via POST to the webhookUrl.
    webhookAuth: string;
}

export class Notification {
    id: string;    
    granted: boolean;
    description: string;
    appName: string;
    appUrl: string;
    appIconPath: string;
    trigger: string;
    perspectiveIds: string[];
    webhookUrl: string;
    webhookAuth: string;
}

export class TriggeredNotification {
    notification: Notification;
    perspectiveId: string;
    triggerMatch: string;
}

export class ImportStats {
    total: number;
    imported: number;
    failed: number;
    omitted: number;
    errors: string[];
}

export class ImportResult {
    perspectives: ImportStats;
    links: ImportStats;
    expressions: ImportStats;
    perspectiveDiffs: ImportStats;
    notifications: ImportStats;
    models: ImportStats;
    defaultModels: ImportStats;
    tasks: ImportStats;
    friends: ImportStats;
    trustedAgents: ImportStats;
    knownLinkLanguages: ImportStats;
}

export class UserStatistics {
    email: string;
    did: string;
    lastSeen?: string;
    perspectiveCount: number;
    remainingCredits: string;
    freeAccess: boolean;
    hotWalletAddress?: string;
}

export class VerificationRequestResult {
    success: boolean;
    message: string;
    requiresPassword: boolean;
    isExistingUser: boolean;
}

export class HostingUserInfo {
    email: string;
    remainingCredits: string;
    hotWalletAddress?: string;
    freeAccess: boolean;

    constructor(email: string, remainingCredits: string, hotWalletAddress?: string, freeAccess: boolean = false) {
        this.email = email;
        this.remainingCredits = remainingCredits;
        this.hotWalletAddress = hotWalletAddress;
        this.freeAccess = freeAccess;
    }
}

export class PaymentRequestResult {
    success: boolean;
    message: string;

    constructor(success: boolean, message: string) {
        this.success = success;
        this.message = message;
    }
}

export class ComputeLogEntry {
    id: number;
    userEmail: string;
    timestamp: string;
    operation: string;
    summary?: string;
    cost: number;
    creditsAfter: number;

    constructor(id: number, userEmail: string, timestamp: string, operation: string, cost: number, creditsAfter: number, summary?: string) {
        this.id = id;
        this.userEmail = userEmail;
        this.timestamp = timestamp;
        this.operation = operation;
        this.cost = cost;
        this.creditsAfter = creditsAfter;
        this.summary = summary;
    }
}
