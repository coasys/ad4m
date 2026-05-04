import { CapabilityInput } from '@coasys/ad4m';

export type AppInfo = {
  name: string;
  description: string;
  url: string;
  iconPath?: string;
};

export type Ad4mConnectOptions = {
  appInfo: AppInfo;
  capabilities: CapabilityInput[];
  dataPath?: string;
  port?: number;
  // token?: string;
  url?: string;
  hosting?: boolean;
  mobile?: boolean;
  // Multi-user options
  multiUser?: boolean;
  remoteUrl?: string;
  userEmail?: string;
  userPassword?: string;
  // Security options for embedded mode
  allowedOrigins?: string[];
  // Hosting options
  hostIndexUrl?: string;     // URL of the central host index API
  lowCreditThreshold?: number; // Credits level at which to warn the user (default: 10)
  // Credit lifecycle callbacks
  onCreditsDepleted?: () => void; // Called once when credits reach zero and the modal is opened
  onUseApp?: () => void;          // Called when the user clicks "Use App" after a credit-depletion session
};

export type ConfigStates = "port" | "url" | "token";
export type AuthStates = "authenticated" | "locked" | "unauthenticated";
export type ConnectionStates = "connecting" | "connected" | "error" | "port-not-found" | "not-connected" | "disconnected" | "checking-local";

/** Host as returned by the central index API */
export type RemoteHost = {
  id: string;
  name: string;
  description?: string;      // host description
  profilePicUrl: string;
  location: string;          // e.g. "Frankfurt, DE"
  url: string;               // API endpoint
  rates: PricingItem[];      // flexible pricing array
  aiModels: string[];        // e.g. ["gpt-4o", "claude-3.5-sonnet"]
  computeSpecs?: string | null; // e.g. "8 vCPU, 32GB RAM"
};

/** Flexible pricing item — avoids locking the interface to a specific pricing model */
export type PricingItem = {
  description: string;       // e.g. "gpt-4o per token", "link write", "SPARQL query"
  priceInHOT: number;        // e.g. 0.000001
};

/** User account data returned by the host after auth */
export type UserInfo = {
  email: string;
  remainingCredits: number;  // in wHOT equivalent
  hotWalletAddress: string | null;  // user's wHOT public address (null until set)
  freeAccess: boolean;
};

/** A single compute activity log entry */
export type ComputeLogEntryData = {
  id: number;
  userEmail: string;
  timestamp: string;
  operation: string;
  summary: string | null;
  cost: number;
  creditsAfter: number;
};