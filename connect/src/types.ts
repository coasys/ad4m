import { Ad4mClient, CapabilityInput } from '@coasys/ad4m';

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
};

export type ConfigStates = "port" | "url" | "token";
export type AuthStates = "authenticated" | "locked" | "unauthenticated";
export type ConnectionStates = "connecting" | "connected" | "error" | "port-not-found" | "not-connected" | "disconnected" | "checking-local";