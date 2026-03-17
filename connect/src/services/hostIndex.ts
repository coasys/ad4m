import type { RemoteHost, UserInfo } from '../types';
import type { Ad4mClient } from '@coasys/ad4m';

const DEFAULT_INDEX_URL = "https://hosting.ad4m.dev";

function isAbsoluteUrl(value: string): boolean {
  try {
    // If this doesn't throw, it's an absolute URL
    new URL(value);
    return true;
  } catch {
    return false;
  }
}

// --- Public API ---

/** Fetch available hosts from the central index API */
export async function fetchHosts(indexUrl?: string): Promise<RemoteHost[]> {
  const url = indexUrl || DEFAULT_INDEX_URL;
  const res = await fetch(`${url}/hosts`);
  if (!res.ok) throw new Error(`Failed to fetch hosts: ${res.status}`);
  const hosts: RemoteHost[] = await res.json();
  // Resolve relative profile image URLs against the index API base URL
  for (const host of hosts) {
    if (host.profilePicUrl && !isAbsoluteUrl(host.profilePicUrl)) {
      host.profilePicUrl = new URL(host.profilePicUrl, url).toString();
    }
  }
  return hosts;
}

/** Fetch current user info from the executor via GraphQL */
export async function fetchUserInfo(ad4mClient: Ad4mClient): Promise<UserInfo> {
  const info = await ad4mClient.agent.hostingUserInfo();
  return {
    email: info.email,
    remainingCredits: info.remainingCredits === "unlimited" ? Infinity : (parseFloat(info.remainingCredits) || 0),
    hotWalletAddress: info.hotWalletAddress || null,
  };
}

/** Trigger a payment request via the executor's GraphQL API */
export async function requestPayment(
  ad4mClient: Ad4mClient,
  amountHOT: number
): Promise<{ success: boolean; message: string }> {
  return ad4mClient.agent.requestPayment(amountHOT.toString());
}
