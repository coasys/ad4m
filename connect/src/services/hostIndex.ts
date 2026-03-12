import type { RemoteHost, UserInfo } from '../types';

const DEFAULT_INDEX_URL = "https://hosting.ad4m.dev/api";

// --- Mock data (used until real index API + executor GraphQL are wired) ---

const MOCK_HOSTS: RemoteHost[] = [
  {
    id: "host-1",
    name: "CoasysCloud EU",
    profilePicUrl: "https://api.dicebear.com/7.x/identicon/svg?seed=coasys-eu",
    location: "Frankfurt, DE",
    url: "wss://eu1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "gpt-4o per token", priceInHOT: 0.000002 },
      { description: "claude-3.5-sonnet per token", priceInHOT: 0.0000015 },
      { description: "link write", priceInHOT: 0.000000001 },
    ],
    aiModels: ["gpt-4o", "claude-3.5-sonnet"],
  },
  {
    id: "host-2",
    name: "AD4M Host US-East",
    profilePicUrl: "https://api.dicebear.com/7.x/identicon/svg?seed=ad4m-us",
    location: "Virginia, US",
    url: "wss://us1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "gpt-4o per token", priceInHOT: 0.0000018 },
      { description: "link write", priceInHOT: 0.000000001 },
    ],
    aiModels: ["gpt-4o"],
  },
  {
    id: "host-3",
    name: "Decentralized Node APAC",
    profilePicUrl: "https://api.dicebear.com/7.x/identicon/svg?seed=decentral-apac",
    location: "Singapore, SG",
    url: "wss://sg1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "claude-3.5-sonnet per token", priceInHOT: 0.0000015 },
      { description: "llama-3.1-70b per token", priceInHOT: 0.0000008 },
      { description: "link write", priceInHOT: 0.0000000015 },
    ],
    aiModels: ["claude-3.5-sonnet", "llama-3.1-70b"],
  },
];

const USE_MOCK = true;

// --- Public API ---

/** Fetch available hosts from the central index API */
export async function fetchHosts(indexUrl?: string): Promise<RemoteHost[]> {
  if (USE_MOCK) {
    // Simulate network delay
    await new Promise(r => setTimeout(r, 600));
    return MOCK_HOSTS;
  }

  const url = indexUrl || DEFAULT_INDEX_URL;
  const res = await fetch(`${url}/hosts`);
  if (!res.ok) throw new Error(`Failed to fetch hosts: ${res.status}`);
  return res.json();
}

/** Fetch current user info from an authenticated host (polled every 30s for dashboard) */
export async function fetchUserInfo(hostUrl: string, token: string): Promise<UserInfo> {
  if (USE_MOCK) {
    return {
      email: "test@example.com",
      remainingCredits: 100,
      hotWalletAddress: null,
    };
  }

  // Convert wss:// GraphQL URL to https:// REST endpoint
  const baseUrl = hostUrl.replace(/^wss?:\/\//, 'https://').replace(/\/graphql$/, '');
  const res = await fetch(`${baseUrl}/api/user-info`, {
    headers: { Authorization: `Bearer ${token}` },
  });
  if (!res.ok) throw new Error(`Failed to fetch user info: ${res.status}`);
  return res.json();
}

/** Trigger a payment request — executor sends a Unit payment request to the user's HOT address */
export async function requestPayment(
  hostUrl: string,
  token: string,
  amountHOT: number
): Promise<{ success: boolean; message: string }> {
  if (USE_MOCK) {
    await new Promise(r => setTimeout(r, 800));
    return { success: true, message: "Payment request sent to Unit app" };
  }

  const baseUrl = hostUrl.replace(/^wss?:\/\//, 'https://').replace(/\/graphql$/, '');
  const res = await fetch(`${baseUrl}/api/request-payment`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      Authorization: `Bearer ${token}`,
    },
    body: JSON.stringify({ amountHOT }),
  });
  if (!res.ok) throw new Error(`Payment request failed: ${res.status}`);
  return res.json();
}
