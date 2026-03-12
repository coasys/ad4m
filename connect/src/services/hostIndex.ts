import type { RemoteHost, UserInfo } from '../types';

const DEFAULT_INDEX_URL = "https://hosting.ad4m.dev/api";

// --- Mock data (used until real index API + executor GraphQL are wired) ---

const MOCK_HOSTS: RemoteHost[] = [
  {
    id: "host-1",
    name: "CoasysCloud EU",
    profilePicUrl: "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI2NCIgaGVpZ2h0PSI2NCI+PHJlY3Qgd2lkdGg9IjY0IiBoZWlnaHQ9IjY0IiByeD0iOCIgZmlsbD0iIzFhMWEyZSIvPjxjaXJjbGUgY3g9IjIwIiBjeT0iMjAiIHI9IjYiIGZpbGw9IiM0ZmMzZjciIG9wYWNpdHk9Ii45Ii8+PGNpcmNsZSBjeD0iNDQiIGN5PSIxNiIgcj0iNCIgZmlsbD0iIzRmYzNmNyIgb3BhY2l0eT0iLjciLz48Y2lyY2xlIGN4PSIzMiIgY3k9IjM4IiByPSI4IiBmaWxsPSIjMjliNmY2IiBvcGFjaXR5PSIuOCIvPjxjaXJjbGUgY3g9IjUwIiBjeT0iNDIiIHI9IjUiIGZpbGw9IiM0ZmMzZjciIG9wYWNpdHk9Ii42Ii8+PGNpcmNsZSBjeD0iMTQiIGN5PSI0OCIgcj0iMyIgZmlsbD0iIzgxZDRmYSIgb3BhY2l0eT0iLjciLz48bGluZSB4MT0iMjAiIHkxPSIyMCIgeDI9IjMyIiB5Mj0iMzgiIHN0cm9rZT0iIzRmYzNmNyIgc3Ryb2tlLXdpZHRoPSIxLjUiIG9wYWNpdHk9Ii41Ii8+PGxpbmUgeDE9IjQ0IiB5MT0iMTYiIHgyPSIzMiIgeTI9IjM4IiBzdHJva2U9IiM0ZmMzZjciIHN0cm9rZS13aWR0aD0iMS41IiBvcGFjaXR5PSIuNSIvPjxsaW5lIHgxPSIzMiIgeTE9IjM4IiB4Mj0iNTAiIHkyPSI0MiIgc3Ryb2tlPSIjNGZjM2Y3IiBzdHJva2Utd2lkdGg9IjEuNSIgb3BhY2l0eT0iLjQiLz48bGluZSB4MT0iMjAiIHkxPSIyMCIgeDI9IjQ0IiB5Mj0iMTYiIHN0cm9rZT0iIzRmYzNmNyIgc3Ryb2tlLXdpZHRoPSIxIiBvcGFjaXR5PSIuMyIvPjxsaW5lIHgxPSIxNCIgeTE9IjQ4IiB4Mj0iMzIiIHkyPSIzOCIgc3Ryb2tlPSIjODFkNGZhIiBzdHJva2Utd2lkdGg9IjEiIG9wYWNpdHk9Ii4zIi8+PC9zdmc+",
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
    profilePicUrl: "",
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
    profilePicUrl: "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI2NCIgaGVpZ2h0PSI2NCI+PHJlY3Qgd2lkdGg9IjY0IiBoZWlnaHQ9IjY0IiByeD0iOCIgZmlsbD0iIzBkMjgxOCIvPjxwb2x5Z29uIHBvaW50cz0iMzIsOCA0OCwxOCA0OCwzOCAzMiw0OCAxNiwzOCAxNiwxOCIgZmlsbD0ibm9uZSIgc3Ryb2tlPSIjNGNhZjUwIiBzdHJva2Utd2lkdGg9IjIiIG9wYWNpdHk9Ii44Ii8+PHBvbHlnb24gcG9pbnRzPSIzMiwxNiA0MiwyMiA0MiwzNCAzMiw0MCAyMiwzNCAyMiwyMiIgZmlsbD0iIzJlN2QzMiIgb3BhY2l0eT0iLjUiLz48Y2lyY2xlIGN4PSIzMiIgY3k9IjI4IiByPSI2IiBmaWxsPSIjNjZiYjZhIiBvcGFjaXR5PSIuOSIvPjxsaW5lIHgxPSIzMiIgeTE9IjgiIHgyPSIzMiIgeTI9IjAiIHN0cm9rZT0iIzRjYWY1MCIgc3Ryb2tlLXdpZHRoPSIxLjUiIG9wYWNpdHk9Ii40Ii8+PGxpbmUgeDE9IjQ4IiB5MT0iMzgiIHgyPSI1NiIgeTI9IjQ0IiBzdHJva2U9IiM0Y2FmNTAiIHN0cm9rZS13aWR0aD0iMS41IiBvcGFjaXR5PSIuNCIvPjxsaW5lIHgxPSIxNiIgeTE9IjM4IiB4Mj0iOCIgeTI9IjQ0IiBzdHJva2U9IiM0Y2FmNTAiIHN0cm9rZS13aWR0aD0iMS41IiBvcGFjaXR5PSIuNCIvPjxjaXJjbGUgY3g9IjEyIiBjeT0iNTYiIHI9IjIiIGZpbGw9IiM4MWM3ODQiIG9wYWNpdHk9Ii41Ii8+PGNpcmNsZSBjeD0iNTIiIGN5PSI1NiIgcj0iMiIgZmlsbD0iIzgxYzc4NCIgb3BhY2l0eT0iLjUiLz48Y2lyY2xlIGN4PSIzMiIgY3k9IjU4IiByPSIyLjUiIGZpbGw9IiNhNWQ2YTciIG9wYWNpdHk9Ii40Ii8+PC9zdmc+",
    location: "Singapore, SG",
    url: "wss://sg1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "claude-3.5-sonnet per token", priceInHOT: 0.0000015 },
      { description: "llama-3.1-70b per token", priceInHOT: 0.0000008 },
      { description: "link write", priceInHOT: 0.0000000015 },
    ],
    aiModels: ["claude-3.5-sonnet", "llama-3.1-70b"],
  },
  {
    id: "host-4",
    name: "Nordic Relay",
    profilePicUrl: "",
    location: "Stockholm, SE",
    url: "wss://nordics.hosting.ad4m.dev/graphql",
    rates: [
      { description: "gpt-4o per token", priceInHOT: 0.0000022 },
      { description: "claude-3.5-sonnet per token", priceInHOT: 0.0000016 },
      { description: "link write", priceInHOT: 0.0000000012 },
    ],
    aiModels: ["gpt-4o", "claude-3.5-sonnet"],
  },
  {
    id: "host-5",
    name: "Holochain Bridge JP",
    profilePicUrl: "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI2NCIgaGVpZ2h0PSI2NCI+PHJlY3Qgd2lkdGg9IjY0IiBoZWlnaHQ9IjY0IiByeD0iOCIgZmlsbD0iIzFhMGEwYSIvPjxjaXJjbGUgY3g9IjQ4IiBjeT0iMTQiIHI9IjEwIiBmaWxsPSIjZDMyZjJmIiBvcGFjaXR5PSIuMyIvPjxjaXJjbGUgY3g9IjQ4IiBjeT0iMTQiIHI9IjYiIGZpbGw9IiNlZjUzNTAiIG9wYWNpdHk9Ii41Ii8+PHJlY3QgeD0iMTQiIHk9IjI0IiB3aWR0aD0iMzYiIGhlaWdodD0iNCIgcng9IjIiIGZpbGw9IiNlNTM5MzUiIG9wYWNpdHk9Ii45Ii8+PHJlY3QgeD0iMTAiIHk9IjIyIiB3aWR0aD0iNDQiIGhlaWdodD0iMyIgcng9IjEuNSIgZmlsbD0iI2VmNTM1MCIgb3BhY2l0eT0iLjciLz48cmVjdCB4PSIxOCIgeT0iMjgiIHdpZHRoPSI0IiBoZWlnaHQ9IjI4IiBmaWxsPSIjYzYyODI4IiBvcGFjaXR5PSIuOCIvPjxyZWN0IHg9IjQyIiB5PSIyOCIgd2lkdGg9IjQiIGhlaWdodD0iMjgiIGZpbGw9IiNjNjI4MjgiIG9wYWNpdHk9Ii44Ii8+PHJlY3QgeD0iMTYiIHk9IjM2IiB3aWR0aD0iMzIiIGhlaWdodD0iMi41IiByeD0iMSIgZmlsbD0iI2U1MzkzNSIgb3BhY2l0eT0iLjUiLz48Y2lyY2xlIGN4PSIzMiIgY3k9IjQ2IiByPSIzIiBmaWxsPSIjZmY4YTgwIiBvcGFjaXR5PSIuNCIvPjwvc3ZnPg==",
    location: "Tokyo, JP",
    url: "wss://jp1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "llama-3.1-70b per token", priceInHOT: 0.0000007 },
      { description: "link write", priceInHOT: 0.000000001 },
    ],
    aiModels: ["llama-3.1-70b"],
  },
  {
    id: "host-6",
    name: "MeshNode Brazil",
    profilePicUrl: "",
    location: "São Paulo, BR",
    url: "wss://br1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "gpt-4o per token", priceInHOT: 0.0000019 },
      { description: "mistral-large per token", priceInHOT: 0.0000006 },
      { description: "link write", priceInHOT: 0.0000000008 },
    ],
    aiModels: ["gpt-4o", "mistral-large"],
  },
  {
    id: "host-7",
    name: "Outback Compute",
    profilePicUrl: "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI2NCIgaGVpZ2h0PSI2NCI+PHJlY3Qgd2lkdGg9IjY0IiBoZWlnaHQ9IjY0IiByeD0iOCIgZmlsbD0iIzFhMTIwYSIvPjxlbGxpcHNlIGN4PSIzMiIgY3k9IjU2IiByeD0iMzYiIHJ5PSIxNCIgZmlsbD0iI2JmMzYwYyIgb3BhY2l0eT0iLjUiLz48ZWxsaXBzZSBjeD0iMzIiIGN5PSI1MiIgcng9IjI4IiByeT0iOCIgZmlsbD0iI2U2NTEwMCIgb3BhY2l0eT0iLjQiLz48cG9seWdvbiBwb2ludHM9IjI0LDIyIDI4LDUyIDIwLDUyIiBmaWxsPSIjZmY2ZjAwIiBvcGFjaXR5PSIuNyIvPjxwb2x5Z29uIHBvaW50cz0iMzgsMTQgNDQsNTIgMzIsNTIiIGZpbGw9IiNmZjhmMDAiIG9wYWNpdHk9Ii44Ii8+PHBvbHlnb24gcG9pbnRzPSI0OCwyOCA1Miw1MiA0NCw1MiIgZmlsbD0iI2ZmYTAwMCIgb3BhY2l0eT0iLjYiLz48Y2lyY2xlIGN4PSIxMiIgY3k9IjEyIiByPSI2IiBmaWxsPSIjZmZjYTI4IiBvcGFjaXR5PSIuNyIvPjxjaXJjbGUgY3g9IjEyIiBjeT0iMTIiIHI9IjMuNSIgZmlsbD0iI2ZmZDU0ZiIgb3BhY2l0eT0iLjkiLz48Y2lyY2xlIGN4PSI1MCIgY3k9IjIwIiByPSIxIiBmaWxsPSIjZmZlMDgyIiBvcGFjaXR5PSIuNSIvPjxjaXJjbGUgY3g9IjQ0IiBjeT0iOCIgcj0iMSIgZmlsbD0iI2ZmZTA4MiIgb3BhY2l0eT0iLjQiLz48Y2lyY2xlIGN4PSI1NiIgY3k9IjEwIiByPSIwLjgiIGZpbGw9IiNmZmUwODIiIG9wYWNpdHk9Ii4zIi8+PC9zdmc+",
    location: "Sydney, AU",
    url: "wss://au1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "claude-3.5-sonnet per token", priceInHOT: 0.0000017 },
      { description: "llama-3.1-70b per token", priceInHOT: 0.0000009 },
      { description: "link write", priceInHOT: 0.000000002 },
    ],
    aiModels: ["claude-3.5-sonnet", "llama-3.1-70b"],
  },
  {
    id: "host-8",
    name: "Sovereign Stack UK",
    profilePicUrl: "",
    location: "London, GB",
    url: "wss://uk1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "gpt-4o per token", priceInHOT: 0.000002 },
      { description: "claude-3.5-sonnet per token", priceInHOT: 0.0000015 },
      { description: "gemini-2.0-flash per token", priceInHOT: 0.0000004 },
      { description: "link write", priceInHOT: 0.000000001 },
    ],
    aiModels: ["gpt-4o", "claude-3.5-sonnet", "gemini-2.0-flash"],
  },
  {
    id: "host-9",
    name: "Cape Distributed",
    profilePicUrl: "data:image/svg+xml;base64,PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHdpZHRoPSI2NCIgaGVpZ2h0PSI2NCI+PHJlY3Qgd2lkdGg9IjY0IiBoZWlnaHQ9IjY0IiByeD0iOCIgZmlsbD0iIzBkMGExYSIvPjxwb2x5Z29uIHBvaW50cz0iMCw1NiAxOCwyNCAzNiw1NiIgZmlsbD0iIzRhMTQ4YyIgb3BhY2l0eT0iLjciLz48cG9seWdvbiBwb2ludHM9IjIwLDU2IDM4LDE4IDU2LDU2IiBmaWxsPSIjNmExYjlhIiBvcGFjaXR5PSIuOCIvPjxwb2x5Z29uIHBvaW50cz0iMzYsNTYgNTIsMzAgNjQsNTYiIGZpbGw9IiM3YjFmYTIiIG9wYWNpdHk9Ii42Ii8+PHBhdGggZD0iTTAsNDggUTE2LDQwIDMyLDQ4IFE0OCw1NiA2NCw0OCBMNjQsNTYgTDAsNTYgWiIgZmlsbD0iIzljMjdiMCIgb3BhY2l0eT0iLjMiLz48cGF0aCBkPSJNMCw1MiBRMTYsNDQgMzIsNTIgUTQ4LDYwIDY0LDUyIEw2NCw1NiBMMCw1NiBaIiBmaWxsPSIjYWI0N2JjIiBvcGFjaXR5PSIuMjUiLz48Y2lyY2xlIGN4PSI1MCIgY3k9IjEwIiByPSI0IiBmaWxsPSIjY2U5M2Q4IiBvcGFjaXR5PSIuNiIvPjxjaXJjbGUgY3g9IjUwIiBjeT0iMTAiIHI9IjIiIGZpbGw9IiNlMWJlZTciIG9wYWNpdHk9Ii44Ii8+PGNpcmNsZSBjeD0iMTAiIGN5PSIxNCIgcj0iMSIgZmlsbD0iI2NlOTNkOCIgb3BhY2l0eT0iLjQiLz48Y2lyY2xlIGN4PSIzMCIgY3k9IjgiIHI9IjAuOCIgZmlsbD0iI2NlOTNkOCIgb3BhY2l0eT0iLjMiLz48Y2lyY2xlIGN4PSI0MiIgY3k9IjYiIHI9IjAuNiIgZmlsbD0iI2UxYmVlNyIgb3BhY2l0eT0iLjMiLz48L3N2Zz4=",
    location: "Cape Town, ZA",
    url: "wss://za1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "llama-3.1-70b per token", priceInHOT: 0.0000006 },
      { description: "link write", priceInHOT: 0.0000000005 },
    ],
    aiModels: ["llama-3.1-70b"],
  },
  {
    id: "host-10",
    name: "Maple Leaf Node",
    profilePicUrl: "",
    location: "Toronto, CA",
    url: "wss://ca1.hosting.ad4m.dev/graphql",
    rates: [
      { description: "gpt-4o per token", priceInHOT: 0.0000021 },
      { description: "claude-3.5-sonnet per token", priceInHOT: 0.0000016 },
      { description: "llama-3.1-70b per token", priceInHOT: 0.00000085 },
      { description: "link write", priceInHOT: 0.0000000011 },
    ],
    aiModels: ["gpt-4o", "claude-3.5-sonnet", "llama-3.1-70b"],
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
