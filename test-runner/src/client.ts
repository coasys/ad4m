import { Ad4mClient } from '@coasys/ad4m';

export function buildAd4mClient(port: number): Ad4mClient {
  const token = global.ad4mToken;
  return new Ad4mClient(`http://localhost:${port}`, token, false)
}