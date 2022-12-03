import path from "path";
import { tryDownload } from "./download";

const BOOTSTRAP_LINK = "https://github.com/perspect3vism/ad4m-seeds/releases/download/0.0.5/mainnetSeed.json";

export const MAINNET_SEED = "mainnet_seed.json";

export async function fetchLatestBootstrapSeed(appDataPath: string) {
  const dest = path.join(appDataPath, MAINNET_SEED);
  return await tryDownload(BOOTSTRAP_LINK, dest);
}
