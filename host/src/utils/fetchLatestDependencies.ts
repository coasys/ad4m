import os from 'os';
import { tryDownload } from "./download";

const HOLOCHAIN_DEP_LINUX = "https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/holochain-linux-0.0.161";
const HC_DEP_LINUX = "https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/hc-linux-0.0.56";
const LAIR_DEP_LINUX = "https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/lair-keystore-linux-0.2.0"

const HOLOCHAIN_DEP_MAC = "https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/holochain-darwin-0.0.161";
const HC_DEP_MAC = "https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/hc-darwin-0.0.56";
const LAIR_DEP_MAC = "https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/lair-keystore-darwin-0.2.0"

const HOLOCHAIN_DEP_WINDOWS = "https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/holochain-windows-0.0.161.exe";
const HC_DEP_WINDOWS = "https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/hc-windows-0.0.56.exe";
const LAIR_DEP_WINDOWS = "https://github.com/perspect3vism/ad4m-host/releases/download/binary-deps-0.0.161/lair-keystore-windows-0.2.0.exe"

export async function fetchLatestDependencies(holochainDest?: string, lairDest?: string, hcDest?: string) {
    let holochainDownloadLink = "";
    let hcDownloadLink = "";
    let lairDownloadLink = "";
    switch (os.platform()) {
        case "linux":
            holochainDownloadLink = HOLOCHAIN_DEP_LINUX;
            hcDownloadLink = HC_DEP_LINUX;
            lairDownloadLink = LAIR_DEP_LINUX;
            break;
        case "darwin":
            holochainDownloadLink = HOLOCHAIN_DEP_MAC;
            hcDownloadLink = HC_DEP_MAC;
            lairDownloadLink = LAIR_DEP_MAC;
            break;
        case "win32":
            holochainDownloadLink = HOLOCHAIN_DEP_WINDOWS;
            hcDownloadLink = HC_DEP_WINDOWS;
            lairDownloadLink = LAIR_DEP_WINDOWS;
            break;
    }

    if (holochainDest) { await tryDownload(holochainDownloadLink, holochainDest); }
    if (hcDest) { await tryDownload(hcDownloadLink, hcDest); }
    if (lairDest) { await tryDownload(lairDownloadLink, lairDest); }
}
