import { html } from "lit";

type ConnectionOverviewProps = {
  localDetected: boolean;
  multiUserConfigured: boolean;
  backendUrl?: string;
  configuredUrl?: string;
  isMobile: boolean;
  onConnectLocal: () => void;
  onConnectRemote: () => void;
  onScanQR: () => void;
  onDownloadAd4m: () => void;
};

export default function ConnectionOverview({
  localDetected,
  multiUserConfigured,
  backendUrl,
  configuredUrl,
  isMobile,
  onConnectLocal,
  onConnectRemote,
  onScanQR,
  onDownloadAd4m,
}: ConnectionOverviewProps) {
  // Determine what the primary action should be
  const showLocalOption = !isMobile && localDetected;
  const showRemoteOption = true; // Always available
  const showQROption = isMobile;
  const showDownload = !localDetected && !multiUserConfigured;

  // Determine which URL to show (backendUrl takes priority)
  const displayUrl = backendUrl || configuredUrl;

  return html`
    <div class="items">
      <div class="text-center">
        <h3 class="heading">Connect to AD4M</h3>
        ${!localDetected
          ? html`
              <p class="body" style="color: var(--body-color); opacity: 0.8;">
                No local AD4M detected.
                ${!isMobile
                  ? html`Download AD4M or connect to a remote executor.`
                  : html`Connect to a remote executor or scan a QR code.`}
              </p>
            `
          : html`
              <p class="body">Choose how you want to connect:</p>
            `}
      </div>

      <div class="items items--small">
        ${showLocalOption
          ? html`
              <div class="connection-option connection-option--primary">
                <div class="connection-option__header">
                  <svg
                    width="24"
                    height="24"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="currentColor"
                  >
                    <circle cx="12" cy="12" r="10"></circle>
                    <circle cx="12" cy="12" r="3" fill="currentColor"></circle>
                  </svg>
                  <h4 class="heading nomargin">Local AD4M</h4>
                </div>
                <p class="body" style="margin: 10px 0 15px 0; font-size: 13px;">
                  Connect to your local executor on port 12000
                </p>
                <button
                  class="button button--full"
                  @click=${onConnectLocal}
                >
                  Connect to Local AD4M
                </button>
              </div>
            `
          : ""}

        ${showRemoteOption
          ? html`
              <div
                class="connection-option ${!showLocalOption
                  ? "connection-option--primary"
                  : ""}"
              >
                <div class="connection-option__header">
                  <svg
                    width="24"
                    height="24"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="currentColor"
                  >
                    <circle cx="12" cy="12" r="10"></circle>
                    <path d="M2 12h20"></path>
                    <path
                      d="M12 2a15.3 15.3 0 0 1 4 10 15.3 15.3 0 0 1-4 10 15.3 15.3 0 0 1-4-10 15.3 15.3 0 0 1 4-10z"
                    ></path>
                  </svg>
                  <h4 class="heading nomargin">
                    ${displayUrl ? "Configured AD4M Node" : "Remote AD4M"}
                  </h4>
                </div>
                ${displayUrl
                  ? html`
                      <p
                        class="body"
                        style="margin: 10px 0 5px 0; font-size: 13px;"
                      >
                        ${multiUserConfigured
                          ? "Multi-user backend configured"
                          : "Connect to AD4M node at URL"}
                      </p>
                      <p
                        class="body"
                        style="margin: 0 0 15px 0; font-size: 12px; opacity: 0.7; font-family: monospace;"
                      >
                        ${displayUrl}
                      </p>
                    `
                  : html`
                      <p
                        class="body"
                        style="margin: 10px 0 15px 0; font-size: 13px;"
                      >
                        Connect to a remote executor or multi-user backend
                      </p>
                    `}
                <button
                  class="button ${showLocalOption
                    ? "button--secondary"
                    : ""} button--full"
                  @click=${onConnectRemote}
                >
                  ${displayUrl ? "Connect to Configured Node" : "Enter Remote URL"}
                </button>
              </div>
            `
          : ""}

        ${showQROption
          ? html`
              <div class="connection-option">
                <div class="connection-option__header">
                  <svg
                    width="24"
                    height="24"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="currentColor"
                  >
                    <rect x="3" y="3" width="7" height="7"></rect>
                    <rect x="14" y="3" width="7" height="7"></rect>
                    <rect x="14" y="14" width="7" height="7"></rect>
                    <rect x="3" y="14" width="7" height="7"></rect>
                  </svg>
                  <h4 class="heading nomargin">Scan QR Code</h4>
                </div>
                <p class="body" style="margin: 10px 0 15px 0; font-size: 13px;">
                  Scan a QR code from another device
                </p>
                <button
                  class="button button--secondary button--full"
                  @click=${onScanQR}
                >
                  Scan QR Code
                </button>
              </div>
            `
          : ""}

        ${showDownload
          ? html`
              <div class="connection-option connection-option--download">
                <div class="connection-option__header">
                  <svg
                    width="24"
                    height="24"
                    viewBox="0 0 24 24"
                    fill="none"
                    stroke="currentColor"
                  >
                    <path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"></path>
                    <polyline points="7 10 12 15 17 10"></polyline>
                    <line x1="12" y1="15" x2="12" y2="3"></line>
                  </svg>
                  <h4 class="heading nomargin">Get AD4M</h4>
                </div>
                <p class="body" style="margin: 10px 0 15px 0; font-size: 13px;">
                  Download and install AD4M on your device
                </p>
                <button
                  class="button button--full"
                  @click=${onDownloadAd4m}
                >
                  Download AD4M
                </button>
              </div>
            `
          : ""}
      </div>
    </div>

    <style>
      .connection-option {
        background: rgba(145, 227, 253, 0.05);
        border: 1px solid rgba(145, 227, 253, 0.2);
        border-radius: 8px;
        padding: 20px;
        transition: all 0.3s ease;
      }

      .connection-option:hover {
        border-color: rgba(145, 227, 253, 0.4);
        background: rgba(145, 227, 253, 0.08);
      }

      .connection-option--primary {
        border: 2px solid var(--gradient);
        background: rgba(145, 227, 253, 0.1);
      }

      .connection-option--primary:hover {
        background: rgba(145, 227, 253, 0.15);
      }

      .connection-option--download {
        border-style: dashed;
        opacity: 0.9;
      }

      .connection-option__header {
        display: flex;
        align-items: center;
        gap: 10px;
        margin-bottom: 5px;
      }

      .connection-option__header svg {
        flex-shrink: 0;
        opacity: 0.8;
      }

      .connection-option--primary .connection-option__header svg {
        color: var(--gradient);
        opacity: 1;
      }
    </style>
  `;
}
