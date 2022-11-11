import { html } from "lit";

function downloadAd4m() {
  const windowsLink =
    "https://github.com/perspect3vism/ad4m/releases/download/v0.2.0/AD4Min_0.2.0_x64_en-US.msi";
  const macLink =
    "https://github.com/perspect3vism/ad4m/releases/download/v0.2.0/AD4Min_0.2.0_x64.dmg";
  const linuxLink =
    "https://github.com/perspect3vism/ad4m/releases/download/v0.2.0/ad4-min_0.2.0_amd64.deb";
  let OSName = "Unkown";
  if (window.navigator.userAgent.indexOf("Windows NT 10.0") !== -1)
    OSName = "Windows";
  if (window.navigator.userAgent.indexOf("Windows NT 6.3") !== -1)
    OSName = "Windows";
  if (window.navigator.userAgent.indexOf("Windows NT 6.2") !== -1)
    OSName = "Windows";
  if (window.navigator.userAgent.indexOf("Windows NT 6.1") !== -1)
    OSName = "Windows";
  if (window.navigator.userAgent.indexOf("Windows NT 6.0") !== -1)
    OSName = "Windows";
  if (window.navigator.userAgent.indexOf("Windows NT 5.1") !== -1)
    OSName = "Windows";
  if (window.navigator.userAgent.indexOf("Windows NT 5.0") !== -1)
    OSName = "Windows";
  if (window.navigator.userAgent.indexOf("Mac") !== -1) OSName = "Mac";
  if (window.navigator.userAgent.indexOf("X11") !== -1) OSName = "UNIX";
  if (window.navigator.userAgent.indexOf("Linux") !== -1) OSName = "Linux";
  const link = document.createElement("a");
  if (OSName === "Mac") {
    link.setAttribute("href", macLink);
    link.click();
  }
  if (OSName === "UNIX" || OSName === "Linux") {
    link.setAttribute("href", linuxLink);
    link.click();
  }
  if (OSName === "Windows") {
    link.setAttribute("href", windowsLink);
    link.click();
  }
}

export default function Start({
  connectToPort,
  isMobile,
  changeState,
  scanQrcode,
}) {
  return html`
    <div class="items">
      <div class="text-center">
        ${!isMobile
          ? html`<a
              class="button"
              target="_blank"
              @click=${() => downloadAd4m()}
            >
              Install AD4M Extension
            </a>`
          : html`<button class="button" @click=${() => scanQrcode()}>
              Connect with QR
            </button> `}
      </div>

      <div class="text-center">
        <button class="button button--link " @click=${() => connectToPort()}>
          Reconnect
        </button>
        or
        <button
          class="button button--link "
          @click=${() => changeState("remote_url")}
        >
          Connect to a remote host
        </button>
      </div>
    </div>
  `;
}
