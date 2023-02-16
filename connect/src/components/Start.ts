import { html } from "lit";
import { version } from "../../package.json";
import { detectOS } from "../utils";

function downloadAd4m() {
  const windowsLink = `https://github.com/perspect3vism/ad4m/releases/download/v${version}/AD4M_${version}_x64_en-US.msi`;
  const macLink = `https://github.com/perspect3vism/ad4m/releases/download/v${version}/AD4M_${version}_x64.dmg`;
  const linuxLink = `https://github.com/perspect3vism/ad4m/releases/download/v${version}/ad4m_${version}_amd64.deb`;
  const OSName = detectOS();
  const link = document.createElement("a");
  console.log({ OSName, link });
  if (OSName === "MacOS") {
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
  connect,
  isMobile,
  hasClickedDownload,
  onDownloaded,
  changeState,
  scanQrcode,
}) {
  return html`
    <div class="items">
      ${!hasClickedDownload
        ? html`<div class="text-center">
              ${isMobile
                ? html`<button class="button" @click=${() => scanQrcode()}>
                    Connect with QR
                  </button> `
                : html`<a
                    class="button"
                    target="_blank"
                    @click=${() => {
                      downloadAd4m();
                      onDownloaded();
                    }}
                  >
                    Install AD4M
                  </a>`}
            </div>
            <div class="text-center">
              <button class="button button--link " @click=${() => connect()}>
                Try again
              </button>
              or
              <button
                class="button button--link "
                @click=${() => changeState("remoteurl")}
              >
                Connect to a remote host
              </button>
            </div>`
        : html`<div class="text-center">
            <a class="button" target="_blank" @click=${() => connect()}>
              Connect
            </a>
            <p>
              Please click try again once you have downloaded and setup your
              AD4M agent
            </p>
          </div>`}

      <div class="text-center">
        <a class="button button--link" _target="blank" href="https://ad4m.dev"
          >Learn more about AD4M</a
        >
      </div>
    </div>
  `;
}
