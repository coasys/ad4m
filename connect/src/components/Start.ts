import { html } from "lit";
import { version } from "../../package.json";
import { detectOS } from "../utils";

function downloadAd4m() {
  const windowsLink = `https://github.com/perspect3vism/ad4m/releases/download/v${version}/AD4Min_${version}_x64_en-US.msi`;
  const macLink = `https://github.com/perspect3vism/ad4m/releases/download/v${version}/AD4Min_${version}_x64.dmg`;
  const linuxLink = `https://github.com/perspect3vism/ad4m/releases/download/v${version}/ad4-min_${version}_amd64.deb`;
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
              Install AD4M
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

      <div class="text-center">
          <a class="button button--link" _target="blank" href="https://ad4m.dev">Learn more about AD4M</a>
      </div>
    </div>
  `;
}
