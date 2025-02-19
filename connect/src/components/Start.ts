import { html } from "lit";
import { getForVersion, DEFAULT_PORT } from "../utils";

export default function Start({
  connect,
  isMobile,
  hasClickedDownload,
  changeState,
  onDownloaded,
  scanQrcode,
  hosting
}) {
  const url = getForVersion('ad4murl') || `http://localhost:${DEFAULT_PORT}`;
  const isLocal = url.includes("localhost");

  function clickLink(e: Event) {
    e.preventDefault();
    const el = e.currentTarget as HTMLLinkElement;
    window.open(el.href, "_blank");
    this.onDownloaded();
  }

  return html`
    <div class="items">
      ${!hasClickedDownload
        ? html`
          <div class="text-center">
          ${isMobile
            ? html`<button class="button" @click=${() => scanQrcode()}>
                Connect with QR
              </button> `
            : html`<a
                class="button"
                target="_blank"
                @click=${clickLink}
                href="https://github.com/coasys/ad4m/releases"
              >
                Download AD4M
              </a>`}
              ${hosting ? html`<p>Or</p>` : ""}
              ${hosting ? html`<button class="button" @click=${() => changeState("hosting")}>Use hosted AD4M (alpha)</button>` : ""}
          </div>
            <div class="text-center">
              <button class="button button--secondary" @click=${() => connect()}>
                Try again
              </button>
              or
              <button
                class="button button--secondary "
                @click=${() => changeState("settings")}
              >
                Change connection settings
              </button>

            </div>`
        : html`<div class="text-center">
            <a class="button" target="_blank" @click=${() => connect()}>
              Connect to ADAM
            </a>
            <p>
              Please connect to ADAM once you have downloaded and setup your
              ADAM agent
            </p>
          </div>`}

          ${isLocal ? html`
            <p style="height: 60px; color: red; font-size: 14px; text-align: center; margin: 0; margin-top: -30px;">
              Please make sure you have the ADAM Launcher running on your computer and there are no browser restrictions ("Shields") blocking your connection to ADAM on localhost.
              (Safari users: please use a different browser for now. Safari is very strict about this and we are working on a solution.)
            </p>
            ` : html`
            <p style="height: 60px; color: red; font-size: 14px; text-align: center; margin: 0; margin-top: -30px;">
              Looks like the remote executor you are trying to connect to is not reachable.
              Please try connecting to a different remote executor or check your connection settings.
            </p>
            `
          }

      <div class="text-center">
        <a class="button button--secondary" _target="blank" href="https://ad4m.dev"
          >Learn more about ADAM</a
        >
      </div>
    </div>
  `;
}
