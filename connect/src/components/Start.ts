import { html } from "lit";

export default function Start({
  connect,
  isMobile,
  hasClickedDownload,
  changeState,
  onDownloaded,
  scanQrcode,
}) {
  function clickLink(e: Event) {
    e.preventDefault();
    const el = e.currentTarget as HTMLLinkElement;
    window.open(el.href, "_blank");
    this.onDownloaded();
  }

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
                    @click=${clickLink}
                    href="https://ad4m.dev/download"
                  >
                    Download AD4M
                  </a>`}
            </div>
            <div class="text-center">
              <button class="button button--link " @click=${() => connect()}>
                Try again
              </button>
              or
              <button
                class="button button--link "
                @click=${() => changeState("settings")}
              >
                Change connection settings
              </button>
            </div>`
        : html`<div class="text-center">
            <a class="button" target="_blank" @click=${() => connect()}>
              Connect to AD4M
            </a>
            <p>
              Please connect to AD4M once you have downloaded and setup your
              AD4M agent
            </p>
          </div>`}

      <p style="height: 10px; color: red; font-size: 14px; text-align: center; margin: 0; margin-top: -30px;">
        Please make sure your ad4m launcher is working and there no browser restrictions blocking you to connect to ad4m.
      </p>

      <div class="text-center">
        <a class="button button--link" _target="blank" href="https://ad4m.dev"
          >Learn more about AD4M</a
        >
      </div>
    </div>
  `;
}
