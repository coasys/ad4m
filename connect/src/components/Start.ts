import { html } from "lit";

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
              href="https://github.com/perspect3vism/ad4min/releases/latest"
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
