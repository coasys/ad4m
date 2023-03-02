import { html } from "lit";

export default function ScanQRCode({ changeState, onSuccess, uiState }) {
  function scanQrcode(e) {
    // @ts-ignore
    const bd = new BarcodeDetector();
    const video = e.currentTarget;

    const capture = async () => {
      try {
        if (uiState !== "qr") return;
        const barcodes = await bd.detect(video);

        const log = barcodes.find((code) => code.format === "qr_code");

        if (log?.rawValue) {
          changeState("requestcap");
          onSuccess(log.rawValue);
          return;
        } else {
          requestAnimationFrame(capture);
        }
      } catch (err) {
        console.error(err);
      }
    };

    capture();
  }

  return html`
    <div class="qr-scanner">
      <div class="qr-content">
        <button
          class="button button--primary stop"
          @click=${() => changeState("start")}
        >
          Stop scanning
        </button>
        <video
          width="100%"
          height="100%"
          @play=${scanQrcode}
          autoplay
          playsinline
          muted
        ></video>
      </div>
    </div>
  `;
}
