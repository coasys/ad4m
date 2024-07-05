import { html } from "lit";
import { Capacitor } from '@capacitor/core';
import { BarcodeScanner } from '@capacitor-community/barcode-scanner';

export default function ScanQRCode({ changeState, onSuccess, uiState }) {
  function scanQrcode(e) {
    if (!Capacitor.isNativePlatform()) {
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
    } else {
      BarcodeScanner.checkPermission({ force: true }).then((status) => {
        if (status.granted) {
          BarcodeScanner.hideBackground();

          BarcodeScanner.startScan().then((result) => {
            if (result.hasContent) {
              changeState("requestcap");
              onSuccess(result.content);
            } else {
              console.log("No content scanned");
            }

            BarcodeScanner.showBackground();
          });
        } else if (status.denied) {
          alert("Please enable camera permissions in your settings.");
        } else {
          console.error("Permission denied");
        }
      });
    }
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
