import { LitElement, html, css } from "lit";
import { customElement, property } from "lit/decorators.js";

@customElement("scan-qr-code")
export class ScanQRCode extends LitElement {
  @property({ type: String }) uiState = "";

  static styles = css`
    :host {
      display: block;
    }

    .qr-scanner {
      width: 100%;
      height: 100%;
    }

    .qr-content {
      position: relative;
      width: 100%;
      height: 100%;
    }

    .stop {
      position: absolute;
      top: 20px;
      left: 50%;
      transform: translateX(-50%);
      z-index: 10;
    }

    video {
      width: 100%;
      height: 100%;
      object-fit: cover;
    }
  `;

  private handleStop() {
    this.dispatchEvent(new CustomEvent("stop", { bubbles: true, composed: true }));
  }

  private handleSuccess(qrValue: string) {
    this.dispatchEvent(
      new CustomEvent("success", {
        detail: { qrValue },
        bubbles: true,
        composed: true,
      })
    );
  }

  private async scanQrcode(e: Event) {
    // @ts-ignore
    const bd = new BarcodeDetector();
    const video = e.currentTarget as HTMLVideoElement;

    const capture = async () => {
      try {
        if (this.uiState !== "qr") return;
        const barcodes = await bd.detect(video);

        const log = barcodes.find((code) => code.format === "qr_code");

        if (log?.rawValue) {
          this.handleSuccess(log.rawValue);
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

  render() {
    return html`
      <div class="qr-scanner">
        <div class="qr-content">
          <j-button
            variant="primary"
            class="stop"
            @click=${this.handleStop}
          >
            Stop scanning
          </j-button>
          <video
            width="100%"
            height="100%"
            @play=${this.scanQrcode}
            autoplay
            playsinline
            muted
          ></video>
        </div>
      </div>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "scan-qr-code": ScanQRCode;
  }
}
