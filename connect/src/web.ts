import { html, css, LitElement } from "lit";
import { customElement, property, state } from "lit/decorators.js";

import Ad4mConnect, {
  AuthStates,
  ConfigStates,
  ConnectionStates,
  Ad4mConnectOptions,
} from "./core";

import { Html5Qrcode } from "html5-qrcode";
import Loading from "./components/Loading";
import RemoteUrl from "./components/RemoteUrl";
import Start from "./components/Start";
import Disconnected from "./components/Disconnected";
import AgentLocked from "./components/AgentLocked";
import RequestCapability from "./components/RequestCapability";
import InvalidToken from "./components/InvalidToken";
import VerifyCode from "./components/VerifyCode";
import CouldNotMakeRequest from "./components/CouldNotMakeRequest";
import Header from "./components/Header";
import autoBind from "auto-bind";
export { getAd4mClient } from "./utils";

function detectMob() {
  const toMatch = [
    /Android/i,
    /webOS/i,
    /iPhone/i,
    /iPad/i,
    /iPod/i,
    /BlackBerry/i,
    /Windows Phone/i,
  ];

  return toMatch.some((toMatchItem) => {
    return navigator.userAgent.match(toMatchItem);
  });
}

const styles = css`
  :host {
    --primary-color: #fff;
    --heading-color: #fff;
    --body-color: #a7a7a7;
    --success-color: #52d652;
    --background-color: #000;
    --start-color: #a4adff;
    --end-color: #d273ff;
    --gradient: linear-gradient(90deg, var(--start-color), var(--end-color));
  }

  .wrapper {
    font-family: "DM Sans", Helvetica, Arial, sans-serif;
    position: fixed;
    top: 0;
    left: 0;
    color: var(--body-color);
    height: 100vh;
    width: 100vw;
  }

  * {
    box-sizing: border-box;
  }

  .items {
    display: flex;
    flex-direction: column;
    flex-gap: 50px;
    gap: 50px;
  }

  .items--small {
    flex-gap: 20px;
    gap: 20px;
  }

  .button {
    text-decoration: none;
    cursor: pointer;
    border: 0;
    color: var(--background-color);
    background: var(--gradient);
    height: 50px;
    font-weight: 600;
    min-width: 100px;
    padding: 0px 30px;
    border-radius: 8px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    text-align: center;
    font-family: inherit;
    font-size: 15px;
  }

  @media (min-width: 800px) {
    .button {
      min-width: 200px;
      padding: 0px 40px;
      font-size: 17px;
    }
  }

  .heading {
    color: var(--heading-color);
    font-size: 18px;
    font-weight: 600;
    margin: 0;
    margin-bottom: 15px;
  }

  .body {
    padding: 0;
    margin: 0;
    font-size: 14px;
    line-height: 1.5;
  }

  .buttons {
    display: flex;
    align-items: center;
    justify-content: center;
    flex-gap: 10px;
    gap: 10px;
  }

  .button--full {
    width: 100%;
    display: flex;
  }

  .button--link {
    padding: 0;
    height: auto;
    background: none;
    color: var(--primary-color);
    font-size: inherit;
    min-width: auto;
    text-decoration: none;
  }

  .button--link:hover {
    text-decoration: underline;
  }

  .button--secondary {
    background: var(--background-color);
    border: 1px solid var(--primary-color);
    color: var(--primary-color);
  }

  .dialog {
    background-color: var(--background-color);
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translateX(-50%) translateY(-50%);
    z-index: 10;
    border-radius: 8px;
    width: calc(100vw - 10px);
    max-width: 500px;
  }

  @media (min-width: 800px) {
    .dialog {
      width: 100%;
      max-width: 500px;
    }
  }

  .dialog__header {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 120px;
    padding: 0 30px;
  }

  .dialog__connect {
    display: flex;
    justify-content: center;
    align-items: center;
    flex-gap: 50px;
    gap: 50px;
    position: relative;
  }

  .dialog__logo {
    text-align: center;
    width: 100%;
    max-width: 50px;
    margin: 0 auto;
  }

  .dialog__logo svg {
    width: 100%;
  }

  .dialog__connect-ad4m {
    width: 100px;
    height: 100px;
    background: var(--background-color);
    padding: 20px;
    box-shadow: 0px 4px 7px 0px rgb(0 0 0 / 8%);
    border-radius: 50%;
    position: relative;
  }

  .dialog__connect-ad4m:before {
    content: "";
    position: absolute;
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    z-index: -1;
    margin: -2px;
    border-radius: inherit;
    background: var(--gradient);
  }

  .dialog__connect-app {
    width: 80px;
    height: 80px;
  }

  .dialog__connect-check:before {
    content: "";
    display: block;
    width: 120px;
    border-bottom: 1px dashed var(--body-color);
    position: absolute;
    left: 50%;
    top: 50%;
    transform: translateY(-50%) translateX(-50%);
  }

  .dialog__connect-check svg {
    position: relative;
  }

  .text-center {
    text-align: center;
  }

  .dialog__content {
    padding-top: 0;
    padding-left: 30px;
    padding-right: 30px;
    padding-bottom: 30px;
  }

  .input {
    display: flex;
    flex-direction: column;
    flex-gap: 10px;
    gap: 10px;
  }

  .input__label {
    font-size: 12px;
  }

  .input__field {
    border-radius: 8px;
    outline: 0;
    height: 60px;
    color: var(--heading-color);
    background-color: var(--background-color);
    padding: 0px 30px;
    font-size: 20px;
    border: 1px solid var(--body-color);
  }

  .input__field:focus {
    border: 1px solid var(--primary-color);
    box-shadow: 0px 0px 0px 1px var(--primary-color);
  }

  .ad4mConnect__backdrop {
    position: absolute;
    top: 0;
    left: 0;
    height: 100vh;
    width: 100vw;
    background-color: rgba(0, 0, 0, 0.5);
  }

  .ad4mConnect__locked {
    position: fixed;
    top: 0;
    left: 0;
    background: linear-gradient(
      90deg,
      rgba(2, 0, 36, 1) 0%,
      rgba(38, 3, 23, 1) 41%,
      rgba(51, 4, 31, 1) 100%
    );
    height: 100vh;
    width: 100vw;
    padding: 36px;
    display: flex;
    align-items: center;
    flex-direction: column;
    font-family: "Comfortaa", cursive;
  }

  .lds-ring {
    display: block;
    position: relative;
    width: 80px;
    height: 80px;
    margin: 0 auto;
    margin-top: 24px;
  }
  .lds-ring div {
    box-sizing: border-box;
    display: block;
    position: absolute;
    width: 64px;
    height: 64px;
    margin: 4px;
    border: 4px solid var(--primary-color);
    border-radius: 50%;
    animation: lds-ring 1.2s cubic-bezier(0.5, 0, 0.5, 1) infinite;
    border-color: var(--primary-color) transparent transparent transparent;
  }
  .lds-ring div:nth-child(1) {
    animation-delay: -0.45s;
  }
  .lds-ring div:nth-child(2) {
    animation-delay: -0.3s;
  }
  .lds-ring div:nth-child(3) {
    animation-delay: -0.15s;
  }
  @keyframes lds-ring {
    0% {
      transform: rotate(0deg);
    }
    100% {
      transform: rotate(360deg);
    }
  }

  .disconnected {
    position: fixed;
    top: 0;
    left: 0;
    width: 100vw;
    padding: 10px 0;
    text-align: center;
    background: red;
  }
`;

@customElement("ad4m-connect")
export class Ad4mConnectElement extends LitElement {
  static styles = [styles];

  @state()
  private _code = null;

  @state()
  private _isMobile = null;

  @state()
  private _hasClickedDownload = null;

  @state()
  private _client: Ad4mConnect;

  @state()
  private _isOpen: boolean = false;

  @state()
  private uiState:
    | "loading"
    | "remoteurl"
    | "start"
    | "requestcap"
    | "verifycode"
    | "invalidtoken"
    | "disconnected"
    | "agentlocked"
    | "closed"
    | "connectionerror" = "start";

  @property({ type: String, reflect: true })
  appName = null;

  @property({ type: String, reflect: true })
  appDesc = null;

  @property({ type: String, reflect: true })
  appDomain = null;

  @property({ type: String, reflect: true })
  appIconPath = null;

  @property({ type: String, reflect: true })
  capabilities = [];

  // TODO: localstorage doesnt work here
  @property({ type: String })
  token = localStorage.getItem("ad4murl") || "";

  // TODO: localstorage doesnt work here
  @property({ type: String, reflect: true })
  port = parseInt(localStorage.getItem("ad4mport")) || 12000;

  // TODO: localstorage doesnt work here
  @property({ type: String, reflect: true })
  url = localStorage.getItem("ad4murl") || "";

  get authState(): AuthStates {
    return this._client.authState;
  }

  get connectionState(): ConnectionStates {
    return this._client.connectionState;
  }

  connectedCallback() {
    super.connectedCallback();
    autoBind(this);

    this._isMobile = detectMob();

    this._client = new Ad4mConnect({
      appName: this.appName,
      appDesc: this.appDesc,
      appDomain: this.appDomain,
      appIconPath: this.appIconPath,
      capabilities: Array.isArray(this.capabilities)
        ? this.capabilities
        : JSON.parse(this.capabilities),
      port: this.port || parseInt(localStorage.getItem("ad4mport")) || 12000,
      token: this.token || localStorage.getItem("ad4mtoken"),
      url: this.url || localStorage.getItem("ad4murl"),
    });

    this._client.on("configstatechange", (name: any, val) => {
      this[name] = val;
      if (val) {
        localStorage.setItem("ad4m" + name, val);
      } else {
        localStorage.removeItem("ad4m" + name);
      }
      this.requestUpdate();
    });

    this._client.on("authstatechange", (event: AuthStates) => {
      const customEvent = new CustomEvent("authstatechange", {
        detail: event,
      });
      if (event === "locked") {
        this._isOpen = true;
      }
      this.dispatchEvent(customEvent);
      this.requestUpdate();
    });

    this._client.on("connectionstatechange", (event: ConnectionStates) => {
      if (event === "connected") {
        this.uiState = "requestcap";
      }
      if (event === "disconnected") {
        this._isOpen = true;
      }
      const customEvent = new CustomEvent("connectionstatechange", {
        detail: event,
      });
      this.dispatchEvent(customEvent);
      this.requestUpdate();
    });

    loadFont();
    constructQR();
  }

  async connect() {
    const client = await this._client.connect();
    if (this.authState !== "authenticated") {
      this._isOpen = true;
    }
    return client;
  }

  getAd4mClient() {
    return this._client.ensureConnection();
  }

  scanQrcode() {
    const html5QrCode = new Html5Qrcode("reader");
    const ele = document.getElementById("camera-id");
    ele.style.display = "block";

    const qrCodeSuccessCallback = (decodedText, decodedResult) => {
      this._client.connect(decodedText);
      html5QrCode.stop();
      ele.style.display = "none";
    };
    function onScanFailure(error) {
      console.warn(`Code scan error = ${error}`);
    }

    const width = window.innerWidth;
    const height = window.innerHeight;
    const reverseAspectRatio = height / width;

    const mobileAspectRatio =
      reverseAspectRatio > 1.5
        ? reverseAspectRatio + (reverseAspectRatio * 12) / 100
        : reverseAspectRatio;

    const config = {
      fps: 20, // frame per seconds for qr code scanning
      qrbox: { width: 250, height: 250 },
      videoConstraints: {
        facingMode: "environment",
        aspectRatio: mobileAspectRatio,
      },
    };

    const cancelBtn = document.getElementById("stop-scan");
    cancelBtn.addEventListener("click", function () {
      html5QrCode.stop();
      ele.style.display = "none";
    });

    html5QrCode.start(
      { facingMode: "environment" },
      config,
      qrCodeSuccessCallback,
      onScanFailure
    );
  }

  private async unlockAgent(passcode) {
    await this._client.ad4mClient.agent.unlock(passcode);
  }

  private verifyCode(code) {
    this._client.verifyCode(code);
  }

  changeUrl(url) {
    this.setAttribute("url", url);
  }

  connectRemote(url) {
    this._client.connect(url);
  }

  async requestCapability(bool) {
    try {
      await this._client.requestCapability(bool);
      this.uiState = "verifycode";
    } catch (e) {
      console.warn(e);
    }
  }

  async isAuthenticated() {
    await this._client.ensureConnection();
    return this._client.checkAuth();
  }

  changeUIState(state) {
    this.uiState = state;
  }

  changeCode(code) {
    this._code = code;
  }

  onDownloaded() {
    this._hasClickedDownload = true;
  }

  renderViews() {
    if (this.authState === "locked") {
      return AgentLocked({
        unlockAgent: this.unlockAgent,
        reconnect: this.connect,
      });
    }

    if (this.connectionState === "connecting") {
      return Loading();
    }

    if (this.uiState === "remoteurl") {
      return RemoteUrl({
        url: this.url,
        changeState: this.changeUIState,
        changeUrl: this.changeUrl,
        connectRemote: this.connectRemote,
      });
    }

    if (this.connectionState === "not_connected") {
      return Start({
        scanQrcode: this.scanQrcode,
        connect: this.connect,
        isMobile: this._isMobile,
        hasClickedDownload: this._hasClickedDownload,
        onDownloaded: this.onDownloaded,
        changeState: this.changeUIState,
      });
    }

    if (this.connectionState === "connected") {
      if (this.uiState === "verifycode") {
        return VerifyCode({
          code: this._code,
          changeCode: this.changeCode,
          changeState: this.changeUIState,
          verifyCode: this.verifyCode,
        });
      }

      return RequestCapability({
        changeState: this.changeUIState,
        requestCapability: this.requestCapability,
        capabilities: this.capabilities,
        appname: this.appName,
        appiconpath: this.appIconPath,
      });
    }

    if (this.connectionState === "disconnected") {
      return Disconnected({
        reconnect: this.connect,
      });
    }

    if (this.connectionState === "port_not_found") {
      return CouldNotMakeRequest();
    }
  }

  render() {
    if (this._isOpen === false) return null;
    if (this.authState === "authenticated") return null;
    return html`
      <div class="wrapper">
        <div class="dialog">
          ${Header()}
          <main class="dialog__content">${this.renderViews()}</main>
        </div>
        <div class="ad4mConnect__backdrop" />
      </div>
    `;
  }
}

export default function Ad4mConnectUI(props: Ad4mConnectOptions) {
  const element = new Ad4mConnectElement();

  Object.entries(props).forEach(([key, value]) => {
    element[key] = value;
  });

  document.body.appendChild(element);

  return element;
}

function loadFont() {
  const link = document.createElement("link");
  link.rel = "stylesheet";
  link.type = "text/css";
  link.crossOrigin = "anonymous";
  link.href =
    "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&display=swap";
  document.head.appendChild(link);
}

function constructQR() {
  const containerEle = document.createElement("div");
  containerEle.id = "camera-id";
  containerEle.style.position = "absolute";
  containerEle.style.top = "0";
  containerEle.style.left = "0";
  containerEle.style.width = "100vw";
  containerEle.style.height = "100vh";
  containerEle.style.zIndex = "10000";
  containerEle.style.display = "none";

  const ele = document.createElement("div");
  ele.id = "reader";
  // @ts-ignore
  ele.width = "100vw";
  ele.style.height = "100vh";

  const cancelBtn = document.createElement("button");
  cancelBtn.id = "stop-scan";
  cancelBtn.innerHTML = "&#10005;";
  cancelBtn.style.paddingTop = "4px";
  cancelBtn.style.display = "flex";
  cancelBtn.style.alignItems = "center";
  cancelBtn.style.justifyContent = "center";
  cancelBtn.style.position = "absolute";
  cancelBtn.style.top = "10px";
  cancelBtn.style.right = "10px";
  cancelBtn.style.borderRadius = "50%";
  cancelBtn.style.border = "0";
  cancelBtn.style.height = "30px";
  cancelBtn.style.width = "30px";
  cancelBtn.style.fontFamily = "inherit";
  cancelBtn.style.fontSize = "20px";

  containerEle.appendChild(ele);
  containerEle.appendChild(cancelBtn);
  document.body.appendChild(containerEle);
}
