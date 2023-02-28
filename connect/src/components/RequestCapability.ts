import { html } from "lit";
import Logo from "./Logo";
import { canCapToString, domainCapToString, pointerCapToString } from "@perspect3vism/ad4m";

export default function CapNotMatchedFirst({
  capabilities,
  appname,
  appiconpath,
  changeState,
  requestCapability,
}) {
  return html`
    <div class="items">
      <div class="text-center">
        <h1 class="heading">Authorize ${appname}</h1>
        <p class="body">${appname} want you to grant it these capabilities: </p>
        <br>
        <div>
          ${capabilities.map(
            (cap) =>
              `${canCapToString(cap.can)} ${domainCapToString(cap.with.domain)}, with specific access to: ${pointerCapToString(cap.with.pointers)}`
          )}
        </div>
      </div>
      ${appiconpath &&
      html`<div class="dialog__connect">
        <img class="dialog__connect-app" src=${appiconpath} alt="Logo" />
        <div class="dialog__connect-check">
          <svg
            xmlns="http://www.w3.org/2000/svg"
            width="30"
            height="30"
            fill="var(--success-color)"
            viewBox="0 0 16 16"
          >
            <path
              d="M16 8A8 8 0 1 1 0 8a8 8 0 0 1 16 0zm-3.97-3.03a.75.75 0 0 0-1.08.022L7.477 9.417 5.384 7.323a.75.75 0 0 0-1.06 1.06L6.97 11.03a.75.75 0 0 0 1.079-.02l3.992-4.99a.75.75 0 0 0-.01-1.05z"
            />
          </svg>
        </div>
        <div class="dialog__connect-ad4m">${Logo()}</div>
      </div>`}
      <button
        class="button button--full"
        @click=${() => requestCapability(true)}
      >
        Authorize
      </button>
      <button
        class="button button--full button--link"
        @click=${() => changeState("remoteurl")}
      >
        Connection settings
      </button>
    </div>
  `;
}
