import { html } from "lit";
import AppLogo from "./AppLogo";

export default function MobileAppLogoButton({ openModal }) {
    return html`
        <div class="mainlogo" @click=${openModal}>
            ${AppLogo()}
        </div>
  `;
}
