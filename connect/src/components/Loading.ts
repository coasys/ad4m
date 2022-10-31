import { html } from "lit";

export default function Loading() {
  return html`
    <div>
      <div class="lds-ring">
        <div></div>
        <div></div>
        <div></div>
        <div></div>
      </div>
    </div>
  `;
}
