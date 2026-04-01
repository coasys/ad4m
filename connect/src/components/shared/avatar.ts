import { html, TemplateResult } from "lit";
import { getInitials, getHue } from "../../utils";
import type { RemoteHost } from "../../types";

/**
 * Render a host avatar with image-error fallback to coloured initials.
 * @param host   The remote host to render
 * @param cls    CSS class for the element (e.g. "host-avatar" or "profile-pic")
 */
export function renderHostAvatar(host: RemoteHost, cls: string): TemplateResult {
  const hue = getHue(host.id || host.name);
  const initials = getInitials(host.name);

  if (host.profilePicUrl) {
    const onError = (e: Event) => {
      const img = e.target as HTMLImageElement;
      const fallback = document.createElement('div');
      fallback.className = `${cls} fallback`;
      fallback.style.background = `hsl(${hue},60%,35%)`;
      fallback.textContent = initials;
      img.replaceWith(fallback);
    };
    return html`<img class=${cls} src=${host.profilePicUrl} alt="" @error=${onError} />`;
  }

  return html`<div class="${cls} fallback" style="background:hsl(${hue},60%,35%)">${initials}</div>`;
}
