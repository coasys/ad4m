// Content script — bridge between page and devtools panel
// No inline script injection needed — the panel polls via
// chrome.devtools.inspectedWindow.eval() directly.

// Forward any messages from the page to the extension background
window.addEventListener('message', (event) => {
  if (event.source !== window) return;
  if (event.data?.type === 'AD4M_DEVTOOLS_EVENT') {
    try {
      chrome.runtime.sendMessage(event.data);
    } catch {}
  }
});
