// Content script — bridge between page and devtools panel
// Forwards messages from the page's window.__AD4M_DEVTOOLS__ to the extension

// Listen for messages from the page
window.addEventListener('message', (event) => {
  if (event.source !== window) return;
  if (event.data?.type === 'AD4M_DEVTOOLS_EVENT') {
    chrome.runtime.sendMessage(event.data);
  }
});

// Inject a script to detect AD4M on the page
const script = document.createElement('script');
script.textContent = `
  (function() {
    const check = setInterval(() => {
      if (window.__AD4M_DEVTOOLS__) {
        window.postMessage({ type: 'AD4M_DEVTOOLS_EVENT', action: 'detected' }, '*');
        clearInterval(check);
      }
    }, 500);
    setTimeout(() => clearInterval(check), 30000);
  })();
`;
document.documentElement.appendChild(script);
script.remove();
