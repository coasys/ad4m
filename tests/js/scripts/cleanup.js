import killProcess from "kill-process-by-name";
import { execSync } from "child_process";

async function cleanup() {
  // First try the graceful kill-by-name (works cross-platform)
  try {
    killProcess("ad4m");
  } catch (e) {
    // Ignore — process may already be gone
  }

  // Then force-kill any lingering ad4m executor processes on Linux/macOS.
  // This prevents "address already in use" errors when a previous test run
  // left an executor alive (e.g. after Ctrl-C or a test timeout).
  try {
    execSync('pkill -9 -f "ad4m" 2>/dev/null || true', { stdio: "ignore" });
  } catch (_) {
    // Ignore on Windows or if no processes match
  }

  // Brief pause so the OS reclaims the ports before the next executor starts
  await new Promise((r) => setTimeout(r, 500));
}

cleanup();
