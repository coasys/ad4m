#!/usr/bin/env bash
# ad4m-connect-auth.sh — Automate ad4m-connect auth flow via Chrome DevTools Protocol
#
# Drives a Chromium browser through the full ad4m-connect authentication:
#   1. Opens Flux URL
#   2. Waits for ad4m-connect to detect local executor
#   3. Clicks "Connect"
#   4. Captures the security code from executor stdout log
#   5. Enters the code in the browser
#   6. Clicks Authorize
#
# Prerequisites:
#   - AD4M executor running with stdout piped to a log file
#   - Flux served on a local port
#   - Chrome/Chromium installed
#   - python3 available
#   - Either 'websocat' (preferred) or python3 'websockets' package
#
# Usage:
#   scripts/ad4m-connect-auth.sh [options]
#
# Options:
#   --flux-url URL       Flux URL (default: http://localhost:3030)
#   --executor-log FILE  Executor stdout log to watch for auth codes
#                        (default: /tmp/ad4m-executor-stdout.log)
#   --cdp-port PORT      Chrome DevTools Protocol port (default: 9222)
#   --chrome PATH        Chrome/Chromium binary (auto-detected if omitted)
#   --timeout SECS       Max seconds to wait for each step (default: 30)
#   --headless           Run Chrome in headless mode
#   --no-launch          Attach to already-running Chrome instead of launching
#   --profile DIR        Chrome user data dir (default: /tmp/ad4m-chrome-profile)
#
# How the auth flow works:
#   ad4m-connect detects a local executor via /health endpoint, shows a
#   "Connect" button. Clicking it calls agentRequestCapability which makes
#   the executor generate a random 6-digit code printed to stdout. The user
#   enters this code in the browser, then clicks Authorize. The executor
#   issues a JWT token stored in localStorage.
#
# Shadow DOM note:
#   ad4m-connect renders entirely in shadow DOM. This script uses
#   element.shadowRoot.querySelector() to pierce it. Standard DOM
#   selectors won't find ad4m-connect's internal elements.
#
# Examples:
#   # Full auto — launch Chrome, complete auth
#   scripts/ad4m-connect-auth.sh --executor-log /tmp/executor.log
#
#   # Attach to existing Chrome session
#   scripts/ad4m-connect-auth.sh --no-launch --cdp-port 9222
#
#   # Headless CI mode
#   scripts/ad4m-connect-auth.sh --headless --executor-log /tmp/executor.log

set -euo pipefail

FLUX_URL="http://localhost:3030"
EXECUTOR_LOG="/tmp/ad4m-executor-stdout.log"
CDP_PORT=9222
CHROME_BIN=""
TIMEOUT=30
HEADLESS=false
LAUNCH=true
PROFILE="/tmp/ad4m-chrome-profile"

while [[ $# -gt 0 ]]; do
    case $1 in
        --flux-url) FLUX_URL="$2"; shift 2;;
        --executor-log) EXECUTOR_LOG="$2"; shift 2;;
        --cdp-port) CDP_PORT="$2"; shift 2;;
        --chrome) CHROME_BIN="$2"; shift 2;;
        --timeout) TIMEOUT="$2"; shift 2;;
        --headless) HEADLESS=true; shift;;
        --no-launch) LAUNCH=false; shift;;
        --profile) PROFILE="$2"; shift 2;;
        -h|--help)
            sed -n '/^# /,/^[^#]/p' "$0" | head -n -1 | sed 's/^# //' | sed 's/^#//'
            exit 0;;
        *) echo "Unknown option: $1"; exit 1;;
    esac
done

log() { echo -e "\033[1;36m→ $1\033[0m"; }
err() { echo -e "\033[1;31m✗ $1\033[0m" >&2; exit 1; }
ok()  { echo -e "\033[1;32m✓ $1\033[0m"; }
warn() { echo -e "\033[1;33m⚠ $1\033[0m"; }

# --- Dependency checks ---
command -v python3 &>/dev/null || err "python3 is required"
command -v curl &>/dev/null || err "curl is required"

if ! command -v websocat &>/dev/null; then
    python3 -c "import websockets" 2>/dev/null || \
        warn "Neither 'websocat' nor python3 'websockets' found. Install one: brew install websocat OR pip3 install websockets"
fi

# --- Find Chrome/Chromium ---
find_chrome() {
    if [[ -n "$CHROME_BIN" ]]; then echo "$CHROME_BIN"; return; fi
    for candidate in \
        "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome" \
        "/Applications/Chromium.app/Contents/MacOS/Chromium" \
        "$(which google-chrome-stable 2>/dev/null || true)" \
        "$(which google-chrome 2>/dev/null || true)" \
        "$(which chromium 2>/dev/null || true)" \
        "$(which chromium-browser 2>/dev/null || true)"; do
        [[ -n "$candidate" && -x "$candidate" ]] && echo "$candidate" && return
    done
    err "Chrome/Chromium not found. Install it or pass --chrome PATH"
}

# --- CDP: evaluate JavaScript in the page context ---
cdp_eval() {
    local expr="$1"
    local ws_url
    ws_url=$(curl -sf "http://127.0.0.1:$CDP_PORT/json" | python3 -c "
import sys, json
tabs = json.load(sys.stdin)
for t in tabs:
    if t.get('type') == 'page':
        print(t['webSocketDebuggerUrl']); break
" 2>/dev/null) || err "Could not get CDP WebSocket URL. Is Chrome running with --remote-debugging-port=$CDP_PORT?"

    local escaped
    escaped=$(python3 -c "import json,sys; print(json.dumps(sys.argv[1]))" "$expr")

    if command -v websocat &>/dev/null; then
        echo "{\"id\":1,\"method\":\"Runtime.evaluate\",\"params\":{\"expression\":$escaped,\"awaitPromise\":true,\"returnByValue\":true}}" | \
            websocat -t --no-close "$ws_url" 2>/dev/null | head -1 | \
            python3 -c "import sys,json; r=json.load(sys.stdin); v=r.get('result',{}).get('result',{}).get('value',''); print(v)"
    else
        python3 << PYEOF
import json, asyncio, websockets, sys
async def main():
    async with websockets.connect("$ws_url") as ws:
        await ws.send(json.dumps({"id":1,"method":"Runtime.evaluate","params":{"expression":$escaped,"awaitPromise":True,"returnByValue":True}}))
        resp = json.loads(await ws.recv())
        v = resp.get("result",{}).get("result",{}).get("value","")
        print(v)
asyncio.run(main())
PYEOF
    fi
}

# --- Wait for a condition (JS expression returning truthy) ---
cdp_wait_for() {
    local js_expr="$1"
    local description="$2"
    local elapsed=0
    log "Waiting for $description..."
    while [[ $elapsed -lt $TIMEOUT ]]; do
        local result
        result=$(cdp_eval "$js_expr" 2>/dev/null) || true
        if [[ -n "$result" && "$result" != "null" && "$result" != "false" && "$result" != "undefined" && "$result" != "None" ]]; then
            ok "$description"
            return 0
        fi
        sleep 1
        elapsed=$((elapsed + 1))
    done
    err "Timeout ($TIMEOUT s) waiting for: $description"
}

# --- Extract security code from executor log ---
extract_security_code() {
    [[ -f "$EXECUTOR_LOG" ]] || err "Executor log not found: $EXECUTOR_LOG"

    local start_line elapsed=0
    start_line=$(wc -l < "$EXECUTOR_LOG" 2>/dev/null)

    log "Watching executor log for security code..."
    while [[ $elapsed -lt $TIMEOUT ]]; do
        local code
        code=$(tail -n +"$start_line" "$EXECUTOR_LOG" 2>/dev/null | \
            grep -oE 'random secret: [0-9]{6}|secret: [0-9]{6}' | \
            tail -1 | grep -oE '[0-9]{6}') || true

        if [[ -n "$code" ]]; then
            ok "Security code: $code"
            echo "$code"
            return 0
        fi
        sleep 1
        elapsed=$((elapsed + 1))
    done
    err "Timeout waiting for security code in $EXECUTOR_LOG"
}

# ==========================================================================
# Main flow
# ==========================================================================

# Step 0: Launch Chrome if needed
if $LAUNCH; then
    CHROME_BIN=$(find_chrome)
    log "Launching Chrome (CDP port $CDP_PORT)..."

    CHROME_FLAGS=(
        --remote-debugging-port="$CDP_PORT"
        --user-data-dir="$PROFILE"
        --no-first-run
        --no-default-browser-check
    )
    $HEADLESS && CHROME_FLAGS+=(--headless=new)

    "$CHROME_BIN" "${CHROME_FLAGS[@]}" "$FLUX_URL" &>/dev/null &
    CHROME_PID=$!
    trap "kill $CHROME_PID 2>/dev/null" EXIT
    sleep 3
    ok "Chrome launched (PID $CHROME_PID)"
else
    log "Attaching to existing Chrome on CDP port $CDP_PORT..."
fi

curl -sf "http://127.0.0.1:$CDP_PORT/json" >/dev/null || err "CDP not responding on port $CDP_PORT"
ok "CDP connected"

# Step 1: Navigate and wait for ad4m-connect
cdp_eval "window.location.href = '$FLUX_URL'" >/dev/null
sleep 3

cdp_wait_for "document.querySelector('ad4m-connect') ? 'yes' : ''" "ad4m-connect component"

# Step 2: Check if already authenticated
ALREADY=$(cdp_eval "localStorage.getItem('ad4m-token') || localStorage.getItem('ad4m_token') ? 'yes' : ''")
if [[ "$ALREADY" == "yes" ]]; then
    ok "Already authenticated (token in localStorage)"
    exit 0
fi

# Step 3: Click Connect button (inside shadow DOM)
log "Clicking Connect..."
cdp_eval "
    const ac = document.querySelector('ad4m-connect');
    const shadow = ac?.shadowRoot;
    const btn = shadow?.querySelector('button');
    if (btn) { btn.click(); 'clicked' } else { '' }
"
sleep 2

# Step 4: Capture security code
CODE=$(extract_security_code)

# Step 5: Enter the code (shadow DOM input)
log "Entering security code..."
cdp_eval "
    const ac = document.querySelector('ad4m-connect');
    const shadow = ac?.shadowRoot;
    const input = shadow?.querySelector('input[type=\"text\"]')
        || shadow?.querySelector('input[type=\"number\"]')
        || shadow?.querySelector('input');
    if (input) {
        input.value = '$CODE';
        input.dispatchEvent(new Event('input', {bubbles: true}));
        input.dispatchEvent(new Event('change', {bubbles: true}));
        'entered'
    } else { '' }
"
sleep 1

# Step 6: Click Authorize
log "Clicking Authorize..."
cdp_eval "
    const ac = document.querySelector('ad4m-connect');
    const shadow = ac?.shadowRoot;
    const btns = [...(shadow?.querySelectorAll('button') || [])];
    const auth = btns.find(b => /confirm|authorize|verify|submit/i.test(b.textContent));
    if (auth) { auth.click(); 'authorized' }
    else if (btns.length > 1) { btns[btns.length - 1].click(); 'clicked last' }
    else { '' }
"

# Step 7: Wait for auth completion
sleep 3
TOKEN=$(cdp_eval "localStorage.getItem('ad4m-token') || localStorage.getItem('ad4m_token') || ''")
if [[ -n "$TOKEN" && "$TOKEN" != "null" ]]; then
    ok "Authentication complete — JWT token stored"
else
    warn "Token not found in localStorage. Auth may still be in progress — check the browser."
fi
