#!/usr/bin/env bash
# ad4m-connect-auth.sh — Automate ad4m-connect auth flow via Chrome DevTools Protocol
#
# Drives a Chromium browser through the full ad4m-connect authentication:
#   1. Opens Flux URL
#   2. Clicks "Connect to Local Node"
#   3. Clicks "Authorize" — triggers requestCapability
#   4. Gets the security code (via admin API or executor stdout)
#   5. Enters the code in the browser
#   6. Waits for auth completion (JWT in localStorage)
#
# Two modes for getting the security code:
#   --admin-credential: Uses the REST API to permit the capability request
#     and retrieve the code programmatically. This is the preferred method
#     for headless/CI operation where stdout watching is unreliable.
#   --executor-log: Watches executor stdout for "random secret: NNNNNN".
#     Only works when auto_permit_cap_requests is enabled.
#
# Prerequisites:
#   - AD4M executor running (with --admin-credential for API mode)
#   - Flux served on a local port
#   - Chrome/Chromium installed
#   - python3 available
#   - Either 'websocat' (preferred) or python3 'websockets' package
#   - curl (for admin API mode)
#   - jq (for admin API mode)
#
# Usage:
#   scripts/ad4m-connect-auth.sh [options]
#
# Options:
#   --flux-url URL           Flux URL (default: http://localhost:3030)
#   --executor-url URL       Executor REST API URL (default: http://127.0.0.1:12000)
#   --admin-credential CRED  Use admin REST API to get auth code (preferred)
#   --executor-log FILE      Watch executor stdout for code (fallback)
#                             (default: /tmp/ad4m-executor-stdout.log)
#   --cdp-port PORT          Chrome DevTools Protocol port (default: 9222)
#   --chrome PATH            Chrome/Chromium binary (auto-detected if omitted)
#   --timeout SECS           Max seconds to wait for each step (default: 30)
#   --headless               Run Chrome in headless mode
#   --no-launch              Attach to already-running Chrome instead of launching
#   --profile DIR            Chrome user data dir (default: /tmp/ad4m-chrome-profile)
#
# Examples:
#   # Full auto with admin API (preferred)
#   scripts/ad4m-connect-auth.sh --admin-credential test123
#
#   # Legacy mode — watch executor stdout
#   scripts/ad4m-connect-auth.sh --executor-log /tmp/executor.log
#
#   # Headless CI mode
#   scripts/ad4m-connect-auth.sh --headless --admin-credential test123

set -euo pipefail

FLUX_URL="http://localhost:3030"
EXECUTOR_URL="http://127.0.0.1:12000"
ADMIN_CREDENTIAL=""
EXECUTOR_LOG="/tmp/ad4m-executor-stdout.log"
CDP_PORT=9222
CHROME_BIN=""
TIMEOUT=30
HEADLESS=false
LAUNCH=true
PROFILE="/tmp/ad4m-chrome-profile"

log() { echo -e "\033[1;36m→ $1\033[0m" >&2; }
err() { echo -e "\033[1;31m✗ $1\033[0m" >&2; exit 1; }
ok()  { echo -e "\033[1;32m✓ $1\033[0m" >&2; }
warn() { echo -e "\033[1;33m⚠ $1\033[0m" >&2; }

while [[ $# -gt 0 ]]; do
    case $1 in
        --flux-url) [[ $# -ge 2 ]] || err "--flux-url requires a URL"; FLUX_URL="$2"; shift 2;;
        --executor-url) [[ $# -ge 2 ]] || err "--executor-url requires a URL"; EXECUTOR_URL="$2"; shift 2;;
        --admin-credential) [[ $# -ge 2 ]] || err "--admin-credential requires a value"; ADMIN_CREDENTIAL="$2"; shift 2;;
        --executor-log) [[ $# -ge 2 ]] || err "--executor-log requires a file path"; EXECUTOR_LOG="$2"; shift 2;;
        --cdp-port) [[ $# -ge 2 ]] || err "--cdp-port requires a port number"; CDP_PORT="$2"; shift 2;;
        --chrome) [[ $# -ge 2 ]] || err "--chrome requires a path"; CHROME_BIN="$2"; shift 2;;
        --timeout) [[ $# -ge 2 ]] || err "--timeout requires seconds"; TIMEOUT="$2"; shift 2;;
        --headless) HEADLESS=true; shift;;
        --no-launch) LAUNCH=false; shift;;
        --profile) [[ $# -ge 2 ]] || err "--profile requires a directory"; PROFILE="$2"; shift 2;;
        -h|--help)
            sed -n '/^# /,/^[^#]/p' "$0" | head -n -1 | sed 's/^# //' | sed 's/^#//'
            exit 0;;
        *) echo "Unknown option: $1"; exit 1;;
    esac
done

# --- Dependency checks ---
command -v python3 &>/dev/null || err "python3 is required"
command -v curl &>/dev/null || err "curl is required"
python3 -c "import websockets" 2>/dev/null || \
    err "python3 'websockets' package required. Install: pip3 install websockets"

if [[ -n "$ADMIN_CREDENTIAL" ]]; then
    command -v jq &>/dev/null || err "jq is required for --admin-credential mode"
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
# Note: ad4m-connect uses nested shadow DOM (e.g. ad4m-connect > shadowRoot >
# connection-options > shadowRoot > button). All JS expressions that query
# internal elements must use deepQueryAll() to pierce nested shadow roots.
DEEP_QUERY_JS='
function deepQueryAll(root, selector) {
    const results = [...root.querySelectorAll(selector)];
    root.querySelectorAll("*").forEach(el => {
        if (el.shadowRoot) results.push(...deepQueryAll(el.shadowRoot, selector));
    });
    return results;
}
function deepQuery(root, selector) { return deepQueryAll(root, selector)[0] || null; }
'
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

    # Always use python3 websockets for reliable multiline JS evaluation.
    # websocat breaks on multiline JSON payloads.
    python3 -c "
import json, asyncio, websockets, sys
async def main():
    expr = sys.argv[1]
    msg = json.dumps({'id':1,'method':'Runtime.evaluate','params':{'expression':expr,'awaitPromise':True,'returnByValue':True}})
    async with websockets.connect(sys.argv[2]) as ws:
        await ws.send(msg)
        resp = json.loads(await ws.recv())
        v = resp.get('result',{}).get('result',{}).get('value','')
        if v is not None:
            print(v, end='')
asyncio.run(main())
" "$expr" "$ws_url" 2>/dev/null
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

# --- Extract security code from executor log (fallback mode) ---
extract_security_code() {
    [[ -f "$EXECUTOR_LOG" ]] || return 1

    local elapsed=0
    log "Checking executor log for security code..."
    while [[ $elapsed -lt 5 ]]; do
        local code
        # Search the last 50 lines for the code pattern
        code=$(tail -50 "$EXECUTOR_LOG" 2>/dev/null | \
            grep -oE 'Random number challenge: [0-9]{6}|random secret: [0-9]{6}' | \
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

# --- Get security code via admin REST API ---
# This bypasses the need for stdout watching. Uses the admin credential to:
# 1. Read the requestId from the browser's ad4m-connect component
# 2. Read the auth info that was sent
# 3. Call the permit endpoint to get the 6-digit code
get_code_via_admin() {
    local elapsed=0

    # Wait for ad4m-connect to have made its requestCapability call
    log "Waiting for ad4m-connect to request capability..."
    local request_id=""
    while [[ $elapsed -lt $TIMEOUT ]]; do
        request_id=$(cdp_eval "
            const ac = document.querySelector('ad4m-connect');
            const core = ac?._core || ac?.core;
            core?.requestId || ''
        " 2>/dev/null) || true
        if [[ -n "$request_id" && "$request_id" != "null" && "$request_id" != "undefined" && "$request_id" != "" ]]; then
            break
        fi
        sleep 1
        elapsed=$((elapsed + 1))
    done
    [[ -n "$request_id" && "$request_id" != "null" && "$request_id" != "undefined" ]] || \
        err "Could not read requestId from ad4m-connect (timeout ${TIMEOUT}s)"
    ok "Got requestId: $request_id"

    # Read the auth info (appName, appDesc, etc.) from ad4m-connect options
    local auth_json
    auth_json=$(cdp_eval "
        const ac = document.querySelector('ad4m-connect');
        const core = ac?._core || ac?.core;
        const opts = core?.options;
        if (opts) {
            JSON.stringify({
                appName: opts.appInfo?.name || 'unknown',
                appDesc: opts.appInfo?.description || '',
                appDomain: opts.appInfo?.url || '',
                appUrl: opts.appInfo?.url || '',
                capabilities: opts.capabilities || []
            });
        } else { '' }
    " 2>/dev/null) || true

    if [[ -z "$auth_json" || "$auth_json" == "null" || "$auth_json" == "undefined" ]]; then
        warn "Could not read auth info from ad4m-connect, using defaults"
        auth_json='{"appName":"ad4m-connect","appDesc":"ad4m-connect auth","appDomain":"localhost","appUrl":"http://localhost:3030","capabilities":[{"with":{"domain":"*","pointers":["*"]},"can":["*"]}]}'
    fi
    ok "Auth info: $auth_json"

    # Build AuthInfoExtended
    local auth_extended
    auth_extended=$(jq -n --arg rid "$request_id" --argjson auth "$auth_json" \
        '{requestId: $rid, auth: $auth}')

    # Call permit endpoint — supports both GraphQL (current dev) and REST (future)
    log "Calling permit endpoint..."
    local permit_body
    permit_body=$(jq -n --arg auth "$auth_extended" '{auth: $auth}')

    local response
    # Try REST first, fall back to GraphQL
    response=$(curl -sf -X POST "${EXECUTOR_URL}/api/v1/agent/auth/permit" \
        -H "Content-Type: application/json" \
        -H "Authorization: ${ADMIN_CREDENTIAL}" \
        -d "$permit_body" 2>/dev/null) || {
        # REST not available — use GraphQL mutation
        local escaped_auth
        escaped_auth=$(echo "$auth_extended" | jq -Rs '.')
        local gql_body
        gql_body=$(jq -n --arg query "mutation { agentPermitCapability(auth: ${escaped_auth}) }" '{query: $query}')
        response=$(curl -sf -X POST "${EXECUTOR_URL}/graphql" \
            -H "Content-Type: application/json" \
            -H "Authorization: ${ADMIN_CREDENTIAL}" \
            -d "$gql_body" 2>&1) || err "Both REST and GraphQL permit calls failed: $response"
        # Extract from GraphQL response: {"data":{"agentPermitCapability":"123456"}}
        response=$(echo "$response" | jq -r '.data.agentPermitCapability // empty' 2>/dev/null) || true
    }

    # Response is a JSON string with the 6-digit code
    local code
    code=$(echo "$response" | jq -r '.' 2>/dev/null) || code="$response"
    # Strip quotes if present
    code="${code//\"/}"

    if [[ ${#code} -eq 6 && "$code" =~ ^[0-9]+$ ]]; then
        ok "Security code from admin API: $code"
        echo "$code"
        return 0
    else
        err "Unexpected permit response: $response"
    fi
}

# ==========================================================================
# Main flow
# ==========================================================================

# Step 0: Launch Chrome if needed
if $LAUNCH; then
    CHROME_BIN=$(find_chrome)
    log "Launching Chrome (CDP port $CDP_PORT)..."

    # Kill any existing Chrome on this CDP port
    lsof -ti:$CDP_PORT 2>/dev/null | xargs kill -9 2>/dev/null || true
    sleep 1

    CHROME_FLAGS=(
        --remote-debugging-port="$CDP_PORT"
        --user-data-dir="$PROFILE"
        --no-first-run
        --no-default-browser-check
        # Disable Chrome Private Network Access preflight — required for
        # localhost cross-port requests (Flux on :3030 → executor on :12000)
        --disable-features=PrivateNetworkAccessRespectPreflightResults
    )
    $HEADLESS && CHROME_FLAGS+=(--headless=new)

    "$CHROME_BIN" "${CHROME_FLAGS[@]}" "$FLUX_URL" &>/dev/null &
    CHROME_PID=$!
    # Don't kill Chrome on script exit — leave it running for debugging
    # and for subsequent auth script runs to reuse the session
    echo "$CHROME_PID" > /tmp/ad4m-chrome.pid
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
ALREADY=$(cdp_eval "Object.keys(localStorage).some(k => k.endsWith('/ad4m-token')) ? 'yes' : ''")
if [[ "$ALREADY" == "yes" ]]; then
    ok "Already authenticated (token in localStorage)"
    exit 0
fi

# Step 3: Click "Connect to Local Node" button (inside shadow DOM)
log "Looking for Connect button..."
cdp_wait_for "
    (() => {
        ${DEEP_QUERY_JS}
        const ac = document.querySelector('ad4m-connect');
        if (!ac?.shadowRoot) return '';
        const btns = deepQueryAll(ac.shadowRoot, 'button');
        return btns.some(b => /connect.*local|local.*node/i.test(b.textContent)) ? 'yes' : '';
    })()
" "Connect button to appear"

cdp_eval "
    (() => {
        ${DEEP_QUERY_JS}
        const ac = document.querySelector('ad4m-connect');
        const btns = deepQueryAll(ac.shadowRoot, 'button');
        const connectBtn = btns.find(b => /connect.*local|local.*node/i.test(b.textContent))
            || btns.find(b => /connect/i.test(b.textContent))
            || btns[0];
        if (connectBtn) { connectBtn.click(); return 'clicked'; } else { return ''; }
    })()
"
sleep 2

# Step 4: Click "Authorize" button if present (triggers requestCapability)
log "Looking for Authorize button..."
sleep 2
cdp_eval "
    (() => {
        ${DEEP_QUERY_JS}
        const ac = document.querySelector('ad4m-connect');
        const btns = deepQueryAll(ac.shadowRoot, 'button');
        const authBtn = btns.find(b => /authorize/i.test(b.textContent));
        if (authBtn) { authBtn.click(); return 'clicked authorize'; } else { return 'no authorize button'; }
    })()
"
sleep 2

# Step 5: Get the security code
# Try stdout first (works when executor has auto-permit, which the CLI always enables)
# Fall back to admin API if stdout doesn't have it
CODE=""
if [[ -f "$EXECUTOR_LOG" ]]; then
    CODE=$(extract_security_code 2>/dev/null) || true
fi
if [[ -z "$CODE" && -n "$ADMIN_CREDENTIAL" ]]; then
    CODE=$(get_code_via_admin)
elif [[ -z "$CODE" ]]; then
    err "No security code found. Provide --executor-log or --admin-credential"
fi

# Step 6: Enter the code and verify
log "Entering security code: $CODE"
cdp_eval "
    (() => {
        ${DEEP_QUERY_JS}
        const ac = document.querySelector('ad4m-connect');
        // Find the local-authentication component
        const localAuth = deepQuery(ac.shadowRoot, 'local-authentication');
        if (localAuth) {
            // Set securityCode directly on the component and trigger verify
            localAuth.securityCode = '$CODE';
            localAuth.requestUpdate();
            // Also set the input value for visual feedback
            const input = localAuth.shadowRoot?.querySelector('input');
            if (input) input.value = '$CODE';
            // Trigger verification
            localAuth.verifyCode();
            return 'verified via component';
        }
        // Fallback: find input in nested shadow DOMs
        const inputs = deepQueryAll(ac.shadowRoot, 'input');
        const input = inputs.find(i => i.type === 'text' || i.type === 'number' || i.type === 'tel')
            || inputs[0];
        if (input) {
            const nativeInputValueSetter = Object.getOwnPropertyDescriptor(
                window.HTMLInputElement.prototype, 'value').set;
            nativeInputValueSetter.call(input, '$CODE');
            input.dispatchEvent(new Event('input', {bubbles: true, composed: true}));
            input.dispatchEvent(new Event('change', {bubbles: true, composed: true}));
            return 'entered via input';
        }
        return 'no input found';
    })()
"

# Step 8: Wait for auth completion
log "Waiting for authentication to complete..."
sleep 3
local_elapsed=0
TOKEN=""
while [[ $local_elapsed -lt $TIMEOUT ]]; do
    TOKEN=$(cdp_eval "
        const keys = Object.keys(localStorage);
        const tokenKey = keys.find(k => k.endsWith('/ad4m-token'));
        tokenKey ? localStorage.getItem(tokenKey) : ''
    " 2>/dev/null) || true
    if [[ -n "$TOKEN" && "$TOKEN" != "null" && "$TOKEN" != "undefined" && "$TOKEN" != "" ]]; then
        ok "Authentication complete — JWT token stored in localStorage"
        # Reload page so Flux picks up the JWT and loads past ad4m-connect
        cdp_eval "window.location.reload()" >/dev/null 2>&1
        ok "Page reloaded — Flux should now load"
        exit 0
    fi
    sleep 1
    local_elapsed=$((local_elapsed + 1))
done

warn "Token not found in localStorage after ${TIMEOUT}s. Auth may still be in progress — check the browser."
exit 1
