#!/bin/zsh
# AD4M × Claude Code — one-command setup
# Sets up the AD4M MCP server, auto-unlock launchd job, and ClaudeMemory Perspective
# for use with Claude Code (https://claude.ai/code)
#
# Usage: ./setup-claude-code.sh --passphrase YOUR_AD4M_PASSPHRASE [--gql-port 4000]

set -e

GQL_PORT=4000
PASSPHRASE=""

while [[ $# -gt 0 ]]; do
  case $1 in
    --passphrase) PASSPHRASE="$2"; shift 2 ;;
    --gql-port)   GQL_PORT="$2";   shift 2 ;;
    *) echo "Unknown arg: $1"; exit 1 ;;
  esac
done

if [[ -z "$PASSPHRASE" ]]; then
  echo "Usage: $0 --passphrase YOUR_AD4M_PASSPHRASE [--gql-port 4000]"
  exit 1
fi

GQL_URL="http://localhost:${GQL_PORT}/graphql"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "→ Checking Node.js..."
node --version >/dev/null 2>&1 || { echo "Node.js not found. Install from https://nodejs.org"; exit 1; }

echo "→ Installing MCP server dependencies..."
cd "$SCRIPT_DIR/mcp-server" && npm install && npm run build
cd "$SCRIPT_DIR"

echo "→ Storing passphrase in macOS Keychain..."
security add-generic-password -s "ad4m-passphrase" -a "ad4m" -w "$PASSPHRASE" 2>/dev/null || \
  security add-generic-password -U -s "ad4m-passphrase" -a "ad4m" -w "$PASSPHRASE"

echo "→ Installing auto-unlock launchd job..."
UNLOCK_SCRIPT="$HOME/Library/Application Support/ad4m/auto-unlock.sh"
mkdir -p "$(dirname "$UNLOCK_SCRIPT")"

cat > "$UNLOCK_SCRIPT" << SCRIPT
#!/bin/zsh
LOG="\$HOME/Library/Logs/ad4m-unlock.log"
GQL="http://localhost:${GQL_PORT}/graphql"
log() { echo "\$(date '+%Y-%m-%d %H:%M:%S') \$*" >> "\$LOG"; }

STATUS=\$(curl -s --max-time 5 -X POST "\$GQL" -H "Content-Type: application/json" -d '{"query":"{ agentStatus { isUnlocked } }"}' 2>/dev/null)
echo "\$STATUS" | grep -q '"isUnlocked":true' && { log "Already unlocked"; exit 0; }

TRIES=0
until curl -s --max-time 3 -X POST "\$GQL" -H "Content-Type: application/json" -d '{"query":"{ agentStatus { isInitialized } }"}' 2>/dev/null | grep -q "isInitialized"; do
  TRIES=\$((TRIES+1)); [ \$TRIES -ge 20 ] && { log "ERROR: executor unreachable"; exit 1; }; sleep 3
done

PASS=\$(security find-generic-password -s "ad4m-passphrase" -a "ad4m" -w 2>/dev/null)
[ -z "\$PASS" ] && { log "ERROR: passphrase not in Keychain"; exit 1; }

RESULT=\$(curl -s --max-time 10 -X POST "\$GQL" -H "Content-Type: application/json" \
  -d "{\"query\":\"mutation { agentUnlock(passphrase: \\\"\$PASS\\\", holochain: true) { isUnlocked } }\"}" 2>/dev/null)

echo "\$RESULT" | grep -q '"isUnlocked":true' && { log "Unlocked OK"; exit 0; } || { log "ERROR: \$RESULT"; exit 1; }
SCRIPT

chmod +x "$UNLOCK_SCRIPT"

PLIST="$HOME/Library/LaunchAgents/dev.ad4m.auto-unlock.plist"
cat > "$PLIST" << PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key><string>dev.ad4m.auto-unlock</string>
    <key>ProgramArguments</key>
    <array><string>/bin/zsh</string><string>${UNLOCK_SCRIPT}</string></array>
    <key>RunAtLoad</key><true/>
    <key>StartInterval</key><integer>30</integer>
    <key>StandardOutPath</key><string>${HOME}/Library/Logs/ad4m-unlock.log</string>
    <key>StandardErrorPath</key><string>${HOME}/Library/Logs/ad4m-unlock.log</string>
</dict>
</plist>
PLIST

launchctl unload "$PLIST" 2>/dev/null || true
launchctl load "$PLIST"
echo "   Auto-unlock job loaded (runs every 30s after boot)"

echo "→ Unlocking agent now..."
sleep 2
RESULT=$(curl -s --max-time 10 -X POST "$GQL_URL" \
  -H "Content-Type: application/json" \
  -d "{\"query\":\"mutation { agentUnlock(passphrase: \\\"$PASSPHRASE\\\", holochain: true) { isUnlocked } }\"}" 2>/dev/null)
echo "$RESULT" | grep -q '"isUnlocked":true' && echo "   Agent unlocked OK" || echo "   Note: unlock returned: $RESULT"

echo "→ Creating ClaudeMemory Perspective..."
RESULT=$(curl -s --max-time 10 -X POST "$GQL_URL" \
  -H "Content-Type: application/json" \
  -d '{"query":"mutation { perspectiveAdd(name: \"ClaudeMemory\") { uuid name } }"}' 2>/dev/null)
UUID=$(echo "$RESULT" | grep -o '"uuid":"[^"]*"' | head -1 | cut -d'"' -f4)

if [[ -n "$UUID" ]]; then
  echo -n "$UUID" > ~/.ad4m/claude-memory-uuid
  echo "   ClaudeMemory UUID: $UUID (saved to ~/.ad4m/claude-memory-uuid)"
else
  echo "   Note: could not create Perspective — executor may not be running yet"
fi

echo "→ Adding MCP server to Claude Code settings..."
SETTINGS="$HOME/.claude/settings.json"
MCP_BIN="$SCRIPT_DIR/mcp-server/dist/index.js"

if [[ -f "$SETTINGS" ]]; then
  echo "   settings.json found — add this manually to the 'mcpServers' section:"
  echo ""
  echo '    "ad4m": {'
  echo '      "command": "node",'
  echo "      \"args\": [\"$MCP_BIN\"],"
  echo '      "env": { "AD4M_GQL_URL": "'"http://localhost:${GQL_PORT}/graphql"'" }'
  echo '    }'
else
  echo "   Claude Code settings not found at $SETTINGS — run Claude Code first, then re-run this script"
fi

echo ""
echo "✓ AD4M × Claude Code setup complete"
echo "  MCP server: $SCRIPT_DIR/mcp-server/dist/index.js"
echo "  ClaudeMemory UUID: ${UUID:-see ~/.ad4m/claude-memory-uuid}"
echo "  Auto-unlock: dev.ad4m.auto-unlock (launchd)"
echo ""
echo "  Restart Claude Code to activate the MCP server."
