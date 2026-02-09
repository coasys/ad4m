# AD4M Connection Test Page

This is a test page for the unified AD4M connection overview that supports multiple connection methods.

## Prerequisites

1. **Ad4m-Connect built**
   ```bash
   cd connect
   pnpm build
   ```

2. **Copy built file** (if testing from this directory):
   ```bash
   cp ../../dist/web.js .
   ```

## Running the Test

1. Start a simple HTTP server in this directory:
   ```bash
   # Using Python 3
   python3 -m http.server 8080

   # Or using Node.js http-server
   npx http-server -p 8080
   ```

2. Open your browser to: `http://localhost:8080`

3. You should see the connection overview with available options

## Test Scenarios

### Scenario 1: Local AD4M Connection
1. Start local AD4M on port 12000
2. Refresh the test page
3. Should see "Local AD4M" as primary option (detected)
4. Click "Connect to Local AD4M"
5. Should go through standard capability request flow

### Scenario 2: Remote Single-User Connection
1. Stop local AD4M (or test without it running)
2. Start a single-user executor on a different machine/port
3. Click "Enter Remote URL"
4. Enter the executor URL (e.g., `ws://192.168.1.100:12000/graphql`)
5. Click "Connect"
6. Should detect as single-user and show capability request option

### Scenario 3: Remote Multi-User Connection
1. Start AD4M with multi-user mode enabled:
   ```bash
   cd rust-executor
   cargo run --release -- --multi-user --port 12000
   ```
2. Click "Enter Remote URL"
3. Enter `ws://localhost:12000/graphql`
4. Click "Connect"
5. Should detect multi-user support and show Login/Sign Up options
6. Create account or login
7. Should connect successfully

### Scenario 4: QR Code Scan (Mobile)
1. Open test page on mobile device
2. Should NOT see "Local AD4M" option
3. Should see "Scan QR Code" option
4. Can scan QR code from another device

## Expected Behavior

### Connection Overview
- Shows detected local AD4M as primary option when available
- Shows remote option always
- Shows download option when no local AD4M detected
- Mobile: No local option, includes QR scan option

### Remote Connection Flow
- URL input with placeholder
- Auto-detects multi-user capability
- Shows appropriate auth options based on detection
- Can go back to overview

### Multi-User Auth
- Shows backend URL being connected to
- Tab-based login/signup interface
- Error handling for invalid credentials

## Troubleshooting

- **"No local AD4M detected"**: Expected if executor not running on port 12000
- **Connection timeout**: Check executor is running and accessible
- **Multi-user not detected**: Verify executor started with `--multi-user` flag
- **Build errors**: Run `pnpm build` in connect directory and copy web.js
- **Import errors**: Make sure web.js is copied to test directory or adjust import path
