# AD4M Connection Library and Authentication Wizard

A powerful library that simplifies connecting applications to AD4M executors by handling:
- Executor discovery and connection
- Capability-based authentication
- Token management and storage
- Connection state management

<div style="text-align: center">
<img src="/docs/public/images/connect1.jpg" width="400" alt="AD4M Connect Initial Screen"></img>
</div>

## Overview

AD4M uses a capability-based security model to protect user data and control access to agent functionality. Every application that wants to interact with an AD4M executor needs to request specific capabilities, which are then granted (or denied) by the user.

### What are Capabilities?

Capabilities in AD4M are like permission tokens that:
- Define what data an application can access (which perspectives)
- Specify what operations an application can perform
- Are scoped to specific domains and functions
- Can be revoked by the user at any time

A capability token might grant permissions like:
- Read access to specific perspectives
- Write access to create or modify links
- Ability to create new perspectives
- Permission to manage agent relationships
- Access to specific language functions

## Installation

```bash
npm install -s @coasys/ad4m-connect
```

## Authentication Flow

When an application wants to connect to an AD4M executor, it goes through a secure authentication handshake:

1. The application requests access with specific capabilities
2. The AD4M executor generates a random verification code
3. The user must confirm the request and enter the code
4. Upon successful verification, a capability token is issued

### Visual Flow

1. Initial Connection Screen:
   ![Initial Connection](/docs/public/images/connect1.jpg)

2. Executor Found:
   ![Executor Found](/docs/public/images/connect2.jpg)

3. Authorization Request:
   ![Authorization Request](/docs/public/images/authorize1.jpg)

4. Verification Code:
   ![Verification Code](/docs/public/images/authorize2.jpg)

## Usage

### In the Browser

```js
import Ad4mConnectUI from "@coasys/ad4m-connect";

const ui = Ad4mConnect({
  // Required parameters
  appName: "My AD4M App",
  appDesc: "A description of what your app does",
  appDomain: "myapp.com",
  capabilities: [{ 
    with: { domain: "*", pointers: ["*"] }, 
    can: ["*"] 
  }],
  
  // Optional parameters
  appIconPath: "https://myapp.com/icon.png",
  port: 12345,  // Custom port
  token: "existing-token",  // Existing JWT token
  url: "custom-executor-url"  // Custom executor URL
});

// Listen for authentication state changes
ui.addEventListener("authstatechange", (e) => {
  switch(e.detail) {
    case "authenticated":
      console.log("Successfully authenticated");
      break;
    case "unauthenticated":
      console.log("Not authenticated");
      break;
    case "locked":
      console.log("Agent is locked");
      break;
  }
});

// Connect and get AD4M client
ui.connect().then((client) => {
  // Client is now ready to use
  console.log("Connected with capabilities");
});
```

### In Node.js / Electron

```js
const { ad4mConnect } = require("@coasys/ad4m-connect/electron");

ad4mConnect({
  // Provide the name of your app to be displayed in the dialog
  appName: "Perspect3ve",
  // Provide an icon to be displayed in the dialog as well
  appIconPath: path.join(__dirname, "graphics", "Logo.png"),
  // Name the capabilities your app needs
  // (this is an example with all capabilities)
  capabilities: [{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }],
  // Provide a directory in which the capability token and the executor
  // URL will be stored such that future calls won't even open a dialog
  // but try the token against that URL and resolve immediately
  // if it works.
  dataPath: path.join(homedir(), ".perspect3ve"),
})
  .then(({ client, capabilityToken, executorUrl }) => {
    // Retrieved `capabilityToken` and selected `executorUrl` are returned
    // but all that is really needed is `client` which is a fully setup
    // (including capability token) and working Ad4mClient.
    //
    // Both, the URL and the token have already been stored on disk
    // in the directory provided as `dataPath`.
    //
    // Consequetive calls
    createWindow(client);
  })
  .catch(() => {
    console.log("User closed AD4M connection wizard. Exiting...");
    app.exit(0);
    process.exit(0);
  });
```

### Capability Specification

When requesting capabilities, specify:

```js
{
  with: {
    domain: string | "*",     // Which perspective/domain
    pointers: string[] | "*"  // Which parts of the domain
  },
  can: string[] | "*"         // Which operations are allowed
}
```

Examples:
```js
// Full access (development only)
{ with: { domain: "*", pointers: ["*"] }, can: ["*"] }

// Read-only access to a perspective
{ with: { domain: "perspective-uuid", pointers: ["*"] }, can: ["read"] }

// Specific operations on a domain
{ with: { domain: "friends", pointers: ["*"] }, can: ["read", "add", "remove"] }
```

## Events

The library emits various events to help track connection state:

- `authstatechange`: 
  - `authenticated`: Successfully connected with capabilities
  - `unauthenticated`: No valid authentication
  - `locked`: Agent is locked

- `connectionstatechange`:
  - `connecting`: Attempting to connect
  - `connected`: Successfully connected
  - `not_connected`: No connection
  - `disconnected`: Lost connection
  - `error`: Connection error

- `configstatechange`: Configuration changes for `token`, `url`, or `port`

## Mobile Integration

### Android Setup

Add to `android/app/src/main/AndroidManifest.xml`:
```diff
<?xml version="1.0" encoding="utf-8"?>
<manifest
  xmlns:android="http://schemas.android.com/apk/res/android"
+  xmlns:tools="http://schemas.android.com/tools"
  package="com.example">

  <application
+    android:hardwareAccelerated="true"
  >
  </application>

+  <uses-permission android:name="android.permission.CAMERA" />

+  <uses-sdk tools:overrideLibrary="com.google.zxing.client.android" />
</manifest>
```

### iOS Setup

Add to `Info.plist`:
```diff
<dict>
+  <key>NSCameraUsageDescription</key>
+  <string>To be able to scan barcodes</string>
</dict>
```

After changes, run:
```bash
npx cap sync
npx cap build
```

## Best Practices

1. **Request Minimal Capabilities**
   - Only request permissions your app actually needs
   - Use specific domains and operations instead of wildcards
   - Consider read-only access when possible

2. **Handle Authentication States**
   - Check if capabilities are still valid
   - Implement reconnection logic
   - Handle capability revocation gracefully

3. **Secure Storage**
   - Store capability tokens securely
   - Never expose admin credentials
   - Implement token refresh mechanisms

4. **User Experience**
   - Clearly explain why your app needs certain capabilities
   - Provide feedback during the authentication process
   - Handle errors gracefully

## More Information

For more details about AD4M authentication and capabilities:
- [AD4M Documentation](https://docs.ad4m.dev/auth)
