# AD4M Multi-User Test Page

This is a simple test page to verify the multi-user signup and login flow with ad4m-connect.

## Prerequisites

1. **AD4M Executor** running with multi-user mode enabled on `ws://localhost:4000/graphql`

   To enable multi-user mode, run the executor with:
   ```bash
   # From the ad4m root directory
   cd rust-executor
   cargo run -- --multi-user
   ```

2. **Ad4m-Connect built**
   ```bash
   cd connect
   pnpm build
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

3. The signup/login dialog should appear automatically

## Test Flow

1. **Signup**: Create a new account with email and password
2. **Login**: Log in with the credentials you just created
3. **Verify**: Check that connection and authentication states change to "connected" and "authenticated"

## Expected Behavior

- On page load, you should see a signup/login dialog
- After successful signup or login, the dialog should close
- The status box should show:
  - Connection: `connected`
  - Auth: `authenticated`
- No capability request or verification code steps (those are for traditional flow)

## Troubleshooting

- **"Multi-user mode is not enabled"**: Make sure the executor is running with `--multi-user` flag
- **Connection timeout**: Verify the executor is running on port 4000
- **Login fails**: Try signing up first, then logging in
- **Build errors**: Make sure you've run `pnpm build` in the connect directory
