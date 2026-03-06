// This file takes priority over .mocharc.json (mocha config resolution order:
// .cjs > .js > .yaml > .json).
//
// The `spec` glob is only included when NOT running in CI.  CI scripts pass
// explicit file paths to `ts-mocha` on the command line; adding `spec` here
// would override those paths and cause mocha to run all tests instead of the
// single file the script selected — which broke CI (PR #717).
//
// Locally (VS Code "Mocha for VS Code" extension), CI is unset, so `spec` is
// present and the extension can discover all test files without needing the
// pattern inside .mocharc.json.

module.exports = {
  ...(process.env.CI ? {} : {
    spec: "tests/**/*.test.ts",
    timeout: 1200000,
    exit: true,
  }),
  "node-option": [
    "experimental-specifier-resolution=node",
    "loader=ts-node/esm",
  ],
};
