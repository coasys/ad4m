# AD4M Launcher

A desktop application to handle the running and administration of an AD4M user.

## Development

Install ad4m-host binaries,

```shell
./scripts/setup-binaries.sh
```

Run frontend dev server,

```shell
yarn install
yarn start
```

In another terminal, run the tauri app,

```shell
yarn tauri dev
```

## Building

```
yarn install
yarn run build-macos/windows/linux (choose your correct OS)
```
