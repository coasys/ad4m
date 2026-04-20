Set-Location languages/test-language
pnpm install
pnpm run build

Set-Location ../note-store
pnpm install
pnpm run build

Set-Location ../note-store-flat
pnpm install
pnpm run build

Set-Location ../aes-flat
pnpm install
pnpm run build
