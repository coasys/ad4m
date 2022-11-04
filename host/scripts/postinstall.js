import { execSync } from 'child_process';

var isWin = process.platform === "win32";

execSync("rm -rf node_modules/ipfs-core-types/src");

execSync("cd ../ && yarn run patchpackage");

if (!isWin) {
  execSync("ln -sf node_modules/@perspect3vism/ad4m-executor/default.nix default.nix")
}