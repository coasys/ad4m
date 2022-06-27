# Update windows holochain version in github CI as its not using nix

let
  holonixPath = (import ./nix/sources.nix).holonix;
  holonix = import (holonixPath) {
    holochainVersionId = "custom";
    holochainVersion = import ./holochain_version.nix;
  };
  nixpkgs = holonix.pkgs;
in
nixpkgs.mkShell {
  inputsFrom = [ holonix.main ];
  packages = [
  ];
  buildInputs = with nixpkgs; [
    binaryen
    nodejs-16_x
    swiProlog
  ];
}