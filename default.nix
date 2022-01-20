let
  holonixPath = builtins.fetchTarball "https://github.com/holochain/holonix/archive/9013d1310e2d37d718fe58d43b4a34aed12c8a59.tar.gz";
  holonix = import (holonixPath) {
    holochainVersionId = "v0_0_122";
  };
  nixpkgs = holonix.pkgs;
in nixpkgs.mkShell {
  inputsFrom = [ holonix.main ];
  packages = [
  ];
  buildInputs = with nixpkgs; [
    binaryen
    nodejs-16_x
    swiProlog
  ];
}