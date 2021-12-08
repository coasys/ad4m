let
  holonixPath = builtins.fetchTarball "https://github.com/holochain/holonix/archive/f3ecb117bdd876b8dcb33ad04984c5da5b2d358c.tar.gz";
  holonix = import (holonixPath) {
    holochainVersionId = "v0_0_115";
  };
  nixpkgs = holonix.pkgs;
in nixpkgs.mkShell {
  inputsFrom = [ holonix.shell ];
  packages = [
  ];
  buildInputs = with nixpkgs; [
    binaryen
    nodejs-16_x
    swiProlog
  ];
}