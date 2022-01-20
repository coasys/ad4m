let
  holonixPath = builtins.fetchTarball "https://github.com/holochain/holonix/archive/d15633710a8d4349dc0ff03b7b47ad01eb9f2433.zip";
  holonix = import (holonixPath) {
    holochainVersionId = "v0_0_120";
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