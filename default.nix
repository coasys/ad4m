{
  holonixPath ?  builtins.fetchTarball { url = "https://github.com/holochain/holonix/archive/52158409f9b76b442e592e8f06632b0e57a6c365.tar.gz"; }
}:

let
  holonix = import (holonixPath) { };
  nixpkgs = holonix.pkgs;
in nixpkgs.mkShell {
  inputsFrom = [ holonix.main ];
  buildInputs = with nixpkgs; [
    binaryen
    nodejs-16_x
  ];
}