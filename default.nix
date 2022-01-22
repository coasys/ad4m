{
  holonixPath ?  builtins.fetchTarball { url = "https://github.com/holochain/holonix/archive/2f7b8047d6314f64fca34394a52d465c18b2f4d5.tar.gz"; }
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