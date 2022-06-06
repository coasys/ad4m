{
  holonixPath ?  builtins.fetchTarball { url = "https://github.com/holochain/holonix/archive/54ed9f4a9ba89765942040da94f809dde3645872.tar.gz"; }
}:

let
  holonix = import (holonixPath) { };
  nixpkgs = holonix.pkgs;
in nixpkgs.mkShell {
  inputsFrom = [ holonix.main ];
  buildInputs = with nixpkgs; [
    binaryen
    nodejs-16_x
    swiProlog
  ];
}