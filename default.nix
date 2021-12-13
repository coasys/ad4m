
{
  holonixPath ?  builtins.fetchTarball { url = "https://github.com/holochain/holonix/archive/de9d6d1e820f4e3beeb20c24005c17f565a24453.tar.gz"; }
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