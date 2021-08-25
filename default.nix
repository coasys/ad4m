let
  holonixPath = builtins.fetchTarball {
    url = "https://github.com/holochain/holonix/archive/55a5eef58979fb6bc476d8c3e0c028cdeb1b5421.tar.gz";
    sha256 = "sha256:0q6d0rql1pyy93xw1c8s28jjjcgk1zgwxwixsp9z5r4w2ihaz3zg";
  };
  holonix = import (holonixPath) {
    includeHolochainBinaries = true;
    holochainVersionId = "custom";

    holochainVersion = {
      rev = "f3d17d993ad8d988402cc01d73a0095484efbabb";
      sha256 = "1z0y1bl1j2cfv4cgr4k7y0pxnkbiv5c0xv89y8dqnr32vli3bld7";
      cargoSha256 = "sha256:1rf8vg832qyymw0a4x247g0iikk6kswkllfrd5fqdr0qgf9prc31";
      bins = {
        holochain = "holochain";
        hc = "hc";
        kitsune-p2p-proxy = "kitsune_p2p/proxy";
      };

      lairKeystoreHashes = {
        sha256 = "1jiz9y1d4ybh33h1ly24s7knsqyqjagsn1gzqbj1ngl22y5v3aqh";
        cargoSha256 = "sha256:0agykcl7ysikssfwkjgb3hfw6xl0slzy38prc4rnzvagm5wd1jjv";
      };
    };
    holochainOtherDepsNames = ["lair-keystore"];
  };
  nixpkgs = holonix.pkgs;
in nixpkgs.mkShell {
  inputsFrom = [ holonix.main ];
  buildInputs = with nixpkgs; [
    binaryen
  ];
}