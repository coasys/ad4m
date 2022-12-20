# This file was generated with the following command:
# update-holochain-versions --git-src=revision:holochain-0.0.161 --lair-version-req=~0.2 --output-file=holochain_version.nix
# For usage instructions please visit https://github.com/holochain/holochain-nixpkgs/#readme

{
    url = "https://github.com/holochain/holochain";
    rev = "holochain-0.1.0-beta-rc.1";
    sha256 = "sha256-xp1DTVrhGEc1CZr6LvBFZZhoOUbUPpg3/mWOj4DDXjI=";
    cargoLock = {
        outputHashes = {
        };
    };

    binsFilter = [
        "holochain"
        "hc"
        "kitsune-p2p-proxy"
        "kitsune-p2p-tx2-proxy"
    ];
}
