let 
  holonixPath = builtins.fetchTarball {
    url = "https://github.com/holochain/holonix/archive/3e94163765975f35f7d8ec509b33c3da52661bd1.tar.gz";
    sha256 = "07sl281r29ygh54dxys1qpjvlvmnh7iv1ppf79fbki96dj9ip7d2";
  };
  holonix = import (holonixPath) {
    includeHolochainBinaries = true;
    holochainVersionId = "custom";
    
    holochainVersion = { 
     rev = "ea726cc05aa6064c3b8b4f85fddf3e89429f018e";
     sha256 = "061i7b59pgwf5n4ma6cypg08v50n60z1b2yan3cgwdzmm95na2cm";
     cargoSha256 = "0fs0va4w3vabbdba6pdwfkfyh271ngri0kpv7amgkljfmszxdw1c";
     bins = {
       holochain = "holochain";
       hc = "hc";
       lair-keystore = "lair-keystore";
     };
    };
  };
in holonix.main