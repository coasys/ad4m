let 
  holonixPath = builtins.fetchTarball {
    url = "https://github.com/holochain/holonix/archive/3e94163765975f35f7d8ec509b33c3da52661bd1.tar.gz";
    sha256 = "07sl281r29ygh54dxys1qpjvlvmnh7iv1ppf79fbki96dj9ip7d2";
  };
  holonix = import (holonixPath) {
    includeHolochainBinaries = true;
    holochainVersionId = "custom";
    
    holochainVersion = { 
     rev = "1ff2cc2935e6a2bfbb95aef5f2860eb09b467b49";
     sha256 = "16hsikyasi0zbh7gfrpzlahydx7csnvshz421sx56f0jpwvi2g80";
     cargoSha256 = "0w29y8w5k5clq74k84ksj5aqxbxhqxh2djhll6vv694djw277rpj";
     bins = {
       holochain = "holochain";
       hc = "hc";
       lair-keystore = "lair-keystore";
     };
    };
  };
in holonix.main