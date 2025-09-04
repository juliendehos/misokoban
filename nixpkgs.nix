 
let

  miso-src = fetchTarball {
    url = https://github.com/dmjio/miso/archive/3c3c359af12d657cf78ace32db53b088b5b8b7a2.tar.gz;
    sha256 = "sha256:0gyxc0ckvb4j6bm8k5kpfh34p76320cgl897ii78zz9y7yps9ziz";
    # should match cabal.project
  };
  
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {
          miso = self.callCabal2nixWithOptions "miso" miso-src "-ftemplate-haskell" {};
        };
      };
    };
  };

  channel = <nixpkgs>;
  # channel = fetchTarball "https://github.com/NixOS/nixpkgs/archive/25.05.tar.gz";

  pkgs = import channel { inherit config; };

in pkgs

