let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          haskell-terminal =
            haskellPackages.callPackage ./default.nix {
	            #mesa = pkgs.mesa;
            };
           };
        };
      };
    };

  pkgs = import <nixpkgs> { inherit config; };
in
  rec {
    haskell-terminal = pkgs.haskellPackages.haskell-terminal;
  }
