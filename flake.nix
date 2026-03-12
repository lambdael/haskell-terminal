{
  description = "Haskell terminal emulator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            ioctl = pkgs.haskell.lib.markUnbroken (pkgs.haskell.lib.doJailbreak hsuper.ioctl);
            Terminal = hself.callCabal2nix "Terminal" (pkgs.lib.cleanSource ./.) {};
          };
        };

        terminal = haskellPackages.Terminal;
      in
      {
        packages.default = terminal;

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.Terminal ];
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskellPackages.haskell-language-server
          ];
        };
      }
    );
}
