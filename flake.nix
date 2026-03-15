{
  description = "Haskell terminal emulator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";

    gpipe-core-src = {
      url = "path:/mnt/share/GPipe-Core";
      flake = false;
    };
    gpipe-glfw-src = {
      url = "path:/mnt/share/GPipe-GLFW";
      flake = false;
    };
    gpipe-freetype-src = {
      url = "path:/mnt/share/gpipe-freetype";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, gpipe-core-src, gpipe-glfw-src, gpipe-freetype-src }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskellPackages.override {
          overrides = hself: hsuper: {
            GPipe = hself.callCabal2nix "GPipe"
              "${gpipe-core-src}/GPipe-Core" {};
            GPipe-GLFW = (hself.callCabal2nix "GPipe-GLFW"
              "${gpipe-glfw-src}/GPipe-GLFW" {}).overrideAttrs (old: {
                buildInputs = (old.buildInputs or []) ++ [ pkgs.glfw ];
              });
            gpipe-freetype = hself.callCabal2nix "gpipe-freetype"
              gpipe-freetype-src {};
            Terminal = hself.callCabal2nix "Terminal" (pkgs.lib.cleanSource ./.) {};
          };
        };

        terminal = haskellPackages.Terminal;

        # GHC with our library packages available, for Dyre recompilation
        ghcForDyre = haskellPackages.ghcWithPackages (p: [
          p.Terminal
        ]);

        # Wrapped binary with GHC in PATH so Dyre can recompile the user config
        wrappedTerminal = pkgs.symlinkJoin {
          name = "hsterm-gpipe-wrapped";
          paths = [ terminal ];
          buildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/hsterm-gpipe \
              --prefix PATH : ${ghcForDyre}/bin
          '';
        };
      in
      {
        packages.default = wrappedTerminal;
        packages.unwrapped = terminal;

        apps.default = {
          type = "app";
          program = "${wrappedTerminal}/bin/hsterm-gpipe";
        };

        apps.hsterm-gpipe = {
          type = "app";
          program = "${wrappedTerminal}/bin/hsterm-gpipe";
        };

        devShells.default = haskellPackages.shellFor {
          packages = p: [ p.Terminal ];
          nativeBuildInputs = with pkgs; [
            cabal-install
            haskellPackages.haskell-language-server
            pkg-config
            freetype
            glfw
          ];
        };
      }
    );
}
