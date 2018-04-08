{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

with nixpkgs;
let
  systemPackages = with pkgs; [
      weechat
      emacs
      bash
      make
      haskellPackages.apply-refact
      haskellPackages.hlint
      haskellPackages.stylish-haskell
      haskellPackages.hasktags
      haskellPackages.hoogle
      readline
       ];
  # Install Fonts.
  fonts = {
    #create a directory with links to all fonts in /run/current-system/sw/share/X11-fonts.
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      mplus-outline-fonts
      anonymousPro
      source-code-pro
      hasklig
      symbola
      uni-vga
    ];
  };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages.ghcWithPackages (self :[
                        self.ghc
                        self.ghc-mod
                        self.apply-refact
                        self.hlint
                        self.stylish-haskell
                        self.hasktags
                        self.hoogle
                        self.terminfo
                          # add more packages here
                          ])
                       else pkgs.haskell.packages.${compiler};

  drv = (import ./release.nix).haskell-terminal;
in
  drv.env
