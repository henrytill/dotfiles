let
  nixpkgs = import <nixpkgs> {};
  pkgs = with nixpkgs;
    [ haskellPackages.Agda
      haskellPackages.cabal-install
      haskellPackages.cabal2nix
      haskellPackages.ghc
      haskellPackages.idris
      haskellPackages.purescript
      nix
      nix-prefetch-scripts
      nix-repl
      pandoc
      racket
    ];
in pkgs
