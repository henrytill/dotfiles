let
  nixpkgs = import <nixpkgs> {};
  pkgs = with nixpkgs;
    [ aspell
      aspellDicts.en
      cacert
      haskellPackages.Agda
      haskellPackages.cabal-install
      haskellPackages.cabal2nix
      haskellPackages.darcs
      haskellPackages.ghc
      haskellPackages.idris
      haskellPackages.purescript
      jshon
      # mercurial
      msmtp
      nix
      nix-prefetch-scripts
      nix-repl
      nixopsUnstable
      notmuch
      offlineimap
      pandoc
      socat
      tree
      wget
      youtube-dl
    ];
in pkgs
