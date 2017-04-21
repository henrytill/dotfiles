{ stdenv, pkgs }:

let
  pkgsShared = with pkgs;
    [ haskellPackages.Agda
      haskellPackages.cabal-install
      haskellPackages.cabal2nix
      haskellPackages.darcs
      haskellPackages.ghc
      haskellPackages.idris
      haskellPackages.purescript
      nix
      nix-prefetch-scripts
      nix-repl
      pandoc
      pijul
    ];

  pkgsLinux = with pkgs;
    [ racket ];

  pkgsDarwin = with pkgs;
    [ aspell
      aspellDicts.en
      cacert
      ctags
      ht.texliveEnv
      jshon
      # mercurial
      msmtp
      nixopsUnstable
      notmuch
      offlineimap
      pandoc
      socat
      tree
      wget
      youtube-dl
    ];

  ps =
    pkgsShared
    ++ stdenv.lib.optional stdenv.isLinux  pkgsLinux
    ++ stdenv.lib.optional stdenv.isDarwin pkgsDarwin;

in ps
