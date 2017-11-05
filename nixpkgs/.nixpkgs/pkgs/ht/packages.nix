{ stdenv, pkgs }:

let

  pkgsShared = with pkgs;
    [ haskellPackages.Agda
      haskellPackages.cabal-install
      haskellPackages.cabal2nix
      haskellPackages.darcs
      haskellPackages.ghc
      haskellPackages.hasktags
      haskellPackages.idris
      haskellPackages.lhs2tex
      haskellPackages.stack
      haskellPackages.threadscope
      nix
      nix-prefetch-scripts
      nix-repl
      pandoc
      youtube-dl
    ];

  pkgsLinux = with pkgs;
    [ rtags
    ];

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
      # notmuch
      offlineimap
      socat
      tree
      wget
    ];

  ps =
    pkgsShared
    ++ stdenv.lib.optional stdenv.isLinux  pkgsLinux
    ++ stdenv.lib.optional stdenv.isDarwin pkgsDarwin;

in ps
