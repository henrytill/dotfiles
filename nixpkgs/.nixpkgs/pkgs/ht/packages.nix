{ stdenv, pkgs }:

let

  pkgsShared = with pkgs;
    [ haskellPackages.cabal-install
      haskellPackages.cabal2nix
      haskellPackages.darcs
      haskellPackages.ghc
      haskellPackages.lhs2tex
      haskellPackages.stack
      haskellPackages.stylish-haskell
      nix
      nix-prefetch-scripts
      nix-repl
      pandoc
      youtube-dl
    ];

  pkgsLinux = with pkgs;
    [ haskellPackages.Agda
      haskellPackages.idris
      haskellPackages.threadscope
      rtags
    ];

  pkgsDarwin = with pkgs;
    [ aspell
      aspellDicts.en
      cacert
      ctags
      ht.texliveEnv
      jshon
      msmtp
      mutt
      nixops
      notmuch
      offlineimap
      postgresql
      socat
      tree
      wget
    ];

  ps =
    pkgsShared
    ++ stdenv.lib.optional stdenv.isLinux  pkgsLinux
    ++ stdenv.lib.optional stdenv.isDarwin pkgsDarwin;

in ps
