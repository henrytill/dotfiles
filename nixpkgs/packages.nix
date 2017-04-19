let
  nixpkgs = import <nixpkgs> {};
  stdenv  = nixpkgs.stdenv;

  pkgsShared = with nixpkgs;
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
      pijul
    ];

  pkgsLinux = with nixpkgs;
    [ racket ];

  pkgsDarwin = with nixpkgs;
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

  pkgs =
    pkgsShared
    ++ stdenv.lib.optional stdenv.isLinux  pkgsLinux
    ++ stdenv.lib.optional stdenv.isDarwin pkgsDarwin;

in pkgs
