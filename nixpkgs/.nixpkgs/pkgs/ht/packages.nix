{ stdenv, pkgs }:

let

  multiGHCTravis =
    pkgs.haskellPackages.callCabal2nix "multi-ghc-travis" (pkgs.fetchFromGitHub {
      owner           = "hvr";
      repo            = "multi-ghc-travis";
      rev             = "a76b3e96a796936b750efbd555cce5714e752f97";
      sha256          = "122bdaszr9nl1nilslc1kxb954v34b72xasqvsplkgby1hzlzgfi";
      fetchSubmodules = true;
    }) {};

  pkgsShared = with pkgs;
    [ aspcud
      haskellPackages.cabal-install
      haskellPackages.cabal2nix
      haskellPackages.darcs
      haskellPackages.ghc
      haskellPackages.stack
      haskellPackages.stylish-haskell
      multiGHCTravis
      nix
      nix-prefetch-scripts
      nix-repl
      pandoc
      youtube-dl
    ];

  pkgsLinux = with pkgs;
    [ haskellPackages.Agda
      haskellPackages.threadscope
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
