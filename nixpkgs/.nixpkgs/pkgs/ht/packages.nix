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

  stableShared = with pkgs;
    [ aspcud
      haskellPackages.cabal-install
      haskellPackages.cabal2nix
      haskellPackages.ghc
      haskellPackages.stack
      haskellPackages.stylish-haskell
      nix
      nix-prefetch-scripts
      nix-repl
      pandoc
    ];

  unstableShared = with pkgs;
    [ haskellPackages.darcs
      youtube-dl
    ];

  stableLinux = with pkgs;
    [ haskellPackages.Agda
    ];

  unstableLinux = with pkgs;
    [ multiGHCTravis
      haskellPackages.threadscope
    ];

  stableDarwin = with pkgs;
    [ aspell
      aspellDicts.en
      cacert
      ctags
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

  unstableDarwin = with pkgs; [];

in {

  stable =
    stableShared
    ++ stdenv.lib.optional stdenv.isLinux  stableLinux
    ++ stdenv.lib.optional stdenv.isDarwin stableDarwin;

  unstable =
    unstableShared
    ++ stdenv.lib.optional stdenv.isLinux  unstableLinux
    ++ stdenv.lib.optional stdenv.isDarwin unstableDarwin;

}
