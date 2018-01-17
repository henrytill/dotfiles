{ stdenv, pkgs }:

let

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
    [ haskellPackages.threadscope
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
