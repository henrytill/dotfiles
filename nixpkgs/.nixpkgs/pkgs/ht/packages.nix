{ stdenv, pkgs }:

let

  stableShared = with pkgs;
    [ aspcud
      haskellPackages.cabal-install
      haskellPackages.cabal2nix
      haskellPackages.ghc
      haskellPackages.ghcid
      haskellPackages.hasktags
      haskellPackages.stylish-haskell
      nix
      nix-prefetch-scripts
      nixops
    ];

  unstableShared = with pkgs;
    [ haskellPackages.darcs
      youtube-dl
    ];

  stableLinux = with pkgs; [];

  unstableLinux = with pkgs;
    [ haskellPackages.threadscope
    ];

  stableDarwin = with pkgs;
    [ aspell
      aspellDicts.en
      cacert
      ctags
      doxygen
      jshon
      mercurial
      msmtp
      mutt
      nodejs
      offlineimap
      postgresql
      python3
      qemu
      socat
      tmux
      tree
      wget
    ];

  unstableDarwin = with pkgs;
    [ notmuch
    ];

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
