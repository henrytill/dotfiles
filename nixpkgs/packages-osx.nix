let
  pkgs = import <nixpkgs> {};
in
[ pkgs.aspell
  pkgs.aspellDicts.en
  pkgs.cacert
  pkgs.haskellPackages.Agda
  pkgs.haskellPackages.cabal-install
  pkgs.haskellPackages.cabal2nix
  pkgs.haskellPackages.darcs
  pkgs.haskellPackages.ghc
  pkgs.haskellPackages.idris
  pkgs.haskellPackages.purescript
  pkgs.irssi
  pkgs.jshon
  # pkgs.mercurial
  pkgs.nix
  pkgs.nix-prefetch-scripts
  pkgs.nix-repl
  pkgs.nixopsUnstable
  pkgs.offlineimap
  pkgs.pandoc
  pkgs.socat
  pkgs.tree
  pkgs.weechat
  pkgs.wget
  pkgs.youtube-dl
]
