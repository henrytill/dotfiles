with import <nixpkgs> {};

let
  haskellEnv = haskellngPackages.ghcWithPackages (p: with p; [
    cabal-install
    cabal2nix
    hlint
  ]);
in [
  aspell
  aspellDicts.en
  cacert
  emacs24Macport
  (gitAndTools.gitFull.override { guiSupport = false; })
  gnumake
  (gnupg.override { pinentry = pinentry.override { gtk2 = null; }; })
  haskellEnv
  mr
  nix-repl
  nixUnstable
  nixops
  offlineimap
  rsync
  stow
  tmux
  tree
  wget
  xz
]
