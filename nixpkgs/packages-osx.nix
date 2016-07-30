with import <nixpkgs> {}; [
  aspell
  aspellDicts.en
  cacert
  ctags
  (gitAndTools.gitFull.override { guiSupport = false; svnSupport = false; })
  gnumake
  haskellPackages.cabal-install
  haskellPackages.cabal2nix
  haskellPackages.ghc
  jshon
  mr
  nix
  nix-repl
  nodejs
  offlineimap
  rlwrap
  rsync
  stow
  tmux
  tree
  vim
  wget
  xz
  youtube-dl
]
