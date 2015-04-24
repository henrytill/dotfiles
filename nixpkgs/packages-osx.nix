with import <nixpkgs> {}; [
  aspell
  aspellDicts.en
  cacert
  emacs24Macport
  (gitAndTools.gitFull.override { guiSupport = false; })
  gnumake
  mr
  nix-repl
  nixUnstable
  offlineimap
  rsync
  stow
  tmux
  tree
  wget
  xz
]
