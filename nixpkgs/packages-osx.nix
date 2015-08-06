with import <nixpkgs> {}; [
  aspell
  aspellDicts.en
  cacert
  emacs24Macport
  ghostscript
  (gitAndTools.gitFull.override { guiSupport = false; })
  gnumake
  go14Packages.vault
  mr
  nix-repl
  nixUnstable
  offlineimap
  postgresql94
  rsync
  stow
  tmux
  tree
  wget
  xz
]
