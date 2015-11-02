with import <nixpkgs> {}; [
  aspell
  aspellDicts.en
  cacert
  cloc
  ed
  emacs
  ghostscript
  (gitAndTools.gitFull.override { guiSupport = false; svnSupport = false; })
  gnumake
  graphviz
  jq
  mr
  nix-repl
  nix
  offlineimap
  postgresql94
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
