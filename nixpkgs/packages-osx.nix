with import <nixpkgs> {}; [
  aspell
  aspellDicts.en
  cacert
  cloc
  ed
  emacs24Macport
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
  wget
  xz
  youtube-dl
]
