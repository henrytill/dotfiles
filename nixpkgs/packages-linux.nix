with import <nixpkgs> {};

let
  compton-git = callPackage ./pkgs/compton/git.nix {};
  haskellEnv  = haskellngPackages.ghcWithPackages (p: with p; [
    cabal-install
    cabal2nix
    hlint
    xmobar
    xmonad
    xmonad-contrib
  ]);
in [
  aspell
  aspellDicts.en
  compton-git
  (dmenu.override { enableXft = true; })
  dunst
  emacs
  file
  firefoxWrapper
  gimp
  gmrun
  gnupg
  haskellEnv
  hsetroot
  htop
  i3lock
  iftop
  inkscape
  iotop
  leiningen
  libnotify
  lsof
  mosh
  mupdf
  nix-repl
  obnam
  racket
  rxvt_unicode_with-plugins
  scrot
  sshfsFuse
  tigervnc
  tmux
  tree
  unclutter
  unzip
  vlc
  wget
  xclip
  xlibs.xmessage
  xz
  youtube-dl
]
