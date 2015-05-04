with import <nixpkgs> {};

let
  haskellEnv  = haskellngPackages.ghcWithPackages (p: with p; [
    cabal-install
    cabal2nix
    hlint
    xmobar
    xmonad
    xmonad-contrib
  ]);
in [
  compton-git
  haskellEnv
  hsetroot
  xlibs.xmessage
]
