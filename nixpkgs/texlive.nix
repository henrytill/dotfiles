let
  pkgs = import <nixpkgs> {};
  texliveEnv = pkgs.texlive.combine {
    inherit (pkgs.texlive)
    scheme-medium
    collection-fontsextra
    collection-fontsrecommended
    enumitem
    fontaxes
    mweights
    titlesec;
  };
in
texliveEnv
