{
  allowUnfree = true;

  packageOverrides = pkgs: {
    ht = {
      base = with pkgs;
        [ cabal2nix
          pandoc
          racket
        ];
      texliveEnv = pkgs.texlive.combine {
        inherit (pkgs.texlive)
        scheme-medium
        biber
        collection-fontsextra
        collection-fontsrecommended
        collection-latexextra
        enumitem
        fontaxes
        mweights
        svg
        titlesec;
      };
    };
  };
}
