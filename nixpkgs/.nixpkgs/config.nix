{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskell =
      let
        lib = self.haskell.lib;
      in
      super.haskell // {
        packages = super.haskell.packages // {
          ghc802 = super.haskell.packages.ghc802.override {
            overrides = self: super: {
              process-extras = lib.dontCheck super.process-extras;
            };
          };
        };
      };

    haskellPackages =
      let
        lib = self.haskell.lib;
        darwinStaticExe = p: if self.stdenv.isDarwin then lib.justStaticExecutables p else p;
      in
      super.haskellPackages.override {
        overrides = self: super: {
          cabal2nix      = darwinStaticExe super.cabal2nix;
          darcs          = darwinStaticExe super.darcs;
          stack          = darwinStaticExe super.stack;
          process-extras = lib.dontCheck super.process-extras;
        };
      };

    ht = {
      packages   = self.callPackage ./pkgs/ht/packages.nix {};
      scripts    = self.recurseIntoAttrs (self.callPackage ./pkgs/ht/scripts.nix {});
      shells     = self.recurseIntoAttrs (self.callPackage ./pkgs/ht/shells.nix  {});
      texliveEnv = self.texlive.combine {
        inherit (self.texlive)
        scheme-medium
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

    nix = super.nix.overrideAttrs (oldAttrs: {
      meta = (oldAttrs.meta // { outputsToInstall = [ "out" "man" ]; });
    });

    weechat = self.callPackage ./pkgs/weechat-minimal.nix {};
  };
}
