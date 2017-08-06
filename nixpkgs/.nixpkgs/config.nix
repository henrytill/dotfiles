{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskell =
      let
        lib = self.haskell.lib;
      in
      super.haskell // {
        packages = super.haskell.packages // {
          ghc7103_async202 = super.haskell.packages.ghc7103.override {
            overrides = self: super: {
              async = self.callHackage "async" "2.0.2" {};
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
          cabal2nix   = darwinStaticExe super.cabal2nix;
          darcs       = darwinStaticExe super.darcs;
          lhs2tex     = darwinStaticExe super.lhs2tex;
          stack       = darwinStaticExe super.stack;
          threadscope = darwinStaticExe super.threadscope;
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
