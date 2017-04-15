{
  allowUnfree = true;

  packageOverrides = super: let self = super.pkgs; in {

    haskell =
      let
        lib = super.haskell.lib;
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
        lib = super.haskell.lib;
      in
      super.haskellPackages.override {
        overrides = self: super: {
          process-extras = lib.dontCheck super.process-extras;
        };
      };

    ht = {
      scripts    = super.recurseIntoAttrs (super.callPackage ./pkgs/ht/scripts.nix {});
      shells     = super.recurseIntoAttrs (super.callPackage ./pkgs/ht/shells.nix  {});
      texliveEnv = super.texlive.combine {
        inherit (super.texlive)
        scheme-medium
        collection-fontsextra
        collection-fontsrecommended
        enumitem
        fontaxes
        mweights
        titlesec;
      };
    };

    nix = super.nix.overrideAttrs (oldAttrs: {
      meta = (oldAttrs.meta // { outputsToInstall = [ "out" "man" ]; });
    });

    weechat = super.callPackage ./pkgs/weechat-minimal.nix {};
  };
}
